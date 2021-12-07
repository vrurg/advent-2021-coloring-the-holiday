use v6.e.PREVIEW;
unit class Runner::UI;
use Terminal::UI:ver<0.1.1+>:auth<cpan:bduggan>;
use Terminal::ANSI::OO;
use Terminal::ANSI;
use Terminal::Print::DecodedInput;
use AttrX::Mooish:ver<0.7.6+>:auth<zef:vrurg>;
use OO::Monitors;

class ExtendedANSI is Terminal::ANSI::OO {
    constant csi = Terminal::ANSI::CSI;
    method bold-faint-reset { csi ~ "22m" }
    method dim-reset { csi ~ "22m" }
    method italic-reset { csi ~ "23m" }
    method underline-reset { csi ~ "24m" }
    method strike-reset { csi ~ "29m" }
    method set-fg-default { csi ~ "39;m" }
}

constant ansi = ExtendedANSI.new(:get-codes);

# $*OUT wrapper class to maintain full control over cursor visibility and position
my class OUTWrapper {
    has $.out handles *;

    has $.force = False;
    has $.row;
    has $.col;

    submethod TWEAK {
        $!out = $*OUT;
        PROCESS::<$OUT> = self;
    }

    method print(|c) {
        if $!force {
            $!out.print: ansi.cursor-off,
                         |c,
                         ansi.cursor-on, ansi.move-to($!row, $!col);
        }
        else {
            $!out.print: |c
        }
    }

    method force-at($!row, $!col) {
        $!force = True;
    }

    method relax {
        $!force = False;
        $!out.print: ansi.cursor-off;
    }

    method unwrap {
        say "UNWRAP";
        $!out.print: ansi.cursor-on;
        PROCESS::<$OUT> = $!out;
    }
}

my class SearchRecord {
    has Bool:D $.is-regex = False;
    has Bool:D $.ignorecase = True;
    has Str:D $.search-str is required;
    has $.needle;

    submethod TWEAK {
        if $!is-regex {
            use MONKEY-SEE-NO-EVAL;
            $!needle = EVAL 'rx/' ~ ($!ignorecase ?? ':i ' !! '') ~ $!search-str ~ '/';
        }
        else {
            $!needle = $!ignorecase ?? $!search-str.fc !! $!search-str;
        }
    }

    multi method match(::?CLASS:D: Str:D $str) {
        ($!ignorecase ?? $str.fc !! $str).match($!needle, :g)
    }
}

has $.ui;
has $.screen;
has $.service-frame;
has $.helper-frame;
has $.service;
has $.service-status;
has $.service-state;
has $.helper;
has $.helper-status;
has $.helper-state;

has @.frames = <service helper>;
has $.focused-frame = 0;
has %.focused-panes = @!frames.map(* => 0);

# Wrapper for $*OUT
has $!out-wrapper = OUTWrapper.new;

# Count sequential Ctrl-C presses when modal frame is active
has $!ctrl-c-modal-count = 0;

has Promise $!shutdown .= new;

# String/char style types
# Bitmap of attributes.
my constant ST-NORMAL       = 0x0000;
my constant ST-BOLD         = 0x0001;
my constant ST-FAINT        = 0x0002;
my constant ST-DIM          = ST-FAINT;
my constant ST-ITALIC       = 0x0004;
my constant ST-UNDERLINE    = 0x0008;
my constant ST-SLOW-BLINK   = 0x0010;
my constant ST-RAPID-BLINK  = 0x0010;
my constant ST-REVERSE      = 0x0020;
my constant ST-CONCEALED    = 0x0040; # Unlikely to be supported
my constant ST-STRIKE       = 0x0080;
my constant ST-MASK         = 0xFFFF;
my constant ST-BOLD-FAINT   = ST-BOLD +| ST-FAINT;

# Bitmap of changed properties
my constant PROP-FG    = 0x010000;
my constant PROP-BG    = 0x020000;
my constant PROP-RESET = 0x040000;
my constant PROP-MASK  = 0xFF0000;
my constant FULL-MASK  = ST-MASK +| PROP-MASK;

#    my subset STColor of Any where Any | Int:D | Str:D | Positional:D;
my class Style {
    has $.fg;
    has $.bg;
    has Int:D $.attrs = ST-NORMAL;

    method diff-mask(::?CLASS:D $prev) {
        my $diff-attrs = $!attrs +^ $prev.attrs;
        # Due to the only way to reset both bold and faint, ...
        if ($diff-attrs +& ST-BOLD-FAINT)                       # ... if any of them is changed...
           && ($!attrs +& $diff-attrs +& ST-BOLD-FAINT) == 0    # ... and any is reset ...
        {
            $diff-attrs +|= ST-BOLD-FAINT;                      # ... then both are to be handled as a pair
        }
        (!($!fg eqv $prev.fg) ?? PROP-FG !! 0)
        +| (!($!bg eqv $prev.bg) ?? PROP-BG !! 0)
        +| $diff-attrs
    }

    method overly(::?CLASS:D $mod, Int:D $mask) {
        my $st-mask = $mask +& ST-MASK;
        self.new:
            fg => ($mask +& PROP-FG ?? $mod.fg !! $!fg),
            bg => ($mask +& PROP-BG ?? $mod.bg !! $!bg),
            attrs => ($!attrs +& (ST-MASK +^ $st-mask)) +| ($mod.attrs +& $st-mask)
    }

    method gist {
        "Style(fg:" ~ ($!fg // "<none>") ~ ", bg:" ~ ($!bg // "<none>") ~ ", 0x" ~ $!attrs.base(16) ~ ")"
    }
}

my role Terminalish {
    use Terminal::ANSIParser;
    use Concurrent::Queue;

    my sub csi2str(Str:D $csi) {
        $csi.trans([ "\x1b" ] => [ '<ESC>' ])
    }

    submethod TWEAK {
        self.frame.border<indicator> = "▸";
    }

    my monitor BufLine {
        my atomicint $nextid = 0;
        has $.id = $nextid⚛++;
        has @.style;
        has @.overly; # Would override values in @.csi if element is defined
        has @.chr;
        has $.pos = 0; # Where the next char will output
        has $.shift = 0;
        has Style:D $.default-style is required; # Default CSI
        has Style:D $.match-style = Style.new(:bg(5), :fg(0), :attrs(ST-ITALIC));
        has SearchRecord $.matcher;

        method chars { +@!chr }

        # Move current position back by $cols
        method rewind(UInt:D $cols --> Int:D) {
            fail "Rewinding by too many columns: $cols > $!pos" if $cols > $!pos;
            $!pos -= $cols;
        }

        method move(Int:D $cols --> Int:D) {
            my $chrs = +@!chr;
            $!pos = $_ < 0 ?? 0 !! ($_ > $chrs ?? $chrs !! $_) given $!pos + $cols
        }

        method clear {
            @!chr = ();
            @!style = ();
            @!overly = ();
            $!pos = 0;
        }

        method del(Int:D $pos, UInt:D :$count = 1) {
            return if $pos < 0 || $pos >= +@!chr;
            @!chr.splice($pos, $count);
            @!style.splice(($pos > +@!style ?? +@!style !! $pos), $count);
            @!overly = ();
        }

        method insert(UInt:D :$at = $!pos, |c) {
            temp $!pos = $at;
            my $tail-chrs := @!chr.splice($at, *);
            my $tail-style := $at > @!style ?? Empty !! @!style.splice($at, *);
            @!overly = ();
            my $p = self.put: |c;
            @!chr.append: $tail-chrs;
            @!style.append: $tail-style;
            $p
        }

        # Move to the line start in terms of console output, given that $width is number of columns in a line.
        method line-start(UInt:D $width --> Int:D) {
            $!pos -= $!pos % $width
        }

        proto method put(|) {*}
        multi method put(Style:D $style, Str:D $str) {
            my $chars = $str.chars;
            @!style.splice($!pos, $chars, $style xx $chars);
            @!chr.splice($!pos, $chars, $str.comb);
            $!pos += $chars;
        }
        multi method put(Str:D $str) {
            my $chars = $str.chars;
            @!chr.splice($!pos, $chars, $str.comb);
            $!pos += $chars;
        }
        multi method put(Pair:D (:key($style), :value($str))) { self.put: $style, $str }

        method chunks( Int:D $width,
                       Int:D :$shift = $!shift,
                       Style:D :$default-style = $!default-style,
                       SearchRecord :$matcher
            --> Seq:D)
        {
            return ([$default-style => ""],).Seq unless +@!chr > $shift;

            with ($matcher // $!matcher) {
                @!overly = Style xx @!chr.elems;
                my $res = .match: my $s = @.chr.join;
                if $res {
                    for $res.list -> $m {
                        my $chars = $m.chars;
                        @!overly.splice($m.from, $chars, $!match-style xx $chars);
                    }
                }
            }

            lazy gather {
                my $pos = $shift;
                my $chars = @!chr.elems;
                # One scan line of $width symbols as a list of csi => str pairs.
                my $line = [];
                my $style;
                my $str;
                while $pos < $chars {
                    my $cur-style = @!style[$pos] // $default-style;
                    $cur-style = $cur-style.overly($_, PROP-MASK +| ST-MASK) with @!overly[$pos];
                    my $pos-shift = $pos - $shift;
                    if $pos-shift > 0 && ( $pos-shift % $width ) == 0 {
                        $line.append: $style => "", $_<> with $str;
                        take $line;
                        $line = [];
                        $str = Nil;
                    }
                    if $style.defined && !$style.diff-mask($cur-style) {
                        $str ~= @!chr[$pos];
                    }
                    else {
                        $line.append: $style => "", $_<> with $str;
                        $style = $cur-style;
                        $str = @!chr[$pos];
                    }
                    ++$pos;
                }
                $line.push: $style => $str<>;
                take $line;
            }
        }
    }

    # Keep track of current color and decorations.
    my class CSIProcessor {
        use Terminal::ANSI;

        has Supplier:D $.log is built is required;
        has Bool:D $.debug is rw = False;
        has $.pane;

        # ASCII codes useful for direct analysis of CSI sequence Buf, received in Terminal::ANSIParse::CSI
        my constant FG-COLORS   = (30..38) | (90..97);
        my constant BG-COLORS   = (40..48) | (100..107);
        my constant ANY-COLORS  = (30..38) | (90..97) | (40..48) | (100..107);
        # If a color is an Int index then with this mask it must be sourced in the 256 color lookup table. For example,
        # 0x01 is red from the base colors, 0x101 is red from the lookup table and mapped into <ESC>[38;5;1m for fg.
        my constant COLOR-TBL-MASK  = 0x100;
        my constant NO-TBL-MASK     = 0x0FF; # Reverse the table mask
        my constant csi         = Terminal::ANSI::CSI;

        has Style $.style is mooish(:lazy, :clearer);

        has $!user-fg;
        has $!user-bg;
        has int $!user-attrs = ST-NORMAL;
        # Default for stdout
        has $.stdout-fg;
        has $.stdout-bg;
        has int $.stdout-attrs = ST-NORMAL;
        # Default for stderr
        has $.stderr-fg = 0x01; # Red
        has $.stderr-bg;
        has int $.stderr-attrs = ST-NORMAL;
        # from stdout or stderr
        has $!default-fg is built(:bind);
        has $!default-bg is built(:bind);
        has int $!default-attrs = ST-NORMAL;

        has $!is-stdout = True;

        multi method dbg(*@msg) { $!log.emit: @msg.map(*.gist).join("") if $!debug }

        method build-style {
            my $fg = $!user-fg // $!default-fg;
            my $bg = $!user-bg // $!default-bg;
            my $attrs = $!user-attrs +| $!default-attrs;
            Style.new: :$fg, :$bg, :$attrs
        }

        # Table maps of ANSI codes into attribute bits
        my @attr-set;
        my @attr-unset;
        BEGIN {
            @attr-set[1] = ST-BOLD;
            @attr-set[2] = ST-FAINT;
            @attr-set[3] = ST-ITALIC;
            @attr-set[4] = ST-UNDERLINE;
            @attr-set[9] = ST-STRIKE;

            @attr-unset[0] = ST-MASK;
            @attr-unset[22] = ST-BOLD + ST-FAINT;
            @attr-unset[23] = ST-ITALIC;
            @attr-unset[24] = ST-UNDERLINE;
            @attr-unset[29] = ST-STRIKE;
        }

        method add(Terminal::ANSIParser::CSI:D \csi) {
            my $csi-seq := csi.sequence;
            my $style-clear = True; # Assume style rebuild is needed

            my proto csic(Int:D) {*}
            multi csic(0) {
                # Full reset
                $!user-fg = $!user-bg = Nil;
                $!user-attrs = ST-NORMAL;
            }
            multi csic(39) {
                $!user-fg = Nil;
            }
            multi csic(49) {
                $!user-bg = Nil;
            }
            multi csic(Int $csi-code where @attr-set[$csi-code] | @attr-unset[$csi-code]) {
                if @attr-set[$csi-code] -> \attr-bits {
                    $!user-attrs +|= attr-bits +& ST-MASK;
                }
                if @attr-unset[$csi-code] -> \attr-bits {
                    $!user-attrs +&= ST-MASK +^ (attr-bits +& ST-MASK);
                }
            }
            multi csic($) {
                # If code is not known we ignore it and no style changes were done
                $style-clear = False;
            }

            # Collect all ASCII digits and make CSI code out of them; i.e. "<ESC>[49m" -> 49
            my $csi-code = 0;
            for 2..* {
                last if $_ > $csi-seq.elems || !(0x30 <= (my $ord = $csi-seq[$_]) <= 0x39);
                $csi-code = $csi-code * 10 + $ord - 0x30;
            }

            if $csi-code ~~ ANY-COLORS {
                # Color would be either an Int or an RGB triplet. See COLOR-TBL-MASK for Int indexes.
                my $color;
                my $is-fg = $csi-code ~~ FG-COLORS;
                my $base = $csi-code > 50 ?? 8 !! 0;
                if ($csi-code % 10) == 8 { # Color index or rgb.
                    my @color-codes = $csi-seq.decode.substr(7, * - 1).split(';').map(*.Int);
                    if $csi-seq[5] == 0x35 {
                        $color = @color-codes.head +| 0x100;
                    }
                    # We don't support Standard 8613-6 which is indicated by use of colon ':' in place of semi-colon
                    elsif $csi-seq[5] == 0x32 && $csi-seq[6].chr eq ';' {
                        $color = @color-codes;
                    }
                    else {
                        # Unsupported color CSI
                        $style-clear = False;
                    }
                }
                else {
                    # This will map csi code into 0-7 or 8-15 color index.
                    $color = $csi-code % 10 + $base;
                }
                ($is-fg ?? $!user-fg !! $!user-bg) = $color;
            }
            else {
                csic($csi-code);
            }

            self.clear-style if $style-clear;
        }

        multi method set-stdout(Bool:D $stdout = True) {
            return unless $stdout ^^ $!is-stdout;
            $!is-stdout = $stdout;
            if $stdout {
                $!default-fg := $!stdout-fg;
                $!default-bg := $!stdout-bg;
                $!default-attrs = $!stdout-attrs;
            }
            else {
                $!default-fg := $!stderr-fg;
                $!default-bg := $!stderr-bg;
                $!default-attrs = $!stderr-attrs;
            }
            self.clear-style;
        }

        proto method color2csi($) {*}
        multi method color2csi(Int:D $color, :$bg) {
            my $bg-shift := $bg ?? 10 !! 0;
            csi ~ ($color < 0x100
                ?? $bg-shift + ($color < 8 ?? 30 !! 82) + $color # map into 30, 40, 90, and 100 ranges
                !! ($bg-shift + 38) ~ ";5;" ~ ($color +& NO-TBL-MASK))
            ~ "m"
        }
        multi method color2csi(@color, :$bg) {
            csi ~ ($bg ?? "48" !! "38") ~ ";2;" ~ @color.join(";") ~ "m"
        }
        multi method color2csi(Mu:U, :$bg) { csi ~ ($bg ?? "49" !! "39" ) ~ "m" }

        # For proper setting of attributes whenever bold or faint/dim are reset the mask must have both ST-BOLD and
        # ST-FAINT set and the $attrs must have both bits set to their desired values. This is due to no way to reset
        # one of them without the other.
        my @st-bitmap =
            Pair.new( (ST-BOLD + ST-FAINT), (ansi.bold-faint-reset, "") ),
            Pair.new( ST-ITALIC,            (ansi.italic-reset,     ansi.italic) ),
            Pair.new( ST-BOLD,              ("",                    ansi.bold) ),
            Pair.new( ST-FAINT,             ("",                    ansi.faint) ),
            Pair.new( ST-UNDERLINE,         (ansi.underline-reset,  ansi.underline) ),
            Pair.new( ST-STRIKE,            (ansi.strike-reset,     ansi.strike) ),
            ;
        method attr2csi(Int:D $attrs, Int:D $mask = ST-MASK) {
            my $csi = "";
            for @st-bitmap -> (:key($bits), :value(@csis)) {
                if ($mask +& $bits) == $bits {
                    $csi ~= $attrs +& $bits ?? @csis[1] !! @csis[0];
                }
            }
            $csi
        }

        proto method style2csi(|) {*}
        multi method style2csi(Style:D $style) {
            self.color2csi(.fg) ~ self.color2csi(.bg, :bg) ~ self.attr2csi(.attrs) given $style
        }
        multi method style2csi(Style:D $prev, Style:D $next) {
            my $mask = $next.diff-mask($prev);
            return "" unless $mask;
            ($mask +& PROP-FG ?? self.color2csi($next.fg) !! "")
            ~ ($mask +& PROP-BG ?? self.color2csi($next.bg, :bg) !! "")
            ~ self.attr2csi($next.attrs, $mask +& ST-MASK)
        }
    }

    has Supplier $.log .= new;
    has Bool:D $.debug is rw = False;

    # List of original raw buffers
    has BufLine:D @.orig;
    has $!autoscroll = True;
    has SearchRecord $.matcher;

    has CSIProcessor $.csi-proc is mooish(:lazy);
    has &!parse-byte is mooish( :lazy );

    # print-seq support
    has Channel $!seq-queue .= new;
    has atomicint $!seq-busy = 0;

    method build-csi-proc {
        CSIProcessor.new(:pane(self), :$!log, :$!debug)
    }

    method !build-parse-byte {
        make-ansi-parser( emit-item => {
            self.add-code( $_ )
        } )
    }

    method !print-seq( \msg, Bool :$stderr = False ) {
        $!seq-queue.send: $stderr => msg;
        if cas($!seq-busy, 0, 1) == 0 {
            self!new-orig-line unless @!orig;
            start {
                CATCH {
                    default {
                        note "===PRINT SEQ=== [{.^name}] " ~ .message ~ "\n" ~ .backtrace;
                        $!log.emit: "===PRINT SEQ=== [{.^name}] " ~ .message ~ "\n" ~ .backtrace
                    }
                }
                loop {
                    # Pull out all available chunks. The cas above will ensure no additional one has been pushed onto
                    # the queue after we're over with the previous batch.
                    my $chunk = $!seq-queue.receive;
                    while $chunk {
                        my $is-stderr = $chunk.key;
                        my \seq = $chunk.value;
                        $!csi-proc.set-stdout(!$is-stderr);
                        for seq {
                            &!parse-byte( $_ );
                        }
                        $chunk = $!seq-queue.poll;
                    }
                    self!flush;
                }
            }
        }
        True
    }

    method dbg(*@msg) {
        $!log.emit: @msg.map(*.gist).join("") if $!debug;
    }

    multi method print( Buf $msg, *%c ) {
        self!print-seq: $msg.Seq, |%c;
    }

    multi method print( *@msg, *%c ) {
        self!print-seq: @msg.map( *.gist ).join( "" ).comb.map( *.ord ), |%c;
    }

    method say( *@msg, *%c ) {
        self.print: |@msg, "\n", |%c
    }

    proto method autoscroll($?) {*}
    multi method autoscroll() { $!autoscroll }
    multi method autoscroll($on) { $!autoscroll = ? $on }

    method maybe-autoscroll {
        $!autoscroll = ? ($_ == self.lines.end with self.current-line-index);
    }

    method set-matcher($matcher) {
        $!matcher = $matcher;
        self.refresh;
    }

    method select(|) {
        callsame;
        self.maybe-autoscroll;
    }

    my constant DEFAULT-STYLE = Style.new;
    method !csi-line(@line --> Positional) {
        my $prev-style = DEFAULT-STYLE;
        @line.map({
            if $_ ~~ Pair {
                my $csi = $!csi-proc.style2csi($prev-style, .key);
#                note   "STYLE PREV: ", $prev-style,
#                     "\n      NEXT: ", .key,
#                     "\n      DIFF: ", .key.diff-mask($prev-style).fmt('%08x'),
#                     "\n       CSI: ", csi2str($csi),
#                     "\n      CHNK: ", .value.raku;
                $prev-style = .key;
                $csi => .value
            }
            else {
#                note .raku;
                $_
            }
        }).List
    }

    method !flush( BufLine:D $orig = @!orig.tail ) {
        my $orig-id = $orig.id;
        my $line-idx = @.meta.first({ .<orig-id> == $orig-id }, :k);
        for $orig.chunks(self.width, :$!matcher) -> $line {
            my $csi-line := self!csi-line($line);
            with $line-idx {
                self.update($csi-line, :line($line-idx), meta => { :$orig-id });
                ++$line-idx;
            }
            else {
                self.put: $csi-line, meta => { :$orig-id };
            }
        }
        self.select-last if $!autoscroll;
    }

    method buf-line(|c) { BufLine.new: |c }

    method !new-orig-line(Style $style?, *%c) {
        @!orig.push: self.buf-line( default-style => $style // $!csi-proc.style,
                                    :$!matcher,
                                    |%c );
    }

    proto method add-code( | ) {*}
    multi method add-code( Int $code ) {
        my $chr := $code.chr;
        # More complex logic for \n, \r might be needed. But this works so far.
        my $nl = $chr eq "\n";
        my $cr = $chr eq "\r";
        my $flush = $nl || $cr;
        my $orig-last = @!orig.tail;

        # On \r shift current position back to the start of the currently being outputted line. Simulates iTerm
        # behaviour on too long \r lines
        $orig-last.line-start(self.width) if $cr;
        self!new-orig-line if $nl;

        if $flush {
            self!flush( $orig-last );
        }
        else {
            $orig-last.put($!csi-proc.style, $chr);
        }
    }

    multi method add-code( Terminal::ANSIParser::CSI $csi ) {
        my $csi-seq := $csi.sequence;
        if $csi-seq[2] == 0x32 && $csi-seq[3] == 74 { # "J", clear sequence
            # We don't actually clear the screen because the information wiped could be requested later
            self.say: "\n", ansi.bg-bright-magenta, ansi.black, "<<---clear--->>", ansi.normal-video;
            self!flush;
        }
        else {
            $!csi-proc.add: $csi;
        }
    }

    # Just ignore any other kind of code
    multi method add-code( $ ) {}

    method refresh(@orig = @!orig) {
        my $save-orig-id = self.current-meta<orig-id>;
        my $selected-line;
        self.clear;
        my $width = self.width;
        for @orig -> $orig {
            my $orig-id = $orig.id;
            for $orig.chunks( $width, :$!matcher ) -> $line {
                self.put( self!csi-line($line), meta => { :$orig-id } );
                $selected-line //= @.lines.end if $orig-id == $save-orig-id;
            }
        }
        self.select($_) with $selected-line;
    }
}

role ProcHandler does Terminalish {
    my class ProcComms {
        has Supplier:D $.infos .= new;
        has Supplier:D $.errors .= new;
        has Promise $.completed;
        has Promise $.proc .= new;
        method start( &code ) {
            $!completed //= Promise.start: &code
        }
        method info( *@msg ) {
            $!infos.emit: @msg.map( *.gist ).join( "" );
        }
        method error( *@msg ) {
            $!errors.emit: @msg.map( *.gist ).join( "" );
        }
    }

    method start-proc(*@cmd, Supply:D :$commands!, Bool :$verbose = False, :$cwd = $*CWD) {
        my $comm = ProcComms.new;
        my $proc-vow = $comm.proc.vow;
        $comm.start: {
            if $verbose {
                self.say: ansi.yellow ~ '$ ' ~ @cmd.join(" ");
            }
            temp %*ENV<COLUMNS> = self.width;
            temp %*ENV<LINES> = self.height;
            my $proc = Proc::Async.new: :w, @cmd;
            react {
                whenever $proc.stdout {
                    self.print: $_
                }
                whenever $proc.stderr {
                    self.print: $_, :stderr
                }
                whenever $proc.ready {
                    $comm.info: "=== Started with PID ", $_;
                    $proc.close-stdin;
                }
                whenever $proc.start(:$cwd) {
                    $comm.info: ansi.yellow, "=== Exited with rc=", .exitcode, ", signal=", .signal;
                    $proc-vow.keep($_);
                    done;
                }
                whenever $commands {
                    when "stop-process" {
                        $proc.kill;
                        whenever Promise.in(15) {
                            $comm.error: "=== Server ignores SIGHUP, trying SIGINT";
                            $proc.kill(SIGINT);
                            whenever Promise.in(15) {
                                $comm.error: "=== Server ignores SIGINT, trying SIGKILL";
                                $proc.kill(SIGKILL);
                                whenever Promise.in(5) {
                                    $comm.error: "=== Server ignores everything. I wash my hands...";
                                    done;
                                }
                            }
                        }
                    }
                }
            }
        }
        $comm
    }
}

role Modalish {
    has $.is-modal = False;

    has $!orig-focus;

    method on-key($) {...}
    method display-off() {...}

    method modal(Bool:D $modal) {
        return unless $!is-modal ^^ $modal;
        my $ui = $*RUNNER-UI.ui;
        if $modal {
            $!orig-focus = ($ui.focused-frame, $ui.focused);
            $!orig-focus[1].unfocus;
            $ui.focus($.frame, :pane(self));
        }
        else {
            self.display-off;
            self.unfocus;
            $ui.focus($!orig-focus[0], pane => $!orig-focus[1]);
        }
        $!is-modal = $modal;
    }

}

role SearchInput does Modalish does Terminalish {
    has $.search-str = "";
    has @.history;
    has $!history-pos;
    has $.prompt = "Search: ";
    has $!row = 1;
    has $!line;
    has $!is-on = False;
    has $!ui = $*RUNNER-UI;
    has $!shift;
    has $!shift-delta = 4;
    has $!prev-state;
    has $!is-regex = False;
    has $!ignorecase = True;
    has $!first-in = False;

    my constant INPUT-STYLE = Style.new(:fg(15), :bg(0), :attrs(ST-UNDERLINE));
    my constant INIT-STYLE = Style.new(:fg(15), :bg(4), :attrs(ST-UNDERLINE));

    submethod TWEAK {
        self.selectable = False;
    }

    method !input-width { $.width - $!prompt.chars }

    method !refresh {
        return unless $!is-on;
        self.update: [ ansi.bright-white => $!prompt,
                       |self!csi-line(
                           $!line.chunks( self!input-width,
                                          :$!shift,
                                          :default-style($!first-in ?? INIT-STYLE !! INPUT-STYLE) ).head ),
                     ],
                     :line($!row);
        self.redraw;
    }

    method !status {
        self.update: [ ansi.white => "F2: ",
                       ansi.green => ($!is-regex ?? " rx" !! "str"),
                       ansi.white => ", ",
                       ansi.white => "F3: ",
                       ansi.green => ($!ignorecase ?? "nocase" !! "  case")
                     ],
                     :line(0)
    }

    method state($st) {
        if $!is-on {
            $!prev-state = $st;
        }
        else {
            self.update: $st, :line(0);
        }
    }

    method move-cursor($cols) {
        my $pos = $!line.move($cols);
        my $iwidth = self!input-width;
        my $chars = $!line.chars;
        if $pos == $chars {
            --$iwidth;
            $!shift = $chars > $iwidth ?? $chars - $iwidth !! 0;
        }
        elsif $pos < $!shift || $pos >= ($!shift + $iwidth) {
            my $delta = $pos < $!shift ?? ($!shift - $pos) !! ($pos - $!shift - $iwidth);
            $delta = ($delta / $!shift-delta).ceiling * $!shift-delta;
            my $max-shift = $chars - $iwidth + 1;
            $!shift = $_ < 0 ?? 0 !! ($_ > $max-shift ?? $max-shift !! $_) given $!shift + $delta * $cols.sign;
        }
        self!refresh;
    }

    method on-key($key) {
        return unless $.is-modal;
        my $refresh = my $first-in = $!first-in;
        $!first-in = False;
        given $key {
            when SpecialKey {
                when CursorLeft {
                    self.move-cursor(-1);
                }
                when CursorRight {
                    self.move-cursor(1);
                }
                when Backspace {
                    my $cur-pos = $!line.pos;
                    if $cur-pos > 0 {
                        my $count = $first-in ?? $cur-pos !! 1;
                        $!line.del($cur-pos - $count, :$count);
                        self.move-cursor(-$count);
                    }
                }
                when Delete {
                    my $count = $first-in ?? ($!line.chars - $!line.pos) !! 1;
                    $!line.del($!line.pos, :$count);
                    $refresh = True;
                }
                when CursorHome {
                    self.move-cursor(-$!line.pos);
                    $refresh = True;
                }
                when CursorEnd {
                    self.move-cursor($!line.chars - $!line.pos);
                    $refresh = True;
                }
                when CursorUp {
                    if $!history-pos > 0 {
                        --$!history-pos;
                        self!history-changed;
                    }
                }
                when CursorDown {
                    if $!history-pos < @!history.end {
                        ++$!history-pos;
                        self!history-changed;
                    }
                }
                when F2 {
                    $!is-regex = !$!is-regex;
                    $!first-in = $first-in; # Don't change "key-pressed" status
                    self!status;
                }
                when F3 {
                    $!ignorecase = !$!ignorecase;
                    $!first-in = $first-in; # Don't change "key-pressed" status
                    self!status;
                }
            }
            when "\x0d" {
                $!search-str = $!line.chr.join;
                my $matcher = SearchRecord.new(:$!is-regex, :$!ignorecase, :$!search-str);
                @!history.push: $matcher;
                $.frame.panes[0].set-matcher: $matcher;
                self.modal(False);
            }
            when "\e" {
                self.modal(False);
            }
            default {
                my $uniprop = .uniprop;
                if ($uniprop ne "Cc" && !$uniprop.starts-with("Z")) || $_ eq " " {
                    $!line.clear if $first-in;
                    $!line.insert($_);
                    self.move-cursor(1);
                    $refresh = True;
                }
            }
        }
        self!refresh if $refresh;
    }

    method redraw(|) {
        if $!is-on {
            $!ui.cursor-at( $.top + $!row,
                            $.left + $!prompt.chars + $!line.pos - $!shift);
        }
        nextsame
    }

    method display-off {
        return unless $!is-on;
        self.update: "", :line($!row);
        self.update: $!prev-state, :line(0);
        $!prev-state = Nil;
        $!ui.cursor-hide;
        $!is-on = False;
        self.redraw;
    }

    method !history-changed {
        $!first-in = True;
        self!from-history;
        self!refresh;
        self!status;
    }

    method !from-history {
        return if $!history-pos < 0;
        with @!history[$!history-pos] {
            $!search-str = .search-str;
            $!ignorecase = .ignorecase;
            $!is-regex = .is-regex;
            my $schars = $!search-str.chars;
            my $iwidth = self!input-width - 1;
            $!shift = $schars > $iwidth ?? $schars - $iwidth !! 0;
            $!line.clear;
            $!line.put: $!search-str;
        }
    }

    method start-search {
        $!first-in = True;
        $!history-pos = @!history.end;
        if $!history-pos < 1 {
            $!shift = 0;
            $!line = self.buf-line(default-style => INPUT-STYLE);
        }
        else {
            self!from-history;
        }
        $!prev-state = $.raw[0];
        self!status;
        $!is-on = True;
        self!refresh;
        self.modal(True);
    }
}

submethod TWEAK {
    PROCESS::<$RUNNER-UI> = self;
    $!ui = Terminal::UI.new;
    $!screen = $!ui.add-screen;

    $!service-frame = $!screen.add-frame: :width(($!screen.cols / 2).ceiling),
                                          :name<service>;
    ($!service, $!service-status, $!service-state) = $!service-frame.add-panes(heights => [fr => 1, 10, 2]);
    $!service does ProcHandler;
    $!service-status does Terminalish;
    $!service-state does SearchInput;

    $!helper-frame = $!screen.add-frame: :left($!service-frame.right + 1),
                                           :width($!screen.cols - $!service-frame.width),
                                           :name<helper>;
    ($!helper, $!helper-status, $!helper-state) = $!helper-frame.add-panes(heights => [fr => 1, 10, 2]);
    $!helper does ProcHandler;
    $!helper-status does Terminalish;
    $!helper-state does SearchInput;

    $!ui.focus(:frame<service>, :pane(0));

    $!ui.draw;
}

method events {
    $!service-frame.draw;
    $!helper-frame.draw;

    my sub focused-pane-idx(Bool :$next = False) {
        my $frame-name := @!frames[$!focused-frame];
        return %!focused-panes{$frame-name} unless $next;
        my $pane-count := $.ui.screen.find-frame($frame-name).panes.pairs.grep({ .value ~~ Terminalish }).elems;
        %!focused-panes{$frame-name} = ++%!focused-panes{$frame-name} % $pane-count
    }

    my sub refocus {
        $.ui.focused.unfocus;
        $.ui.focus(:frame(@!frames[$!focused-frame]), pane => focused-pane-idx);
        $.ui.draw;
    }

    supply {
        whenever decoded-input-supply() {
            CATCH {
                default {
                    note .message ~ "\n" ~ .backtrace;
                    emit $_
                }
            }
            my $focused-pane = $.ui.focused;
            if $focused-pane ~~ Modalish && $focused-pane.is-modal {
                if $_ eq "\x3" {
                    $!ctrl-c-modal-count++;
                    $.ui.screen.remove-frame: $.ui.focused-frame if $!ctrl-c-modal-count == 1;
                    emit "quit" if $!ctrl-c-modal-count > 1;
                    if $!ctrl-c-modal-count > 2 {
                        die "Force quit";
                        exit 1;
                    }
                }
                $focused-pane.on-key($_);
            }
            else {
                $!ctrl-c-modal-count = 0;
                when "k" | "K" {
                    $.ui.focused.scroll-down;
                    $.ui.focused.select-up;
                }
                when "j" | "J" {
                    $.ui.focused.scroll-up;
                    $.ui.focused.select-down;
                }
                when "q" | "Q" {
                    emit "shutdown"
                }
                when "b" | "B" {
                    emit "rebuild"
                }
                when "r" | "R" {
                    emit "restart"
                }
                when "c" {
                    $.ui.focused.clear;
                    $.ui.focused.print: "";
                }
                when " " {
                    $.ui.focused.say: ansi.magenta, ">>> CUT <<<";
                }
                when "\t" {
                    my $frame = $.ui.focused-frame;
                    $.ui.focused.unfocus;
                    $.ui.focus( $frame, pane => focused-pane-idx( :next ) );
                    $.ui.draw;
                }
                when "\x3" {
                    emit "quit";
                }
                when "\x12" {
                    given $.ui.focused {
                        .?refresh;
                        .focus;
                    }
                }
                when "\e" {
                    $.ui.draw;
                }
                when "/" {
                    $.ui.focused-frame.panes[2].start-search;
                    $.ui.focused.unfocus;
                    $.ui.focus($.ui.focused-frame, :pane(2));
                }
                when CursorDown {
                    $.ui.focused.select-down;
                }
                when CursorUp {
                    $.ui.focused.select-up;
                }
                when CursorLeft {
                    $!focused-frame = 0;
                    refocus;
                }
                when CursorRight {
                    $!focused-frame = 1;
                    refocus;
                }
                when CursorEnd {
                    $.ui.focused.select( $.ui.focused.lines.elems - 1 );
                    $.ui.focused.autoscroll( True );
                }
                when CursorHome {
                    $.ui.focused.select( 0 );
                    $.ui.focused.autoscroll( False );
                }
                when PageUp {
                    $.ui.focused.page-up;
                }
                when PageDown {
                    $.ui.focused.page-down;
                }
                default {
                    emit "key" => $_;
                }
            }
        }
        whenever $!shutdown {
            done;
        }
    }
}

method shutdown {
    $!shutdown.keep if $!shutdown.status == Planned;
}

method finish {
    return unless $!shutdown.status == Planned;
    self.shutdown;
    $!out-wrapper.unwrap;
    $!ui.shutdown;
}

method cursor-at($row, $col) {
    $!out-wrapper.force-at($row, $col)
}

method cursor-hide { $!out-wrapper.relax }
