use v6.e.PREVIEW;
unit class Runner;

use AttrX::Mooish;
use Runner::UI;
use Terminal::ANSI::OO 'ansi';

my class ProcData {
    has Int $.last-rc;
    has Int $.last-signal;
    has Instant $.last-exited;
    has Int $.restart-count = 0;
    has Promise $.completed is rw;
    has Supplier:D $.commands .= new;
    has $.main is required;
    has $.status is required;
    has $.state is required;

    method update-from-exit(Proc $p) {
        $!last-rc = $p.exitcode;
        $!last-signal = $p.signal;
        $!last-exited = now;
        if $!last-rc || $!last-signal {
            ++$!restart-count;
        }
        else {
            $!restart-count = 0;
        }
    }

    method reset-restarts {
        $!restart-count = 0;
    }

    method Str {
        my $csi = $!last-rc || $!last-signal ?? ansi.red !! ansi.bright-green;
        $csi
        ~ ( $!last-rc.defined
            ?? ( "rc: " ~ $!last-rc ~ ", "
                 ~ "signal: " ~ $!last-signal ~ ", "
                 ~ "restarts: " ~ $!restart-count)
            !! "no stats yet")
    }

    method exec(@cmd, :$cwd) {
        $.main.start-proc: @cmd, :$cwd, :$.commands
    }

    method stop(:$sync) {
        if $.completed.defined && $.completed.status == Planned {
            $.commands.emit: "stop-process";
            if $sync {
                await $.completed;
            }
        }
    }
}


has IO::Path $.base-dir = $?FILE.IO.parent(2);
has IO::Path $.foo-dir = $!base-dir.add("services", "foo");
has @.server-cmd = $*EXECUTABLE, '-I' ~ $!foo-dir, $!foo-dir.add("service.p6");
has @.helper-cmd = <ls -lA --color=always services/foo>;

has Promise:D $!shutdown .= new;
has ProcData $!server-proc;
has ProcData $!helper-proc;

has $.ui = Runner::UI.new;

method run {
    $!server-proc = ProcData.new: main => $.ui.service, 
                                  status => $.ui.service-status,
                                  state => $.ui.service-state;
    $!helper-proc = ProcData.new: main => $.ui.helper, 
                                  status => $.ui.helper-status,
                                  state => $.ui.helper-state;
    %*ENV<FOO_HOST> = '*';
    %*ENV<FOO_PORT> = 12345;
    self.start-cmd: $!server-proc, @.server-cmd, :auto-restart;
    self.start-cmd: $!helper-proc, @.helper-cmd;
    my $ex;
    react {
        whenever $.ui.events {
            CATCH {
                default { $ex = $_; done }
            }
            when "shutdown" {
                self.stop-all;
                $.ui.service-status.say: "Runner is shutting down";
                done;
            }
            when "quit" {
                self.stop-all;
                done;
            }
            when "restart" {
                if $.ui.focused-frame == 0 {
                    $!server-proc.stop;
                }
                else {
                    $!helper-proc.stop(:sync);
                    self.start-cmd: $!helper-proc, @.helper-cmd;
                }
            }
            when Exception {
                $ex = $_;
                self.stop-all;
                done;
            }
            LAST { done; }
            QUIT { $ex = $_; done; }
            CLOSE { done; }
        }
        #whenever self.helper-needs-run {
        #    $.ui.helper-status.say: "Helper re-run";
        #    self.helper-re-run;
        #}
        whenever signal(SIGINT).merge: signal(SIGHUP) {
            $.ui.service-status.put: "Runner is shutting down";
            self.finish;
            done;
        }
    }
    self.finish;
    note $ex.message ~ "\n" ~ $ex.backtrace.Str.indent(4) if $ex;
    self
}

method start-cmd(ProcData:D $proc, @cmd, :$auto-restart = False) {
    return if self.is-down;
    if $proc.restart-count > 2 {
        my $delay = 15;
        react {
            # $service-source-monitor is supposed to monitor 
            #whenever $service-source-monitor.Supply {
            #    $.ui.status.say: "Service sources ({.events.elems} changes) updated";
            #    done;
            #}
            whenever $!shutdown { done }
            whenever Supply.interval(1) {
                if $delay {
                    $proc.state.state: $proc ~ "; restart in {$delay--}";
                }
                else {
                    done
                }
            }
        }
    }
    $proc.state.state: ~$proc;
    my $pcomm = $proc.exec: @cmd, :cwd($.base-dir);
    $proc.completed = start {
        CATCH {
            default {
                note .message ~ "\n" ~ .backtrace;
                $proc.main.say: "[" ~ .^name ~ "] " ~ .message ~ "\n" ~ .backtrace, :stderr;
            }
        }

        react {
            whenever $pcomm.completed {
                unless self.is-down {
                    if $auto-restart {
                        $proc.status.put: "Try restarting command";
                        self.start-cmd($proc, @cmd, :auto-restart) 
                    }
                }
                done;
            }
            whenever $pcomm.proc {
                $proc.update-from-exit( $_ );
                $proc.state.state: ~$proc;
            }
            whenever $pcomm.infos {
                $proc.status.say: ansi.white, $_;
            }
            whenever $pcomm.errors {
                $proc.status.say: ansi.red, $_;
            }
            whenever $proc.main.log {
                $proc.status.say: "[$_]";
            }
        }
    }
}

method stop-all {
    $!server-proc.stop;
    $!helper-proc.stop;
}

method is-down {
    $!shutdown.status != Planned;
}

method finish {
    return if self.is-down;
    $!shutdown.keep;
    $.ui.service-status.say: ansi.red, "... Stopping everything";
    self.stop-all;
    my @completions;
    for $!server-proc.completed, $!helper-proc.completed -> $compl {
        @completions.push: $_ with $compl;
    }
    await Promise.anyof(
        Promise.in(30),
        Promise.allof(@completions)
    );
    $.ui.finish;
    say ansi.clear-screen;
}
