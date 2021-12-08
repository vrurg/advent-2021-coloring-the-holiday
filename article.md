# Coloring your ~~tools~~ holidays

```
Jingle bells, jingle bells,
Santa's busy guy,
Don't we bother him with what
We can make ourselves...
```

And if you're in doubt yet wether Santa was really overloaded last year, just check out [the advent calendar of 2020](https://raku-advent.blog/category/2020/). That's why back then I fetched out an old, dusty reddish-white cap, pulled it over my ears and started a small home-brew project to help my wife in her job.

To be fully honest here, it was a gift to myself too since for some time I planned to learn more about front-end programming. A good chance to look at Vue and TypeScript, why not to take it? There is [`Cro`](https://cro.services/),  [`Cro::RPC::JSON`](https://github.com/vrurg/raku-Cro-RPC-JSON) for APIs, [`Red`](https://github.com/FCO/Red) for databases. Of course, there is [Raku](https://raku.org/) to bind them all... Oh, pardon me, it's a different epic story to be told when time comes!

This article (is it really a post? ah, whatever...) started with something, any one doing backend development knows well about: the need to monitor the server script, restart it upon failures, or when sources changes, etc., etc. Aside of that, I also wanted to keep my eye on rebuilds of the frontend code. And since I didn't like keeping both tasks in two different shell sessions, I came up with a script `runner.raku` which was controlling them and juggling processes the way I needed.

As always, there is a "but". This time I quickly realized that often simultaneous changes of both server and frontend sources results in a chaotic mixture of outputs, hard to read and to locate errors. Having some experience in creating a prototype of a [text UI framework](https://github.com/vrurg/raku-Vikna), I soon started considering something for the runner script to separate and manage outputs of each task. Unfortunately, Vikna was not nor yet ready for real use; and however much I'd like to complete the project, I just don't have enough time for it. So I gave up...

... Until [`Terminal::UI` by Brian Duggan](https://raku.land/cpan:BDUGGAN/Terminal::UI) was released. "Oh!" – I said to myself. And... Well, nothing. Because then I started thinking: since `npm build` outputs in colors and it is really easy to spot any useful bits of information this way, I'd like to preserve these colors. But this would require parsing the input, picking any control sequences from it, analyzing them, translating into... Oh, my, thanks! After all, not everything is that bad about the old plain flat stream of sometimes mixed up output. Not that often it happens, isn't worth the troubles...

But once there was a day when I thought: perhaps there is a module for parsing ANSI sequences? And thrown I a dragnet into the waters of [Raku Land](https://raku.land/), and came it back empty, and made I a helpless gesture... Only to see in two days an announcement of Geoffrey Broadwell releasing his [`Terminal::ANSIParser`](https://raku.land/zef:japhb/Terminal::ANSIParser)! It was the sign. After all, having something for UI, something for parsing the input, and an already working process orchestrator – how long would it take to forge them into _something_? Yes, my naïvety again and again. Once started, I wanted to get from it:

- Split coloring for stdout/stderr
- Split process output from related runner script status change messages
- A specialized current state indication row (state bar)
- `print`/`say` methods for outputting different kinds of messages, because `Terminal::UI` only has `put` which doesn't even wrap lines. Basically, it's what the module is primarily designed for: output lines and navigate them.
- Search for strings or regexes with color marking of matches found
- Input line with history for the above feature

And I got it. All. Not that it took me a couple of days, but here is what the result looks like:

![Demo Screen](https://raw.githubusercontent.com/vrurg/advent-2021-coloring-the-holiday/main/img/ScreenRecording.gif)

(the link to image in case it's not possible to insert it inline: https://raw.githubusercontent.com/vrurg/advent-2021-coloring-the-holiday/main/img/ScreenRecording.gif)

Only later as it came to me: look, it's a simple terminal emulator! Without keyboard passing into subprocesses, though. But with ANSI sequence parsing, and translating, and showing the result back to user.

It would be too much to discuss all the aspects of the resulting code. For adventurous ones [the source is available](https://github.com/vrurg/advent-2021-coloring-the-holiday). It consist of an [UI module](https://github.com/vrurg/advent-2021-coloring-the-holiday/blob/main/lib/Runner/UI.rakumod), which is responsible for all the in/out interactions; and of a [runner module](https://github.com/vrurg/advent-2021-coloring-the-holiday/blob/main/lib/Runner.rakumod), doing the management work. The latter primarily consists of the process orchestrator, and barely of much interest here. So, let's focus on the UI.

The core of it is `Terminalish` role which is responsible for gathering text streams, seasoned with ANSI ESC-sequences, from different sources, buffering their intermediate representation, possibly doing some processing work like applying search requests, and sending the result to some kind of output.

For example, to colorize your own message the following is expected to be done:

```
use Terminal::ANSI::OO 'ansi';
...
$terminalish-pane.say: ansi.blue, "This is blue", ansi.text-reset, " this is in default colors";
```

`$terminalish-pane` is a pane object of `Terminal::UI` with `Terminalish` role mixed in. Remove it and leave the `say` alone and it will work as expected on the terminal emulator of your choice.

Even though I made it around `Terminal::UI`, with limited amount of changes the role can be adapted to any other kind of UI library/framework since output is just a side effect of its primary purpose.

Internally the role is using the following components:

- `CSIProcessor` is responsible for filtering input, passed through `Terminal::ANSIParser`, and translating it into the internal representation implemented by `BufLine`. This is the component which knows the ANSI sequences meaning.
- `BufLine`, which holds a single line of scroll buffer with all the attributes necessary to display it correctly. It also responsible for transforming the line into output-ready form, providing support for line wrapping, highlighting search results, and colorizing stdout/stderr if requested.

The `Terminalish` role itself manages input, scroll buffer, and interacts with user code. It would better off to have it as a class, inheriting from `Terminal::UI::Pane`, but `Terminal::UI` doesn't support inheritance of its components.

One of the biggest lessons learned, while implementing the role, was not to operate the `ESC`-sequences directly unless there are no plans to manipulate them in either way. Even a simplest manipulation in plain sight may bring troubles, except for bare removal.

Or it would be fine if the plan _is_ to produce highly ineffective stream of output symbols, bloated with rubbish `ESC`-sequences...

Way more effective approach is to keep all the style attributes as bit-masks; and all the colors in whichever form you like, but one color per foreground/background, per symbol. Eventually, I introduced [a `Style` class](https://github.com/vrurg/advent-2021-coloring-the-holiday/blob/3e3dcc75fe2c487f704cc8ed4e8cbc928353bf3e/lib/Runner/UI.rakumod#L62), which is a commonplace solution, but it works. Instances of the class are attached to each individual symbol.

A big advantage of `Style` is it's ability to produce a difference of its two instances. The difference, in turn, is a highly effective approach to determine _when_ an ANSI `ESC` sequence is to be inserted into the output. And even more importantly, _what_ this sequence must consist of! Because only changes will make it through. For example, if we have a string "ab", and "a" styled with green on blue + italic, whereas "b" is yellow on blue + bold + italic then the output would be made of:

```
ansi.green ~ ansi.bg-blue ~ ansi.italic ~ "a"
~ ansi.yellow ~ ansi.bold ~ "b"
```

I was rather surprised to see how effective the approach is when the firsts test runs resulted in rather decent performance, even though no real code optimization  were done. Actually, since the whole thing was never planned for publication, no optimization has been done yet and not even planned to.

Also, as I mentioned already, the approach works extremely well when one needs to mark some part of the text with special colors of attributes. It is sufficient just to apply them to the correct symbols – and the rest will happen automatically! Oh, and don't forget that as soon as the search results are not needed any more, the original style must be restored. This requirement resulted in an overlay style layer, which is applied over the original style. No need to say how much easier is it to flatten down two well-structured objects!

There one more trick I'd like to mention. When I started implementing [`SearchInput`](https://github.com/vrurg/advent-2021-coloring-the-holiday/blob/3e3dcc75fe2c487f704cc8ed4e8cbc928353bf3e/lib/Runner/UI.rakumod#L769) role, which is responsible for implementing input field for text search functionality, I realized that `Terminal::UI` doesn't have support for turning the cursor on only when it is needed; and to maintain its position at where it is needed. I could send a sequence to enable it, but if a process generates some output at the time, you know what happens: it "snows". And, worse of all, when its done snowing, the cursor is located anywhere else, but inside the input field.

I didn't have time for a PR and came up with a workaround, which takes the `$*OUT` handle, wrapps it into my [`OUTWrapper`](https://github.com/vrurg/advent-2021-coloring-the-holiday/blob/3e3dcc75fe2c487f704cc8ed4e8cbc928353bf3e/lib/Runner/UI.rakumod#L23) object. The object can be told about when the cursor is needed and where exactly it is needed. Then it intercepts `print` method of the original `$*OUT` handle, hides the cursor before passing the control to the original method, and restores after that. All this only whenever necessary. The solution is so simple, that I like it despite its hackyness! Best of all, it doesn't care about edge cases because I found none. Aside of service methods, the core of it is this method:

```
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
```

That's all, folks... With `OUTWrapper.new`, all I is needed to achieve the goal is: `$*OUT.force-at($x, $y)` – and visible cursor will stick to the required position.

Now, as I'm finishing and looking back at this text, it feels somewhat guilty of how little code it has. But the feeling lasts for only a short moment because the article has a whole project [attached](https://github.com/vrurg/advent-2021-coloring-the-holiday) to it. The project is a demo with regard to it using only a dummy `Cro` server as a persistent process; and `ls -lA` command as a replacement for the frontend building. But otherwise it's a fully functional, though unpolished, code. Just take it and use if there is a use for it. Let it be my little hand-made gift to the community for the upcoming holiday! 

And what about the gift for my wife? Well... I turned out to be a terrible elf as it is not done it yet, in the year passed! With a good excuses, though, like Vue+TypeScript learning from the scratch is very time consuming, especially when done in spare time. But more importantly, it allowed me to make great advances in `Cro::RPC::JSON` module, especially in areas, related to WebSockets support. I created `Config::BINDish` module, a beast of configuration formats, to which I hope to come back in a later article, if time allows. Even a couple of Rakudo bugs was squashed as a result of working on the gift. So, a lot has been done, except... But there is no way I'd give up on this!
