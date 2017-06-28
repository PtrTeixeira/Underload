## Underload ##

This is an interpreter for the
[Underload programming languages](https://esolangs.org/wiki/Underload),
 mostly just because I think that it is really neat.


### Usage ###
This program is written in the Racket programming language, and will require
that you have Racket installed to do anything.

```
$ racket interpreter.rkt --help
underload [ <option> ... ] <input>
 where <option> is one of
  --here, -s : Interpret given argument as a program, not a file
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
```

### Contributing ###
If you want to make suggestions or changes or improvements, go for it. If you
open a PR or an issue, I will probably get back to you pretty quickly, but I
can't make any guarantees about how involved I'm going to be. I did this as a
hobby project and don't work that much with either programming languages or
Racket.
