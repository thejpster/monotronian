# Monotronian

An interpreted programming language for the [Monotron](https://github.com/thejpster/monotron).

## What is Monotronian

Monotronian is a line-based programming language, based on and compatible with BBC BASIC, but with some additions. The best introduction is probably with an example:

```
10 LIMIT% = 100
20 TARGET% = RND(LIMIT%)
30 PRINT "I have picked a number from ", LIMIT%
40 INPUT "Enter your guess:", GUESS%
50 IF GUESS% > TARGET% THEN PRINT "Too high! Try again."
60 IF GUESS% < TARGET% THEN PRINT "Too low! Try again."
70 IF GUESS% = TARGET% THEN
80 PRINT "That's right! Well done."
90 END
100 ENDIF
110 GOTO 40
```

So far, so BASIC. Line numbers, GOTO, integers qualified with a %. As lines are entered, they are reduced to a compact byte-code representation to save memory and for speedier execution. There's also REPL, but unlike BASIC it will automatically print the result of any expressions entered, like Python.

```
> 1 + 2
3
> "Hello"
Hello
> LEFT$("Foo", 1)
F
```

Keywords are accepted in either case, with a simple A-Z to a-z transform. Input is UTF-8.

We can also enter the example above as:

```
> auto on
>> dim limit, target, guess as integer
>> limit = 100
>> target = rnd(limit)
>> print "I have picked a number from 1 TO ", limit
>> loop
>>   input "ENTER YOUR GUESS:", guess
>>   if guess > target then print "Too high! Try again."
>>   if guess < target then print "Too low! Try again."
>>   if guess = target then
>>     print "That's right! Well done."
>>     end
>>   endif
>> endloop
>
```
The `auto` command activates automatic line numbering. Now what we type is added as a new line to our program, until we enter a blank line. If we list this program, it has been numbered.

```
> list
 1 dim limit, target, guess as integer
 2 limit = 100
 3 target = rnd(limit)
 4 print "I have picked a number from 1 TO ", limit
 5 while true
 6   input "ENTER YOUR GUESS:", guess
 7   if guess > target then print "Too high! Try again."
 8   if guess < target then print "Too low! Try again."
 9   if guess = target then
10     print "That's right! Well done."
11     end
12   endif
13 endwhile
>
```

We can bring a line into the edit buffer with the `edit` command:

```
> edit 6
input "ENTER YOUR GUESS:", guess_
```

We can also insert lines:

```
> insert 11
>> print "Please do play again".
> list
 1 dim limit, target, guess as integer
 2 limit = 100
 3 target = rnd(limit)
 4 print "I have picked a number from 1 TO ", limit
 5 while true
 6   input "ENTER YOUR GUESS:", guess
 7   if guess > target then print "Too high! Try again."
 8   if guess < target then print "Too low! Try again."
 9   if guess = target then
10     print "That's right! Well done."
11     print "Please do play again."
12     end
13   endif
14 endwhile
```

Inserting lines causes subsequent lines to be automatically renumbered. You cannot jump to an automatically numbered line with `GOTO` or `GOSUB` - instead you must use functions. When listing code that has been auto-numbered, it is indented automatically, with two spaces per indent level.

## Defining functions and procedures

There is a new function declaration syntax. Any function that reaches `endfn` without a `return` returns a nil type (i.e. is a 'procedure' in old BASIC terminology).

```
fn add_one(arg)
  return arg + 1
endfn
```

The old BBC BASIC syntax also works:

```
DEF FNadd_one(arg)
=arg+1

DEF PROCboo
PRINT "boo"
ENDPROC
```

All variables declared with `DIM` at the top-level of the program, as well as all of the single-letter variables `A$`-`Z$`, `A`-`Z` and `A%`-`Z%`are global variables. Any variable declared with `DIM` in a `FN` is a local variable. In a `DEF` function, you must use `LOCAL` to make a variable local.

## Numeric formats.

Integers are 32-bit signed. Long integers are 64-bit signed. Real values are 64-bit doubles. Strings are 8-bit (maybe with UTF-8 as a compile option at a later date).

Integers can be expressed in decimal (`123`) or hex (`0x123`). Real numbers can be expressed in decimal (`123.456`). Underscores are allowed within the digits except in the first position (`1_000_000` is OK, `_1000.0` is not).

## Graphics extensions

TBD

Expect functions to get/set the graphics mode, change colours, line drawing, point plotting, filled shapes, etc.

## Text extensions

TBD

Expect functions to get the console size, move the cursor, change colours, etc.

## In-line assembler

TBD

