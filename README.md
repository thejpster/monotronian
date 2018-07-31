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

## List of keywords
### CASE/OF/OTHERWISE/ENDCASE
A select statement.

```basic
CASE x% OF
WHEN 1, 2:
PRINT "SMALL"
CASE 3, 4:
PRINT "MEDIUM"
OTHERWISE
PRINT "LARGE"
ENDCASE
```

### DEF/ENDPROC
Define a new function or procedure (old-style)

```basic
DEF FN_addone(A)
=A+1
```

```basic
DEF PROC_print
PRINT "HELLO"
ENDPROC
```

### BY
Makes `DRAW` relative

```basic
DRAW BY 100,0
```

### IF/ELSEIF/ELSE/ENDIF
Conditional branching.

Single line:
```basic
IF X == 0 THEN PRINT "FOO" ELSE PRINT "BAR"
```

Multi-line:
```basic
IF X > 10 THEN
PRINT "BAR"
PRINT "BAZ"
ELSEIF X < 5 THEN
PRINT "QUUX"
ELSE
PRINT "FAIL"
ENDIF
```

### FN/ENDFN
Define a new function (new-style)

```basic
FN my_function(a, b)
LOCAL X
X = A + B
X = X / 2
RETURN X
ENDFN
```

### WHILE/ENDWHILE
Loop with initial guard expression.

```basic
WHILE X > 0
PRINT "BAZ"
X = X - 1
ENDWHILE
```

### FOR/STEP/NEXT
Counting loop.

```basic
FOR X = 1 TO 100 STEP 10
PRINT X
NEXT
```

### INTEGER
A type for use with `DIM`

```basic
DIM X AS Integer
```

### LET
Perform an assignment to a variable. It can be ommitted for brevity but is
retained for compatibility.

```basic
LET X = 10
```

### LOCAL
Declare a variable to have function scope.

```basic
FN my_function(a, b)
LOCAL X
X = A + B
X = X / 2
RETURN X
ENDFN
```

### PRIVATE
Declare a variable to have function scope and remember value between calls

```basic
FN my_function(a, b)
PRIVATE Y
LOCAL X
Y = Y + 1
X = A + B
X = X / 2
RETURN X
ENDFN
```

### REM
Start a comment.

```basic
REM This is a comment
```

### REPEAT/UNTIL
Loop with closing guard expression

```basic
X = 10
REPEAT
PRINT "HEY YOU"
X = X - 1
UNTIL X = 0
```

## List of operators
### `DIV` or `/`
Integer division.

```basic
A = 100
X = A / 10
PRINT X
```

### `EOR`
Exclusive OR

```basic
POKE 4096, PEEK(4096) EOR 0x80
```

### `-`
Subtraction or negation

```basic
X = X - 1
```

### `MOD`
Calculate a remainder

```basic
X = A MOD 3
```

### `NOT`
Logical negation

```basic
IF NOT X THEN
PRINT "OH"
ENDIF
```

### `<>`
Not equal

```basic
IF X <> 3 THEN
PRINT "OH"
ENDIF
```

### `OR`
Boolean or logical or

```basic
POKE 4096, PEEK(4096) OR 1
```

### `+`
Addition

```basic
X = X + 1
```

## List of functions

Functions take zero or more arguments and return a value (string, integer or
real). Functions that return strings have '$' in the name. Arguments, if
given, must be in parentheses.

### ABS - Absolute

Returns the absolute value of the given real argument.

```basic
> ABS(-1.0)
1.0

```
### ACS
Arc-cosine, in radians.

```basic
> ACOS(-1.0)
3.141592654
```

### ASC
Get ASCII value of first character in given string.

```basic
> ASC("A")
65
```

### ASC
Arc-sine, in radians.

```basic
> ASC(1.0)
1.570796327
```

### ATN
Arc-tangent, in radians.

```basic
> ATN(1.0)
0.785398163
```

### CHR$
Convert a numeric ASCII code to a string

```basic
> CHR$(65)
"A"
```

### COS
Cosine of an angle given in radians.

```basic
> COS(3.141592)
0.99849715
```

### COUNT
Number of text chars written since last call

```basic
> COUNT
4
```

### DEG
Convert radians to degrees

```basic
> DEG(3.141592)
180.0
```

### EOF#
check if at end of file

```basic
> EOF#3
False
```

### ERL
Line number of last error

```basic
> ERL
10
```

### ERR
Error code of last error

```basic
> ERR
1200
```

### EXP
Calculates 'e' to the given argument (the inverse natural logarithm)

```basic
> EXP(1)
2.718281828
```

### EXT#
Total length of open file, in bytes.

```basic
> EXT#2
1024
```

### GET
Check for numeric keyboard input (this is also a command).

### GET$
Get for character keyboard input (this is also a command)

### INKEY
GET with timeout

### INKEY$
GET$ with timeout

### INSTR
Find a string within a string

### INT
Convert a real to an integer

### LEFT$
Return characters from the left of a string

### LEN
Get the length of a string

### LN
Calculate natural logarithm

### LOG
Calculate a logarithm with base 10

### MID$
Return characters from the middle of a string

### POS
Return the cursor's horizontal position (i.e. the current screen column)

### RAD
Convert degrees to radians

### RIGHT$
Return characters from the right of a string

### RND
Get a random number

### SGN
Return -1, 0 or 1 if argument is negative, zero or positive

### SIN
Calculate the sine of the argument

### SPC
Print a  number of spaces to the screen

### SQR
Calculate the square root of the argument

### STR$
Convert a number to a string

### STRING$
Concatentates 'n' copies of a string

### SUM
Sum all of the items in the given array

### TAN
Calculate the tangent of the given argument

### TINT
Return the colour of a given point on screen

### VAL
Convert a string to a number

### VPOS
Return the current row

### WIDTH
Return the current screen width


## List of commands
### AUTO
Manage automatic line numbering

### BEEP
Emit audio

### BPUT#
Write a byte to a file

### BGET#
Get a byte from a file

### BREAK
Exit a loop

### CALL
Jump to a machine-code routine

### CIRCLE
Draw a circle

### CLEAR
Delete all variables

### CLG
Clear the graphics viewport

### CLOSE
Close a file

### CLS
Clear the text viewport

### COLOUR (also COLOR)
Set foreground/background colour

### DATA
Store numeric data in the source code

### DIM
Declare a new variable

### DRAW
Draw on the screen

### EDIT
Edit a line of code

### ELLIPSE
Draw an ellipse on screen

### END
Quit the program

### FILL
Perform a flood-fill

### GCOL
Set the foreground/background colour

### GET
Check for keyboard input (this is also a function)

### GET#
Read from a file

### GET$
Get for character keyboard input (this is also a function)

### GOSUB
Jump to a sub-routine

### GOTO
Jump to a line

### INPUT
Read a line from the keyboard

### INPUT#
Read from file

### INSERT
Add a line to the program

### LINE
Draw a line

### LIST
Show the current program

### LOAD
Load a new program

### MODE
Change graphics mode

### MOVE
Move the graphics cursor

### NEW
Delete the current program

### OFF
Hide the cursor

### ON
Enable the cursor

### OPEN
Open a file

### ORIGIN
Change the origin for graphics commands

### POINT
Draw a single point on the screen

### POKE
Write a value to memory

### PRINT
Print to the screen

### PRINT#
Print to an open file

### QUIT
Exit the program

### READ
Read data from a `DATA` statement

### RECTANGLE
Draw a rectangle

### RESTORE
Reset the data pointer

### RETURN
Return to the last GOSUB

### RUN
Run the current program

### SAVE
Save the current program

### SOUND
Generate some noise

### STOP
Same as 'END'

### STROKE
Set the line colour

### SWAP
Swap two variables

### SWI
Perform a software interrupt to run some OS code

### SYS
Jump to a machine language routine

### TAB
Used with 'PRINT' or 'INPUT' to align to a specific column

### TRACE
Enable/disable printing of line numbers as code is executed

### TIME
Uptime of the system in 1/100sec units

### TIME$
The wall time as a string in the format "Day.dd Mon yyyy,hh:mm:ss"

### USR
Jump to a machine-language routine

### WAIT
Wait a given number of 1/100sec ticks, or for the next v-sync period.

### VERIFY
Check the program in memory against a saved file

## Defining new functions and procedures

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

