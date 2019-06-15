# Monotronian

An interpreted programming language for the [Monotron](https://github.com/thejpster/monotron).

## What is Monotronian

Monotronian is a line-based programming language, similar to Python and BASIC, but not the same as either. It is primarily designed for systems with very low amounts of memory, and so allows line-by-line tokenisation of source code and does not assume that a full-screen editor is available.

```
fn main(args)
    if len(args) == 1
        limit := int(args[0])
        if limit == nil
            println("Argument $ isn't a number?!", args[0])
            return
        endif
    else
        limit := 100
    endif
    guesses := []
    target := random(limit)
    println("I have picked a number from 1 to ", limit)
    while true
        guess := input("Enter your guess:")
        guess := int(guess)
        if guess == nil
            println("That's not a number")
        else
            append(guesses, guess)
            if guess > target
                println("Too high!")
            elif guess < target
                println("Too low!")
            else
                println("That's right! Well done :)")
                println("Your guesses were:")
                i := 0
                while i < len(guesses)
                    println(i)
                    i := i + 1
                endwhile
                break
        endif
    endwhile
endfn
```

This is 712 characters including newlines.

This is what it looks like as byte code. String literal lengths, num args and
num params are one byte. Integer literals are i32 (4 bytes, little endian).
Tokens are 1 byte.

```
T_FN                                    ; Function Name, Number of Parameters, Parameter Name 0, ... Parameter Name N-1
04 main
01
04 args
T_IF                                    ; Expression which is tested for truthiness
T_EQUALS                                ; Two expressions tested for equality
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
03 len
01
T_GETVAL                                ; Name of variable
04 args
T_LITERAL_INTEGER
01
T_ASSIGN
05 limit
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
03 int
01
T_GETVAL_IDX                            ; Name of variable, Index Expression
04 args
T_LITERAL_INTEGER
00
T_IF
T_EQUALS                                ; Two expressions tested for equality
T_GETVAL                                ; Name of variable
05 limit
T_LITERAL_NIL
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
02
T_LITERAL_STRING
"Argument $ isn't a number?!"
T_GETVAL_IDX                            ; Name of variable, Index Expression
4args
T_LITERAL_INTEGER
00000000
T_RETURN
T_NIL
T_ENDIF
T_ELSE
T_ASSIGN                                ; Name of variable, Expression
05 limit
T_LITERAL_INTEGER
00000064
T_ENDIF
T_ASSIGN
07 guesses
T_LITERAL_EMPTY_VECTOR
T_ASSIGN
6target
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
06 random
01
T_GETVAL                                ; Name of variable
05 limit
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
02
T_LITERAL_STRING
22 I have picked a number from 1 to $
T_GETVAL                                ; Name of variable
05 limit
T_WHILE                                 ; Expression
T_TRUE
T_ASSIGN
05 guess
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 input
01
T_LITERAL_STRING
13 Enter your guess:
T_ASSIGN
05 guess
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
03 int
01
T_GETVAL                                ; Name of variable
05 guess
T_IF
T_EQUALS                                ; Two expressions tested for equality
T_GETVAL                                ; Name of variable
05 guess
T_LITERAL_NIL
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
02
T_LITERAL_STRING
13 That's not a number
T_ELSE
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
06 append
02
T_GETVAL                                ; Name of variable (arrays are passed by reference)
07 guesses
T_GETVAL                                ; Name of variable
05 guess
T_IF                                    ; Expression
T_GT                                    ; Expression, Expression
T_GETVAL                                ; Name of variable
05 guess
T_GETVAL                                ; Name of variable
6target
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
01
T_LITERAL_STRING
0B Too high!
T_ELIF
T_LT
T_GETVAL                                ; Name of variable
05 guess
T_GETVAL                                ; Name of variable
06 target
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
01
T_LITERAL_STRING
0A Too low!
T_ELSE
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
01
T_LITERAL_STRING
1B That's right! Well done :
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
01
T_LITERAL_STRING
12 Your guesses were:
T_ASSIGN
01 i
T_LITERAL_INTEGER
0
T_WHILE
T_LT
T_GETVAL                                ; Name of variable
01 i
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
06 length
01
T_GETVAL                                ; Name of variable
07 guesses
T_CALL                                  ; Function to call, Number of Arguments, Argument 0, ... Argument N-1
05 print
01
T_GETVAL                                ; Name of variable
01 i
T_ASSIGN
01 i
T_ADD
T_GETVAL                                ; Name of variable
01 i
T_LITERAL_INTEGER
00000001
T_ENDWHILE
T_BREAK
T_ENDIF
T_ENDIF
T_ENDWHILE
T_ENDFN
```

This program is 159 bytes, plus string literals.

const T_ADD: u8 = 0x00;
const T_ARRAY: u8 = 0x02;
const T_ASSIGN: u8 = 0x03;
const T_BAND: u8 = 0x04;
const T_BOOLEAN: u8 = 0x05;
const T_BOR: u8 = 0x06;
const T_BREAK: u8 = 0x07;
const T_CALL: u8 = 0x08;
const T_CHAR: u8 = 0x09;
const T_DIFFERENT: u8 = 0x0A;
const T_DIVIDE: u8 = 0x0B;
const T_ELIF: u8 = 0x0C;
const T_ELSE: u8 = 0x0D;
const T_ENDFN: u8 = 0x0E;
const T_ENDIF: u8 = 0x0F;
const T_ENDWHILE: u8 = 0x10;
const T_EQUALS: u8 = 0x11;
const T_FALSE: u8 = 0x12;
const T_FLOAT: u8 = 0x13;
const T_FN: u8 = 0x14;
const T_FOR: u8 = 0x15;
const T_FOR_STEP: u8 = 0x16;
const T_GETVAL: u8 = 0x17;
const T_GETVAL_IDX: u8 = 0x18;
const T_GT: u8 = 0x19;
const T_GTE: u8 = 0x1A;
const T_IF: u8 = 0x1B;
const T_INTEGER: u8 = 0x1D;
const T_LAND: u8 = 0x1E;
const T_LITERAL_INTEGER: u8 = 0x1F;
const T_LITERAL_NIL: u8 = 0x20;
const T_LITERAL_STRING: u8 = 0x21;
const T_LOR: u8 = 0x22;
const T_LT: u8 = 0x23;
const T_LTE: u8 = 0x24;
const T_MODULO: u8 = 0x25;
const T_NEGATE: u8 = 0x26;
const T_NEXT: u8 = 0x27;
const T_NIL: u8 = 0x28;
const T_NOT: u8 = 0x29;
const T_RETURN: u8 = 0x2A;
const T_SHORTIF: u8 = 0x2B;
const T_STRING: u8 = 0x2C;
const T_SUBTRACT: u8 = 0x2D;
const T_TIMES: u8 = 0x2E;
const T_TRUE: u8 = 0x2F;
const T_VAR: u8 = 0x30;
const T_WHILE: u8 = 0x31;

This is what is looks like in postfix notation (which is more amenable for a stack-based implementation).

```
FN("main", "args")
    IF(EQUALS(CALL("len", GETVAL("args")),LITERAL_INTEGER(1)))
        ASSIGN("limit", CALL("int", GETVAL("args[0]")))
        IF(EQUALS(GETVAL("limit"),LITERAL_NIL))
            CALL("print", LITERAL_STRING("Argument "), GETVAL("args[0]"), LITERAL_STRING(" isn't a number?!"))
            RETURN(NIL)
        ENDIF
    ELSE
        ASSIGN(("limit"), LITERAL_INTEGER(100))
    ENDIF
    ASSIGN(("target"), CALL("random", GETVAL("limit")))
    CALL("print", LITERAL_STRING("I have picked a number from 1 to "), GETVAL("limit"))
    WHILE(TRUE)
        ASSIGN("guess", CALL("input", LITERAL_STRING("Enter your guess:")))
        ASSIGN("guess", CALL("int", GETVAL("guess")))
        IF(EQUALS(GETVAL("guess"), LITERAL_NIL))
            CALL("print", LITERAL_STRING("That's not a number"))
        ELSE
            CALL("append", GETVAL("guesses"), GETVAL("guess"))
            IF(GT(GETVAL("guess"), GETVAL("target")))
                CALL("print", LITERAL_STRING("Too high!"))
            ELIF(LT(GETVAL("guess"), GETVAL("target")))
                CALL("print", LITERAL_STRING("Too low!"))
            ELSE
                CALL("print", LITERAL_STRING("That's right! Well done :)"))
                CALL("print", LITERAL_STRING("Your guesses were:"))
                ASSIGN("i", LITERAL_INTEGER(0))
                WHILE(LT(GETVAL("i"), CALL("length", GETVAL("guesses"))))
                    CALL("print", GETVAL("i"))
                    ASSIGN("i", ADD(GETVAL("i"), LITERAL_INTEGER(1)))
                ENDWHILE
                BREAK
            ENDIF
        ENDIF
    ENDWHILE
ENDFN
```

One way to encode this as bytes is:

bytes := 0x00 .. 0xFF
len := u8
nil := T_NIL
boolean := T_TRUE | T_FALSE
integer := i32
float := f32
literal := T_STRING string | T_ARRAY argument_list | scalar
scalar := T_INTEGER integer | T_FLOAT float | T_BOOLEAN boolean | T_NIL | T_CHAR char
variable := T_GETVAL name:string | T_GETVAL_IDX name:string index:expression
call := T_CALL func:string argument_list
string := string_length:len [ [ char:byte ] * string_length ]
param_list := num_params:len [ [ param_name:string ] * num_params ]
argument_list := num_args:len [ [ arg:expression ] * num_args ]

function := T_FN name:string param_list
endfn := T_ENDFN
short_if := T_SHORTIF expression THEN short_statement
if := T_IF expression
for := T_FOR variable expression expression
for_step := T_FOR_STEP variable expression expression expression
next := T_NEXT
else := T_ELSE
endif := T_ENDIF
while := T_WHILE expression
endwhile := T_ENDWHILE
assign := T_ASSIGN variable expression
return := T_RETURN expression
break := T_BREAK

*Only valid for boolean*
not := T_NOT expression
*Only valid for boolean, boolean*
logical_and := T_LAND expression expression
logical_or := T_LOR expression expression
*Only valid for int, int*
bitwise_and := T_BAND expression expression
bitwise_or := T_BOR expression expression
*Only valid for int or float*
negate := T_NEGATE expression
*Only valid for int, int or float, float*
add := T_ADD expression expression
subtract := T_SUBTRACT expression expression
times := T_TIMES expression expression
divide := T_DIVIDE expression expression
modulo := T_MODULO expression expression
*Only valid for any two types which are the same*
equals := T_EQUALS expression expression
different := T_DIFFERENT expression expression
less_than := T_LT expression expression
less_than_equal := T_LTE expression expression
greater_than := T_GT expression expression
greater_than_equal := T_GTE expression expression

expression := variable | literal | not | logical_and | logical_or | bitwise_and | bitwise_or | negate | add | subtract | times | divide | modulo | append | equals | different | less_than | less_than_equal | greater_than | greater_than_equal | call | for | for_step | next

**NB: Is expression just a special case of assign, with zero parameters?**
stored_line := statement
short_statement := assign | call | return
statement := if | assign | call | else | endif | while | endwhile | return
**If it's an expression, we print the result**
immediate_line := assign | expression

For any given pointer p which points at one of the above, you can determine the type of that item by looking at that byte, and compute the length of that item by computing the length of the constituent pieces. You can evaluate an expression by
calculating the value of the constituent pieces. We don't have to worry about brackets as the shunting-yard algorithm has sorted that.

To call a function, we can either look up the function name in a special index, or we can walk the tree looking for functions defined at the top-level. Nested functions are not supported. Closures are not supported. Maybe we could have a cache of pointers to the X most recently called functions, to speed up lookup time.

This ends up being a bit like the S-Expressions used in LISP and Web Assembly.

Variables live on the stack. Global variables live on the heap. Strings are heap allocated. Maybe we could support pointers, boxing integers and floats and dereferencing? Maybe we could support structures and fields? Or dictionaries? How about arrays/lists? Method syntax (var.method(args))?

Maybe each item above (prefixed with T_xxx token) could also include a value which was how long that item was in bytes. This would make it quicker to skip through complex expressions? And it is fixed once the line has been parsed and stored? Maybe an optimisation for later.

## Built-in functions

* sin / cos / tan / etc
* len(collection) -> int
* print(anything...)
* println(anything...)
* input() -> string
* char(int) -> string
* int(scalar) -> int or nil
* float(scalar) -> float or nil
* string(scalar) -> string
* boolean(scalar) -> boolean
**Or is the slice [x..y] syntax better here?**
* array_left(array, count:int) -> array
* array_mid(array, start:int, count:int) -> array
* array_right(array, count:int) -> array
* string_left(string, count:int) -> string
* string_mid(string, start:int, count:int) -> string
* string_right(string, count:int) -> string
* cursor_enable(enable:boolean)
* cursor_move(row:int, col:int)
* maths_random() -> float between 0 and 1

We can't use Vec and HashMap without `collections` and `alloc` and they are both nightly only. I'd like to avoid nightly if at all possible.

BASIC has floats (X), integers (X%) and strings (X$), and homogeneous arrays of these. Functions that end $ take strings (like MID$). We could take a Python approach where scalars are Copy (always by value) but lists are by reference (and strings are immutable...). QBasic's approach is that all function arguments are passed by reference. How that works if you supply an expression I don't know.

```
x := 0
increment(x)
if x == 0
    print("Of course it is!")
endif
increment(6) # is a no-op
x := array_int(5)
do_stuff(x)

fn increment(z)
    z := z + 1 # Creates a new (local) z
    return z
endfn

fn do_stuff(a)
    print(a[0]) # Fine
    array_append(a, 5) # Fine
    a := array_int(6) # Makes a new a
endfn
```

Internal Rust functions we'll need:

// Allocate a value on the stack
* allocate_array(name, len)
* allocate_string(name, value)
* allocate_scalar_int(name, value)
* allocate_scalar_float(name, value)
* allocate_scalar_boolean(name, value)
* allocate_scalar_char(name, value)

// When we call a function 

**Some ideas...**
* array() -> array
* append(array, anything) -> nil
* contains(array or dict, anything) -> found:boolean
* find(string or array, anything) -> index:int
* dict() -> dict
* get(dict, key:anything) -> value:anything
* set(dict, key:anything, value:anything) -> old_value:anything

FOR x := 1 to LENGTH(some_array)
some_array[x] := int(some_array[x])
NEXT

## Example use

```
$ main = load("example.mon")
$ list(main)
Listing 36 lines of main()
01: fn main(args)
02:     if len(args) == 1
03:         limit := int(args[0])
04:         if limit == nil
05:             println("Argument $ isn't a number?!", args[0])
06:             return
07:         endif
08:     else
09:         limit := 100
10:     endif
11:     guesses := []
12:     target := random(limit)
13:     println("I have picked a number from 1 to ", limit)
14:     while true
15:         guess := input("Enter your guess:")
16:         guess := int(guess)
17:         if guess == nil
18:             println("That's not a number")
19:         else
20:             append(guesses, guess)
21:             if guess > target
22:                 println("Too high!")
23:             elif guess < target
24:                 println("Too low!")
25:             else
26:                 println("That's right! Well done :)")
27:                 println("Your guesses were:")
28:                 i := 0
29:                 while i < len(guesses)
30:                     println(i)
31:                     i := i + 1
32:                 endwhile
33:                 break
35:             endif
36:         endif
37:     endwhile
38: endfn
$ insert(main, 26)
> println("You're so clever.")
$ list main
01: fn main(args)
02:     if len(args) == 1
03:         limit := int(args[0])
04:         if limit == nil
05:             println("Argument $ isn't a number?!", args[0])
06:             return
07:         endif
08:     else
09:         limit := 100
10:     endif
11:     guesses := []
12:     target := random(limit)
13:     println("I have picked a number from 1 to ", limit)
14:     while true
15:         guess := input("Enter your guess:")
16:         guess := int(guess)
17:         if guess == nil
18:             println("That's not a number")
19:         else
20:             append(guesses, guess)
21:             if guess > target
22:                 println("Too high!")
23:             elif guess < target
24:                 println("Too low!")
25:             else
26:                 println("That's right! Well done :)")
27:                 println("You're so clever.")
28:                 println("Your guesses were:")
29:                 i := 0
30:                 while i < len(guesses)
31:                     println(i)
32:                     i := i + 1
33:                 endwhile
34:                 break
36:             endif
37:         endif
38:     endwhile
39: endfn
$ delete(main, 26)
$ list(main, 25, 29)
25: else
26:     println("You're so clever.")
27:     println("Your guesses were:")
28:     i := 0
29:     while i < len(guesses)
$ edit(26)
> println("You're so clever.")_
$ save("guesses")
$ dir
```
