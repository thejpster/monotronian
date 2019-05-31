# Monotronian

An interpreted programming language for the [Monotron](https://github.com/thejpster/monotron).

## What is Monotronian

Monotronian is a line-based programming language, similar to Python and BASIC, but not the same as either. It is primarily designed for systems with very low amounts of memory, and so allows line-by-line tokenisation of source code and does not assume that a full-screen editor is available.

```
fn main(args)
	if length(args) == 1
		limit, error = int(args[0])
		if error
			println("Argument ", args[0], " isn't a number?!")
			return
		endif
	else
		limit = 100
	endif
	guesses = []
	target = random(limit)
	println("I have picked a number from 1 to ", limit)
	while true
		guess = input("Enter your guess:")
		guess, error = int(guess)
		if error
			println("That's not a number")
		else
			guesses[] = guess
			if guess > target
				println("Too high!")
			elif guess < target
				println("Too low!")
			else
				println("That's right! Well done :)")
				println("Your guesses were:")
				i = 0
				while i < length(guesses)
					println(i)
					i = i + 1
				endwhile
				break
		endif
	endwhile
endfn
```

This is 712 characters including newlines.

This is what it looks like tokenised and converted to prefix.

```
FN("main", "args")
	IF(EQUALS(CALL("length", VAR("args")),INTEGER(1)))
		ASSIGN(("limit", "error"), CALL("int", VAR("args[0]")))
		IF(VAR("error"))
			CALL("print", STRING("Argument "), VAR("args[0]"), STRING(" isn't a number?!"))
			RETURN
		ENDIF
	ELSE
		ASSIGN(("limit"), INTEGER(100))
	ENDIF
	ASSIGN(("target"), CALL("random", VAR("limit")))
	CALL("print", STRING("I have picked a number from 1 to "), VAR("limit"))
	WHILE(TRUE)
		ASSIGN(("guess"), CALL("input", STRING("Enter your guess:")))
		ASSIGN(("guess", "error"), CALL("int", VAR("guess")))
		IF(VAR("error"))
			CALL("print", STRING("That's not a number"))
		ELSE
			ASSIGN(("guess[]"), VAR("guess"))
			IF(GT(VAR("guess"), VAR("target")))
				CALL("print", STRING("Too high!"))
			ELIF(LT(VAR("guess"), VAR("target")))
				CALL("print", STRING("Too low!"))
			ELSE
				CALL("print", STRING("That's right! Well done :)"))
				CALL("print", STRING("Your guesses were:"))
				ASSIGN("i", INTEGER(0))
				WHILE(LT(VAR("i"), CALL("length", VAR("guesses"))))
					CALL("print", VAR("i"))
					ASSIGN("i", ADD(VAR("i"), INTEGER(1)))
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
literal := T_STRING string | T_INTEGER integer | T_FLOAT float | T_ARRAY argument_list | T_NIL
variable := T_VARIABLE name:string | T_INDEX name:string index:expression
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
*Only valid for string, string*
append := T_APPEND expression expression
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
* length(array or string) -> int
* print(anything...)
* input() -> string
* char(int) -> string
* int(anything) -> int or nil
* float(anything) -> float or nil
* string(anything) -> string
* boolean(anything) -> boolean
**Or is the slice [x..y] syntax better here?**
* left(string or array, count:int) -> string or array
* mid(string or array, start:int, count:int) -> string or array
* right(string or array, count:int) -> string or array
* cursor(enable:boolean)
* move(row:int, col:int)
* random() -> float between 0 and 1

**Some ideas...**
* array() -> array
* append(array, anything) -> nil
* contains(array or dict, anything) -> found:boolean
* find(string or array, anything) -> index:int
* dict() -> dict
* get(dict, key:anything) -> value:anything
* set(dict, key:anything, value:anything) -> old_value:anything

FOR x = 1 to LENGTH(some_array)
some_array[x] = int(some_array[x])
NEXT

## Example use

```
$ list
Listing 36 lines
01: fn main(args)
02: 	if length(args) == 1
03: 		limit, error = int(args[0])
04: 		if error
05: 			println("Argument ", args[0], " isn't a number?!")
06: 			return
07: 		endif
08: 	else
09: 		limit = 100
10: 	endif
11: 	guesses = []
12: 	target = random(limit)
13: 	println("I have picked a number from 1 to ", limit)
14: 	while true
15: 		guess = input("Enter your guess:")
16: 		guess, error = int(guess)
17: 		if error
18: 			println("That's not a number")
19: 		else
20: 			guesses[] = guess
21: 			if guess > target
22: 				println("Too high!")
23: 			elif guess < target
24: 				println("Too low!")
25: 			else
26: 				println("That's right! Well done :)")
27: 				println("Your guesses were:")
28: 				i = 0
29: 				while i < length(guesses)
30: 					println(i)
31: 					i = i + 1
32: 				endwhile
33: 				break
34: 		endif
35: 	endwhile
36: endfn
$ insert after 26
> println("You're so clever.")
$ list
01: fn main(args)
02: 	if length(args) == 1
03: 		limit, error = int(args[0])
04: 		if error
05: 			println("Argument ", args[0], " isn't a number?!")
06: 			return
07: 		endif
08: 	else
09: 		limit = 100
10: 	endif
11: 	guesses = []
12: 	target = random(limit)
13: 	println("I have picked a number from 1 to ", limit)
14: 	while true
15: 		guess = input("Enter your guess:")
16: 		guess, error = int(guess)
17: 		if error
18: 			println("That's not a number")
19: 		else
20: 			guesses[] = guess
21: 			if guess > target
22: 				println("Too high!")
23: 			elif guess < target
24: 				println("Too low!")
25: 			else
26: 				println("That's right! Well done :)")
27: 				println("You're so clever.")
28: 				println("Your guesses were:")
29: 				i = 0
30: 				while i < length(guesses)
31: 					println(i)
32: 					i = i + 1
33: 				endwhile
34: 				break
35: 		endif
36: 	endwhile
37: endfn
$ delete(26)
$ list(25, 29)
25: else
26: 	println("You're so clever.")
27: 	println("Your guesses were:")
28: 	i = 0
29:		while i < length(guesses)
$ edit(26)
> println("You're so clever.")_
$ save("guesses")
$ dir
```