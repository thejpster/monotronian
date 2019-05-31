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
		ASSIGN(("guess"), CALL("input",STRING("Enter your guess:")))
		ASSIGN(("guess", "error"), CALL("int",VAR("guess")))
		IF(VAR("error"))
			CALL("print", CONSTANT("That's not a number"))
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
boolean := T_TRUE | T_FALSE
integer := i32
float := f32
literal := T_STRING string | T_INTEGER integer | T_FLOAT float | T_ARRAY argument_list
variable := T_VARIABLE name:string | T_INDEX name:string index:expression
atom := literal | variable | call
call := T_CALL func:string argument_list
string := string_length:len [ [ char:byte ] * string_length ]
param_list := num_params:len [ [ param_name:string ] * num_params ]
argument_list := num_args:len [ [ arg:expression ] * num_args ]

function := T_FN name:string param_list
endfn := T_ENDFN
short_if := T_SHORTIF expression THEN short_statement
if := T_IF expression
else := T_ELSE
endif := T_ENDIF
while := T_WHILE expression
endwhile := T_ENDWHILE
assign := T_ASSIGN param_list argument_list
return := T_RETURN argument_list
break := T_BREAK
atom := T_VAR name:string | string | T_INT integer | T_FLOAT float

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

expression := literal | not | logical_and | logical_or | bitwise_and | bitwise_or | negate | add | subtract | times | divide | modulo | append | call

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