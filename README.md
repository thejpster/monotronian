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

This might tokenise down to

```
F2XmainXargsI=CXlength1VXargs,G1AXlimit,Xerror,CXint,VXargs[0]IFVXerrorCXprint",SXArgument,VXargs[0],SX isn't a number?!RiEAXlimit,G100iAXtarget,CXrandom,VXlimitCXprint,SXI have picked a number from 1 to ,VXlimitWTRUEAXguess,CXinput,SXEnter your guess:AXguess,"error,CXint,VXguessIFVXerrorCXprint,CONSTANTXThat's not a numberEAXguess[],VXguessIFGTVXguess,VXtargetCXprint,SXToo high!L<VXguess,VXtargetCXprint,SXToo low!ECXprint,SXThat's right! Well done :)CXprint,SXYour guesses were:AXi,G0W<VXi,CXlength,VXguessesCXprint,VXiAXi",+VXi,G1wBREAKiiwf
```

Which is 546 characters and should be completely equivalent.

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