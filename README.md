# Monotronian

An interpreted programming language for the [Monotron https://github.com/thejpster/monotron].

## What is Monotronian

Monotronian is a curly-bracket programming language, a little bit like Javascript, but much simpler. The best introduction is probably with an example:

```
let x = 123;
let y = foo(x);
if y > x {
    baz(true);
} else {
    bar(false);
}
return 0x200;
```

The system is designed to parse one individual function at a time. The idea is that the Monotron will have a full-screen editor, but to save memory, you will only be able to edit a single function at a time. When exiting that editor, your code will be compiled down to an Abstract Syntax Tree using this crate. That AST is persisted in memory to subsequent execution, either called from another function or called directly from the command line.

Monotron will also feature a REPL - items entered here will be considered as a function with no arguments.

```
> 1 + 2
3
> print("Hello, world!")
"Hello, world!"
> while true { print("You smell!") }
You smell!
You smell!
You smell!
You smell!
You smell!
You smell!
<Ctrl+C>
> edit foo
<full screen editor appears for the 'foo' function>
> list foo
let parts = ["a", "b", "c", "d"]
for x in 1..4 {
	print("parts[");
	print(x);
	print("] is ");
	print(parts[x]);
	print("\n");
}
> foo()
parts[1] is "a"
parts[2] is "b"
parts[3] is "c"
parts[4] is "d"
>
```
