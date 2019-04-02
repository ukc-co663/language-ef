# Language EF Interpreter

An interpreter built directly from the semantic reduction and type
checking rules of language EF.

## Compile

You will need menhir to build this project:

`opam install menhir`

Otherwise it should build easily with `make`.

## Running

`./ef.byte` will take in an expression and evaluate it, and print the
result and its type.


## Example expressions

```
(5+3)
```
Outputs: `8 : num`

```
((6*7)+|"Hello!"|)
```
Outputs: `48 : num`

```
let x be 7 in (6*x)
```
Outputs: `42 : num`

```
let x be "Hello " in (x ++ "world!")
```
Outputs: `"Hello world!" : str`

```
let f be \ {num} (x) . (x+3) in (f) (6)
```
Outputs: `9 : num`

```
\ {str} (z) . (|z| + 1)
```
Outputs: `(function) : (str -> num)`
