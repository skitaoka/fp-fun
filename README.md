fp-fun
======

Functional programming samples for me

## installation

## build tools
| lang    | build tool | compiler   | interpreter |
| ------- | ---------- | ---------- | ----------- |
| Erlang  | rebar      | erlc       | erl         |
| F#      | fake       | fsc        | fsi         |
| Haskell | cabal      | ghc        | ghci        |
| Rust    | cargo      | rustc      |             |
| Scala   | sbt        | scalac/fsc | scala       |

## Hello World!

### Erlang
~~~
-module(main).
-export([main/0]).

main() -> io:fwrite("Hello World!\n").
~~~

### FSharp
~~~
[<EntryPoint>]
let main argv =
  printfn "Hello World!"
  0
~~~

### Haskell
~~~
module Main where

main = putStrLn "Hello World!"
~~~

### Rust
~~~
fn main() {
  println!("Hello World!");
}
~~~

### Scala
~~~
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello World!");
  }
}
~~~

## Primitives
