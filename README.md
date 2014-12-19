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
`main.erl`
~~~
-module(main).
-export([main/0]).

main() -> io:fwrite("Hello World!\n").
~~~

~~~
> erl
c(main).
main:main().
halt().
~~~

### FSharp
`Main.fs`
~~~
[<EntryPoint>]
let main argv =
  printfn "Hello World!"
  0
~~~
~~~
> fsc Main.fs
> Main.exe
~~~

### Haskell
`Main.hs`
~~~
module Main where

main = putStrLn "Hello World!"
~~~
~~~
> ghc Main.hs
> Main.exe
~~~

### Rust
`Main.rs`
~~~
fn main() {
  println!("Hello World!");
}
~~~
~~~
> rustc Main.rs
> Main.exe
~~~

### Scala
`Main.scala`
~~~
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello World!");
  }
}
~~~
~~~
> scalac Main.scala
> scala Main
~~~

## Primitives
| Erlang | FSharp         | Haskell | Rust   | Scala   |
| ------ | -------------- | ------- | ------ | ------- |
|        | ()             | ()      | ()     | Unit    |
|        | bool           | Bool    | bool   | Boolean |
|        | int            | Int     | int    | Int     |
|        | uint32         | Word    | uint   |         |
|        | sbyte          | Int8    | i8     | Byte    |
|        | int16          | Int16   | i16    | Short   |
|        | int            | Int32   | i32    | Int     |
|        | int64          | Int64   | i64    | Long    |
|        | byte           | Word8   | u8     |         |
|        | uint16         | Word16  | u16    |         |
|        | uint32         | Word32  | u32    |         |
|        | uint64         | Word64  | u64    |         |
|        | single/float32 | Float   | f32    | Float   |
|        | double/float   | Double  | f64    | Double  |
|        | char           | Char    | char   | Char    |
|        |                |         | str    |         |
|        | string         | String  | String | String  |
