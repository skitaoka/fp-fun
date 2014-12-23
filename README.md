fp-fun
======

Functional programming samples for me

複数の言語をやると、どれがどの文法だったか混乱するので、ここにまとめようと思った。

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

## FizzBuzz

### Erlang
### FSharp
### Haskell
~~~
module Main where

fizzbuzz :: [String]
fizzbuzz = map toFizzbuzz [1..]
  where
  toFizzbuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod`  5 == 0 = "Fizz"
    | n `mod`  3 == 0 = "Buzz"
    | otherwise       = show n

main = print $ take 100 fizzbuzz
~~~

### Rust
### Scala
~~~
object Fizzbuzz {

  def fizzbuzz(n: Int) = {
    1 to n
  } map {
    case n if n % 15 == 0 => "FizzBuzz"
    case n if n %  3 == 0 => "Fizz"
    case n if n %  5 == 0 => "Buzz"
    case n                => n.toString
  }

  def main(args: Array[String]): Unit = {
    println(fizzbuzz(100))
  }
}
~~~

## Installation

## Tools
| lang    | build tool | compiler   | interpreter |
| ------- | ---------- | ---------- | ----------- |
| Erlang  | rebar      | erlc       | erl         |
| F#      | fake       | fsc        | fsi         |
| Haskell | cabal      | ghc        | ghci        |
| Rust    | cargo      | rustc      |             |
| Scala   | sbt        | scalac/fsc | scala       |

## Primitives
| Erlang | FSharp          | Haskell      | Rust        | Scala   | 意味                |
| ------ | --------------- | ------------ | ----------- | ------- | ------------------- |
|        | ()              | ()           | ()          | Unit    | ユニット            |
|        | bool            | Bool         | bool        | Boolean | 真偽値              |
|        | sbyte           | Int8         | i8          | Byte    |  8 bit 符号あり整数 |
|        | int16           | Int16        | i16         | Short   | 16 bit 符号あり整数 |
|        | int             | Int32, Int   | i32, int    | Int     | 32 bit 符号あり整数 |
|        | int64           | Int64        | i64         | Long    | 64 bit 符号あり整数 |
|        | byte            | Word8        | u8          |         |  8 bit 符号なし整数 |
|        | uint16          | Word16       | u16         |         | 16 bit 符号なし整数 |
|        | uint32          | Word32, Word | u32, uint   |         | 32 bit 符号なし整数 |
|        | uint64          | Word64       | u64         |         | 64 bit 符号なし整数 |
|        | single, float32 | Float        | f32         | Float   | 単精度浮動小数点数  |
|        | double, float   | Double       | f64         | Double  | 倍精度浮動小数点数  |
|        | char            | Char         | char        | Char    | 文字                |
|        | string          | String       | str, String | String  | 文字列              |

## Operators
| Erlang    | FSharp             | Haskell      | Rust         | Scala        | 意味               |
| --------- | ------------------ | ------------ | ------------ | ------------ | ------------------ |
| ==, =:=   | =                  | ==           | ==           | ==           | 等しい             |
| /=, =/=   | <>                 | /=           | !=           | !=           | 等しくない         |
| <         | <                  | <            | <            | <            | より小さい         |
| >         | >                  | >            | >            | >            | より大きい         |
| <=        | <=                 | <=           | <=           | <=           | 以下               |
| >=        | >=                 | >=           | >=           | >=           | 以上               |
| and       | &&                 | &&           | &&           | &&           | 論理積             |
| or        | &#124;&#124;       | &#124;&#124; | &#124;&#124; | &#124;&#124; | 論理和             |
| xor       |                    |              |              |              | 排他的論理和       |
| not       | not                | not          | !            | !            | 論理否定           |
| +         | +                  | +            | +            | +            | 加算               |
| -         | -                  | -            | -            | -            | 減算               |
| *         | *                  | *            | *            | *            | 乗算               |
| /         | /                  | /            | /            | /            | 除算               |
|           | **                 | ^            |              |              | 累乗               |
| div       | /                  | \`div\`      | /            | /            | 整数除算           |
| rem       | %                  | \`mod\`      | %            | %            | 整数剰余           |
| band      | &&&                | .&.          | &            | &            | ビット論理積       |
| bor       | &#124;&#124;&#124; | .&#124;.     | &#124;       | &#124;       | ビット論理和       |
| bxor      | ^^^                | xor          | ^            | ^            | ビット排他的論理和 |
| bnot      | ~~~                |              | !            | !            | ビット論理否定     |
| bsl       | <<<                | shiftL       | <<           | <<           | ビット左シフト     |
| bsr       | >>>                | shiftR       | >>           | >>,>>>       | ビット右シフト     |
|           | +                  | ++           | +            | +            | 文字列結合         |

## かきかけ
* リスト, 配列
* タプル, レコード
* 判別共用体, 列挙型
* 型クラス, トレイト
* 構文
  * if-then-else
  * for
  * while
  * パターンマッチ, ガード
