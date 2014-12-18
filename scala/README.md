Scala
======

Scala samples for me

## `scalac`

コンパイラ

起動が重いので、デーモンが同時に立ち上がる `fsc` を利用する。

## `scala`

バイトコードインタプリタ

## `sbt`

ビルドツール

### フォルダ構成

* project-dir
  * build.sbt
  * src
    * main
      * java
      * scala
        * Main.scala

`project-dir` でコマンドをたたく。

### 設定ファイル
`build.sbt` を下記のように設定する。
~~~
lazy val root = (project in file(".")).
settings(
  name := "hello",
  version := "1.0",
  scalaVersion := "2.11.4"
  )
~~~

### ソースファイル
`Main.scala` を記述する。
~~~
object Main {
  def main(args: Array[String]): Unit = {
    args foreach println
  }
}
~~~

### 構築
~~~
sbt compile
~~~

### 実行
~~~
sbt run
~~~

### テスト
~~~
sbt test
~~~

### 消去
~~~
sbt clean
~~~

### 対話実行
~~~
sbt
> compile
> run
> test
> clean
~~~
