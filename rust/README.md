rust-fun
========

Rust samples for me


## Cargo

Cargo は、Rust 用のパッケージマネージャです。

できることは
* あたらしいプロジェクトをつくる
* プロジェクトをビルドする
* プロジェクトをテストする
* プロジェクトのドキュメントをつくる

### あたらしいプロジェクトをつくる

```
cargo new hello-project --bin
```

上記のコマンドをコマンドラインから実行すると下記のカレントディレクトリにプロジェクトのフォルダが作られます。

* hello-project/
  * .gitignore
  * Cargo.toml
  * src/
    * main.rs

中身はこんな感じなので、とりあえず main.rs にプログラムを記述していきましょう。

### Cargo.toml

Cargo の設定ファイルです。

```
[package]

name = "hello-project"
version = "0.0.1"
authors = ["Shinya Kitaoka <skitaoka@gmail.com>"]
```

git で設定している名前とメールアドレスが自動で authors に設定されています。

git へのユーザ名とメールアドレスの設定は、こんな感じ。

```
git config --global user.name "Shinya Kitaoka"
git config --global user.email "skitaoka@gmail.com"
```

### main.rs

デフォルトで生成されるファイル。 Hello World!

```
fn main() {
  println!("Hello, workd!")
}
```

### プロジェクトをビルドする

プロジェクトのディレクトリに移動して、
```
cargo build
```
するとビルドされます。

ビルドできると、下記のファイルが生成されます。

* Cargo.lock --- ビルドの設定ファイル？ (自分で編集はしない)
* target/
  * hello-project.exe  --- 成果物
  * deps/
  * native/
