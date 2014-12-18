haskell-fun
===========

Haskell samples for me


`cabal`
-------

`Haskell` 用のパッケージツール

### 一般的な利用

#### 更新
全パッケージの更新
~~~
cabal update
~~~

#### `cabal` 自身の更新

~~~
cabal install cabal-install
~~~

Windows だと、`C:\Users\(ユーザ名)\AppData\Roaming\cabal\bin` にインストールされているので、パスを通して、そちらを使うようにする。

#### インストール
hogehoge パッケージのインストール
~~~
cabal install hogehoge
~~~

#### 情報
hogehoge パッケージの情報
~~~
cabal info hogehoge
~~~

#### 一覧
~~~
ghc-pkg list
~~~

### パッケージを作成

#### 作成
~~~
cabal init
~~~
`.cabal` が作られる。

#### インストール
`.cabal` のあるディレクトリで
~~~
cabal install
~~~

### 参考文献

1. [本物のプログラマはHaskellを使う 第56回 Cabalを使ってパッケージを作成する](http://itpro.nikkeibp.co.jp/article/COLUMN/20121106/435201/)
