# Abstract

cl-annot-revisit is a re-implementation of [cl-annot](https://github.com/m2ym/cl-annot), authored by Tomohiro Matsuyama.

元の実装と異なる点:

- [構文乗っ取りマクロ](https://g000001.cddddr.org/3756404769) を明示して使えるようにする。
- 上で定義したマクロへの expander として '@' reader macro を定義する。

思いつき

- @defannotation で defmacro を囲えば @ reader macro にできる
