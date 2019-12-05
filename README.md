# Abstract

cl-annot-revisit is a re-implementation of [cl-annot](https://github.com/m2ym/cl-annot), authored by Tomohiro Matsuyama.

元の実装と異なる点:

- [構文乗っ取りマクロ](https://g000001.cddddr.org/3756404769) を明示して使えるようにする。
- 上で定義したマクロへの expander として '@' reader macro を定義する。

思いつき

- @defannotation で defmacro を囲えば @ reader macro にできる
- 無限引数 annotation を作れば、ファイル全体に作用できる(ひどい)
- inline expansion なんて、 macro でサポートする必要ない。勝手に `#.` 置けばいいじゃない。
  at-syntax ではサポートしてもよいが。
- @required 系は https://lisp-journey.gitlab.io/blog/how-to-check-slots-types-at-make-instance/

困った点

- @ reader macro と @ normal macro は共存できない。自分でリーダいじってもらわないと。
- compat な annotation (例えば inline 入り) と、通常の annotation の使い分けは、どの @ 系 symbol を use-package するかに依存
- ・・すると思っていたが、 @ で別package の symbol を参照できないと不便な気がする。

# cl-annot user memo

version: 2018-12-10

## cl-annot


## cl-annot-prove (by Rudolph Miller)

- Uses `@doc`.
- Defines `@tests`, `@tests.around`, `@tests.before`, `@tests.after`, `@tests.around.each`, `@tests.before.each`, `@tests.after.each`.
- No inline definition.

## cl-flowd (by Mike Maul)

- Uses `@export`, `@export-class`, `@inline` (top-level usage).

## cl-influxdb (by Mike Maul)

- Uses `@export`, `@export-class`, `@export-structure`.

## cl-locale (by Eitarow Fukamachi)

- Uses `@export`.

## cl-oclapi (by gos-k)

- Uses `@export`.

## cl-pattern (by Tomohiro Matsuyama)

- Uses `@eval-always`, `@export`, `@export-accessors`.

## cl-syntax (by Tomohiro Matsuyama)

only dependency.

## cl-tasukete (by gos-k)

- Uses `@export`, `@export-class`.

## clache (by Tomohiro Matsuyama)

- Uses `@export`, `@annotation`, `@type` (in comment), `@ignore`, `@annot.slot:required` (this is inline!).
- Defines `@cache`.
- No inline definition.

## elb-log (by Rudolph Miller)

- Uses `@doc`, `@export-structure`, and `@tests`, `@tests.around` in **cl-annot-prove**.

## glisph (by Tamamu)

- Uses `@export`, `@type` and `@optimize` (in *declare* usage).

## jonathan (by Rudolph Miller)

- Uses `@doc`.

## lucerne (by Fernando Borretti)

- Defines `@route`.
- No inline definition.

## multival-plist (by Eitarow Fukamachi)

- Uses `@export`.

## thread.comm.rendezvous (by Kazuo Koga)

- Uses `@export`.

## zenekindarl (by κeen)

- Uses `@export`, `@export-accessors`, `@export-constructors` in nested style, and `@ignore` (this is inline!).
