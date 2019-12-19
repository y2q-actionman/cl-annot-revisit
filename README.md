(This README is under writing.)

# Abstract

cl-annot-revisit is a re-implementation of [cl-annot](https://github.com/m2ym/cl-annot), authored by Tomohiro Matsuyama.

The Motivation why I implemented again is described in [this article (Japanese)]().

# Overview

This implementation splits the '@' syntax of cl-annot to the two potions:

- Normal `defmacro`-s acting like `export`, `doc`, etc in cl-annot.
- `@` reader macro, which expands `@foo (bar baz)` to `(foo bar baz)`.

And:

- Fixes many bugs [commented in reddit](https://www.reddit.com/r/Common_Lisp/comments/556mpn/reader_macros_common_lisp_bad_examples/)
- `#n@` reader macro, which can specify '@' syntax arity by its infix parameter.

# Examples

## Loading

Clone this repository, locate it into `~/quicklisp/local-projects/`, and:

``` common-lisp
(ql:quickload "cl-annot-revisit")
```

## Using without '@' syntax

`defun` and export its name.

``` common-lisp
(cl-annot-revisit:export
  (defun foo () t))
```

And, adding a docstring

``` common-lisp
(cl-annot-revisit:documentation "docstring"
   (cl-annot-revisit:export
     (defun foo () t)))
```

## Using with '@' syntax

``` common-lisp
;; Enables @ syntax
(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

;; This is same as the nested example above.
@cl-annot-revisit:documentation "docstring"
@cl-annot-revisit:export
(defun foo () t)
```

# License 

WTFPL


<!-- # Memo -->

<!-- - 無限引数 annotation を作れば、ファイル全体に作用できる(ひどい) -->
<!-- - inline expansion なんて、 macro でサポートする必要ない。勝手に `#.` 置けばいいじゃない。 -->
<!--   at-syntax ではサポートしてもよいが。 -->

<!-- - @required 系は https://lisp-journey.gitlab.io/blog/how-to-check-slots-types-at-make-instance/ -->

<!-- - @ reader macro と @ normal macro は共存できない。自分でリーダいじってもらわないと。 -->
