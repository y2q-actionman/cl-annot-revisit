(This README is under writing.)

# Abstract

cl-annot-revisit is a re-implementation of [cl-annot](https://github.com/m2ym/cl-annot), authored by Tomohiro Matsuyama.

The Motivation why I implemented again is described in [this article (Japanese)](http://y2q-actionman.hatenablog.com/entry/2019/12/20/cl-annot_%E3%82%92%E5%86%8D%E5%AE%9F%E8%A3%85%E3%81%97%E3%81%A6_cl-annot-revisit_%E3%82%92%E4%BD%9C%E3%81%A3%E3%81%9F).

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
