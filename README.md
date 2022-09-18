(This README is under writing.)

# Abstract

cl-annot-revisit is a re-implementation of [cl-annot](https://github.com/m2ym/cl-annot), an annotation library for Common Lisp.

My main motivation for implementing it again is to split its concept into two parts:

1. Normal `defmacro`s acting like `export`, `doc`, etc. in cl-annot. Conceptually, form overriding and rewriting can be implemented just with `defmacro`.
2. `@` reader macro which just wraps forms with `()`, like `@foo bar` => `(foo bar)`.

For instance, consider this example:

``` common-lisp
(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

@cl-annot-revisit:export
@(cl-annot-revisit:optimize ((speed 3) (safety 0)))
(defun foo ()
  "Hello, World!")
```

`@` reader macro expand it to a nested form:

``` common-lisp
(cl-annot-revisit:export
  (cl-annot-revisit:optimize ((speed 3) (safety 0))
     (defun foo ()
       "Hello, World!")))
```

My `export` and `optimize` macros rewrite the `defun` form to below:

``` common-lisp
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(foo)))                    ; by `cl-annot-revisit:export'
  (defun foo ()
    (declare (optimize (speed 3) (safety 0))) ; by `cl-annot-revisit:optimize'
    "Hello, World!"))
```

Other motiviations are:

- Fix many bugs of cl-annot. (TODO: make a link)
- Show the funny *infinite* annotation I found. See [`#@` syntax](#infinite-annotation) below.

These are described in [this article (Japanese)](http://y2q-actionman.hatenablog.com/entry/2019/12/20/cl-annot_%E3%82%92%E5%86%8D%E5%AE%9F%E8%A3%85%E3%81%97%E3%81%A6_cl-annot-revisit_%E3%82%92%E4%BD%9C%E3%81%A3%E3%81%9F) also.

# Before Using This...

I encourage you to read the following articles;

- Comments in [Reader Macros | Common Lisp - Bad Examples](https://www.reddit.com/r/Common_Lisp/comments/556mpn/reader_macros_common_lisp_bad_examples/), discussing this kind of reader macro.
- [Why I don't like eval-always](http://random-state.net/log/3387124996.html) and [I Still Don't Like EVAL-ALWAYS](http://random-state.net/log/3387296853.html) by Nikodemus Siivola.

Please consider these alternatives:

- The `nest` macro, introduced in [A tale of many nests](https://fare.livejournal.com/189741.html) by @fare, to flatten nested macros.
- [How to Check Slots Types at make-instance](https://lisp-journey.gitlab.io/blog/how-to-check-slots-types-at-make-instance/), to make CLOS slots "optional" or "required".
- Simply enclose your forms with `()`, instead of `@` macro.

# Loading

cl-annot-revisit is not Quicklisp-ready now. 

At this time, clone this repository, locate it into
`~/quicklisp/local-projects/`, and:

``` common-lisp
(ql:quickload "cl-annot-revisit")
```

This library depends following libraries:

- alexandria
- named-readtables

# Usage of macros

(stub)

TODO: nesting

## `eval-when` shorthands

- `eval-always`

- `eval-when-compile`
- `eval-when-load`
- `eval-when-execute`

## Declarations

- `special`
- `type`
- `ftype`
- `inline`
- `notinline`
		
- `ignore`
- `ignorable`
- `dynamic-extent`
- `optimize`
   
- `declaration`

## Documentation

- `documentation`
- `doc`

## Export

- `export`

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

## `defclass`

- `metaclass`
- `export-slots`
- `export-accessors`
- `export-class`

## defclass slots

- `optional`
- `required`

## `defstruct`

- `export-accessors` (same as above)
- `export-constructors`
- `export-structure`


# Usage of '@' syntax

(stub)

- `at-syntax-readtable`
- `find-at-syntax-arity`

## @(list) syntax

## @symbol syntax

`cl-annot-revisit-at-syntax:find-at-syntax-arity`

``` common-lisp
;; Enables @ syntax
(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

;; This is same as the nested example above.
@cl-annot-revisit:documentation "docstring"
@cl-annot-revisit:export
(defun foo () t)
```

## #n@(list) and #n@symbol syntax

## *infinite* annotation ##

#@(list) and #@symbol

# License 

```
Copyright © 2021-2022 YOKOTA Yuki <y2q-actionman@users.noreply.github.com>

This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To Public License, Version 2,
as published by Sam Hocevar. See the COPYING file for more details.
```
