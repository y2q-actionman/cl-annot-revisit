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

The `export` and `optimize` macros rewrite the `defun` form to below:

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

## `eval-when` shorthands

### [Macro] `cl-annot-revisit:eval-always` *&body body*

Just a shorthand of `(eval-when (:compile-toplevel :load-toplevel :execute) ...)`.

```common-lisp
(cl-annot-revisit:eval-always
  (defun foo ()))
```

It is equivalent to:

```common-lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foo ()))
```

### [Macro] `cl-annot-revisit:eval-when-compile` *&body body*

Just a shorthand of `(eval-when (:compile-toplevel) ...)`

### [Macro] `cl-annot-revisit:eval-when-load` *&body body*

Just a shorthand of `(eval-when (:load-toplevel) ...)`

### [Macro] `cl-annot-revisit:eval-when-execute` *&body body*

Just a shorthand of `(eval-when (:execute) ...)`

## Adding Declarations

### [Macro] `cl-annot-revisit:declaration` *((&rest names))*
		
Just a shorthand of `(declaim (declaration ...))`.

```common-lisp
(cl-annot-revisit:declaration (hoge fuga))
```

It is equivalent to:

```common-lisp
(declaim (declaration hoge fuga))
```

### [Macro] `cl-annot-revisit:ignore` *name-or-names &body body*

Adds `cl:ignore` declaration into the BODY.

```common-lisp
(cl-annot-revisit:ignore (x y z)
  (defun foo (x y z)
    "Hello, World!"))
```

It is equivalent to:

```common-lisp
(defun foo (x y z)
  (declare (ignore x y z))
  "Hello, World!")
```

If BODY is null, this is expanded to a quoted `(declare (ignore ...))` form, to embed declarations using `#.`.
(This feature is to follow [the original cl-annot semantics](https://github.com/m2ym/cl-annot#annotation-ignore).)

```common-lisp
(defun foo (x y z)
  #.(cl-annot-revisit:ignore (x y z)) ; same as writing (declare (ignore x y z))
  "Hello, World!")
```

### [Macro] `cl-annot-revisit:ignorable` *name-or-names &body body*

Adds `cl:ignorable` declaration into the BODY.
Check `cl-annot-revisit:ignore` to see how it works.

### [Macro] `cl-annot-revisit:dynamic-extent` *name-or-names &body body*

Adds `cl:dynamic-extent` declaration into the BODY.
Check `cl-annot-revisit:ignore` to see how it works.

### [Macro] `cl-annot-revisit:special` *&optional vars-or-form &body body*

Adds `special` declaration or proclamation into BODY. This macro has three syntaxes.

1. If the first arg is a variable name or a list of names and BODY is not null, it adds a `declare`.

```common-lisp
(cl-annot-revisit:special *x*
  (defun foo (*x*) 100))
```

It is equivalent to

```common-lisp
(defun foo (*x*)
  (declare (special *x*))
  100)
```

2. If the first arg is not names, it tries to add `declaim`.

```common-lisp
(cl-annot-revisit:special
  (defvar *x* 1)
  (defvar *y* 2)
  (defun foo (x) 100))
```

It is equivalent to

```common-lisp
(progn (declaim (special *x*))
       (defvar *x* 1)
       (declaim (special *y*))
       (defvar *y* 2)
       (defun foo (x) 100))
```


3. If the first arg is a name or a list of names and BODY is null, it is expanded to `declaim` and quoted `declare` form.

```common-lisp
(cl-annot-revisit:special (*x* *y*))
```

is expanded to

```common-lisp
(progn (declaim (special *x* *y*))
       '(declare (special *x* *y*)))
```

This works as `declaim` at toplevel and can be embed as declarations using `#.`.

```common-lisp
(defun foo (*x*)
  #.(cl-annot-revisit:special (*x*))
  100)
```

It is equivalent to

```common-lisp
(defun foo (*x*)
  (declare (special *x*))
  100)
```

### [Macro] `cl-annot-revisit:type` *typespec &optional vars-or-form &body body*

Adds `type` declaration or proclamation into BODY.
How this is expanded is described in `cl-annot-revisit:special` description.

Next example is "1. Adding a declaration" case:

```common-lisp
(cl-annot-revisit:type integer x
  (defun foo (x) 100))
```

It is equivalent to:

```common-lisp
(defun foo (x)
  (declare (type integer x))
  100)
```

### [Macro] `cl-annot-revisit:ftype` *typespec &optional vars-or-form &body body*

Adds `ftype` declaration or proclamation into BODY.
How this is expanded is described in `cl-annot-revisit:special` description.

Next example is "2. Adding a proclamation" case:

```common-lisp
(cl-annot-revisit:ftype (function (integer integer) integer)
  (defun foo (x y) (+ x y)))
```

It is equivalent to:

```common-lisp
(progn (declaim (ftype (function (integer integer) integer) foo))
       (defun foo (x y)
         (+ x y)))
```

### [Macro] `cl-annot-revisit:inline` *&optional names-or-form &body body*

Adds `inline` declaration or proclamation into BODY. This macro has two syntaxes.
How this is expanded is described in `cl-annot-revisit:special` description.

Next example is "3. Toplevel declamation" case:

```common-lisp
(cl-annot-revisit:inline (foo))
```

It is equivalent to:

```common-lisp
(progn (declaim (inline foo))
       '(declare (inline foo)))

```

### [Macro] `cl-annot-revisit:notinline` *&optional names-or-form &body body*

Adds `notinline` declaration or proclamation into BODY.
How this is expanded is described in `cl-annot-revisit:notinline` description.

### [Macro] `cl-annot-revisit:optimize` *&optional qualities &body body*

Adds `optimize` declaration or proclamation into BODY. This macro has two syntaxes.

1. If BODY is not null, it add a `declare` into BODY.

```common-lisp
(cl-annot-revisit:optimize (speed safety)
  (defun foo (x) (1+ x)))
```

It is equivalent to:

```common-lisp
(defun foo (x)
  (declare (optimize speed safety))
  (1+ x))
```

2. If BODY is null, it is expanded to `declaim` and quoted `declare`.

```common-lisp
(cl-annot-revisit:optimize ((speed 3) (safety 0) (debug 0)))
```

It is equivalent to:

```common-lisp
(progn (declaim (optimize (speed 3) (safety 0) (debug 0)))
       '(declare (optimize (speed 3) (safety 0) (debug 0))))
```

Refer `cl-annot-revisit:special` description to see why both `declaim` and `declare` appeared.

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
