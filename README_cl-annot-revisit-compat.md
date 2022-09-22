`cl-annot-revisit-compat` is a compatibility layer for the original cl-annot.

It has `defannotation` macro works like the original cl-annot.

# Design

cl-annot's `defannotation` has four effects.

1. If `:arity` is specified, saves it as `@symbol` syntax arity.
2. If `:inline` is specified, it enables its read-time expansion feature.
3. If `:alias` is specified, it sets aliases.
4. Defines a macro by `defmacro`.

For cl-annot-revisit, the first three features are not required.

- I encourage you to use the normal Lisp syntax, `()`. It clearly show its arity. `@(list)` syntax's arity is always 1, because it always fills the last form. `:arity` is needed only by `@symbol` syntax.
- When a kind of expansion is required at read-time, I think it should be invoked with a visible way, like `#.`. `:inline` is a unique feature because it perform read-time expansion at read-time transparently. I personally think it is confusing.
- In the cl-annot, `:alias` defined for the `CL` package symbols. I think it is used to define annotations with the same name as the `CL` symbols. My solution is simply defines another package and use a same name in it, like `cl-annot-revisit:optimize`.

In addition, I split *annotation* between the `@` syntax and `defmacro`. Therefore, they do not need to be defined at the same time.

So I dropped `defannotation` from the main package. I still keep it in this subpackage only for keeping same syntax with the cl-annot.

# Loading

``` common-lisp
(ql:quickload "cl-annot-revisit-compat")
```

or


``` common-lisp
(asdf:load-asd "cl-annot-revisit-compat.asd")
(asdf:load-system :cl-annot-revisit-compat)
```

# Usage

## [Variable] `cl-annot-revisit-at-syntax:*cl-annot-compatibility*`

If this is true, `@` reader macro enables cl-annot compatibility layer.

## [Method] `cl-annot-revisit-at-syntax:find-at-syntax-arity` *operator cl-annot-compatible-p*

For `@symbol` syntax, this function returns how many forms should be read after the operator.

Its default is 1. For some cl-annot-revisit macros (such as `cl-annot-revisit:optimize`), it returns 2.
If `*cl-annot-compatibility*` is true, this method attempts to get a value in the same way as the original cl-annot.

## [Method] `cl-annot-revisit-at-syntax:expand-at-read-time-p` *operator cl-annot-compatible-p*

For `@symbol` syntax, this function returns whether the operator should be expanded at read-time or not.

Its default is `nil`.
If `*cl-annot-compatibility*` is true, this method attempts to get a value in the same way as the original cl-annot.

## [Method] `cl-annot-revisit-at-syntax:resolve-at-syntax-alias` *operator cl-annot-compatible-p*

For `@symbol` syntax, this function is used for resolve operator's aliases to the original name.

By default, the operator itself is returned.
If `*cl-annot-compatibility*` is true, this method attempts to get a value in the same way as the original cl-annot.

## [Macro] `cl-annot-revisit-compat:defannotation` *name lambda-list &key arity inline alias*

It works work almost like `defmacro` except it overrides the above methods using the keyword parameters.
