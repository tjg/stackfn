# stackfn

A stack-based language embedded in Clojure. With features to help
visualize the execution.

Mainly just a sandbox to practice & explore certain parts of Clojure &
the JVM.


## Table of contents:

1. [Example execution](#example-execution)
2. [Spies](#spies)
3. [Functionality](#functionality)
4. [Running unit tests](#running-unit-tests)


## Example execution

Let's see the Fibonacci example in [smoke_test.clj](test/stackfn_sandbox/stack/smoke_test.clj##L149),
which was coded gratuituously to exercise some extra interpreter features:

```clojure
stackfn-sandbox.stack.smoke-test> (spies/with-spy
                                    (fibonacci an-adder 2))
1

| :pc   | :stack     | :env                                                | :instr                   | :instr-type     |
|-------+------------+-----------------------------------------------------+--------------------------+-----------------|
|  0/47 | []         | {!adder #<+>, !n 2}                                 |                          |                 |
|  1/47 | [0]        | {!adder #<+>, !n 2}                                 | 0                        | :constant       |
|  2/47 | [0]        | {!adder #<+>, !n 2, !a 0}                           | !a+                      | :assign-var     |
|  3/47 | [0 1]      | {!adder #<+>, !n 2, !a 0}                           | 1                        | :constant       |
|  4/47 | [0 1]      | {!adder #<+>, !n 2, !a 0, !b 1}                     | !b+                      | :assign-var     |
|  5/47 | [0]        | {!adder #<+>, !n 2, !a 0, !b 1}                     | <pop>                    | :pop            |
|  6/47 | []         | {!adder #<+>, !n 2, !a 0, !b 1}                     | <pop>                    | :pop            |
|  7/47 | [2]        | {!adder #<+>, !n 2, !a 0, !b 1}                     | !n                       | :var            |
|  8/47 | [false]    | {!adder #<+>, !n 2, !a 0, !b 1}                     | (invoke> zero? 1)        | :invoke         |
| 11/47 | []         | {!adder #<+>, !n 2, !a 0, !b 1}                     | if> → 11?                | :jump-false     |
| 12/47 | [2]        | {!adder #<+>, !n 2, !a 0, !b 1}                     | !n                       | :var            |
| 13/47 | [false]    | {!adder #<+>, !n 2, !a 0, !b 1}                     | (invoke> eq1? 1)         | :invoke         |
| 16/47 | []         | {!adder #<+>, !n 2, !a 0, !b 1}                     | if> → 16?                | :jump-false     |
| 17/47 | [2]        | {!adder #<+>, !n 2, !a 0, !b 1}                     | !n                       | :var            |
| 18/47 | [2 2]      | {!adder #<+>, !n 2, !a 0, !b 1}                     | 2                        | :constant       |
| 19/47 | [2 2]      | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | !counter+                | :assign-var     |
| 20/47 | [true]     | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | (invoke> <= 2)           | :invoke         |
| 21/47 | []         | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | while> → 46?             | :jump-false     |
| 22/47 | [0]        | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | !a                       | :var            |
| 23/47 | [0 1]      | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | !b                       | :var            |
| 24/47 | [0 1 #<+>] | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | !adder                   | :var            |
| 25/47 | [1]        | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2}         | (invoke-method> add 3)   | :invoke-virtual |
| 26/47 | [1]        | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2, !acc 1} | !acc+                    | :assign-var     |
| 27/47 | []         | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2, !acc 1} | <pop>                    | :pop            |
| 28/47 | [1]        | {!adder #<+>, !n 2, !a 0, !b 1, !counter 2, !acc 1} | !b                       | :var            |
| 29/47 | [1]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | !a+                      | :assign-var     |
| 30/47 | [1 1]      | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | !acc                     | :var            |
| 31/47 | [1 1]      | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | !b+                      | :assign-var     |
| 32/47 | [1]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | <pop>                    | :pop            |
| 33/47 | []         | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | <pop>                    | :pop            |
| 34/47 | [2]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | !counter                 | :var            |
| 35/47 | [3]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 2, !acc 1} | (invoke> my-increment 1) | :invoke         |
| 36/47 | [3]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | !counter+                | :assign-var     |
| 37/47 | []         | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | <pop>                    | :pop            |
| 38/47 | [2]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | !n                       | :var            |
| 39/47 | [2 3]      | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | !counter                 | :var            |
| 40/47 | [false]    | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | (invoke> <= 2)           | :invoke         |
| 44/47 | []         | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | if> → 44?                | :jump-false     |
| 46/47 | []         | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | <break> → 46             | :jump           |
| 47/47 | [1]        | {!adder #<+>, !n 2, !a 1, !b 1, !counter 3, !acc 1} | !acc                     | :var            |
```

## Spies

> Show me your flowcharts and conceal your tables, and I shall be
> continued to be mystified. Show me your tables, and I won’t usually
> need your flowcharts; they’ll be obvious.
>
> — Fred Brooks

*This is neither part of the language nor interpreter specification.
It's intended only as a visualization/debugging aid and may be removed
or face breaking changes.*

The interpreter loop can update a reference with state updates. A "spy" is
simply a function watching that reference via `add-watch`.

It can help:
- people understand the interpreter's operation
- debugging
- perform absurdities with continuations (in cahoots with `invoke>` or `invoke-method>`)
- gather stats

Example with a somewhat recursive stackfn call:

```clojure
(ns consumer.debug
  (:require [stackfn-sandbox.stack.spies :as spies]
            [stackfn-sandbox.stack.interpreter :refer [stackfn defstackfn]]
            [stackfn-sandbox.stack.vm :refer [make-spy-reference *spy*]]))

(let [spy-reference (make-spy-reference)]
  (spies/spy-on-interpreter spy-reference)
  (binding [*spy* spy-reference]
    ((stackfn foo
       ([] :ok (invoke> foo 1))
       ([!a] !a)))))

=> :ok

| :pc | :stack | :env     | :instr | :instr-type |
|-----+--------+----------+--------+-------------|
| 0/1 | []     | {!a :ok} |        |             |
| 1/1 | [:ok]  | {!a :ok} | !a     | :var        |

| :pc | :stack | :env | :instr          | :instr-type |
|-----+--------+------+-----------------+-------------|
| 0/2 | []     | {}   |                 |             |
| 1/2 | [:ok]  | {}   | :ok             | :constant   |
| 2/2 | [:ok]  | {}   | (invoke> foo 1) | :invoke     |
```

## Functionality

A more rigorous description of control-flow semantics [here](./doc/intro.md#control-flow-semantics).

Within Clojure:

| Clojure op/var | Description                                                                              |
| -------------- | ---------------------------------------------------------------------------------------- |
| `defstackfn`   | Named function like defn: accesses lexical closures, multi-arity, varargs                |
| `stackfn`      | Anon function like fn: optionally named, accesses lexical closures, multi-arity, varargs |

Within a stack function:

| Stack fn form    | Category      | Description                                                                  |
| ---------------- | ------------- | ---------------------------------------------------------------------------- |
| *const*          | constant      | Unevaluated form. Restriction: can't be a bare symbol nor list. [1]          |
| *!var*           | variable      | Can't be used before assigned. Indefinite extent, lexical scope. [2]         |
| *!var+*          | assignment    | Sets *!var* & shadows all other same-named *!var* in same scope/extent. [2]  |
| `<pop>`          | stack control | Pops top item off the stack. [3]                                             |
| `if>`            | branch        | If top-of-stack truthy, takes `if>` branch. Otherwise `else>` branch. [4]    |
| `while>`         | loop          | If top-of-stack truthy, proceeds with loop. Otherwise exits it.              |
| `do-while>`      | loop          | Like `while>`, except tests top-of-stack at end of loop.                     |
| `<break>`        | loop control  | Exits lexically-innermost enclosing loop, going right past its end. [5]      |
| `<continue>`     | loop control  | Goes to the start of lexically-innermost enclosing loop. [5]                 |
| `stackfn>`       | stackfn       | Anon stackfn: optionally named, lexical closures, multi-arity, varargs. [6]  |
| `invoke>`        | invoke        | Invokes function on zero or more items on stack. [7]                         |
| `invoke-method>` | invoke        | Invokes virtual method on an object on the stack, and optional further args. |

**Footnotes:**

1. The restriction against bare symbols is to avoid bugs like using
   `if` instead of `if>`, and to reserve future symbols
   without breaking otherwise legal programs. (Similar for lists,
   which are also reserved for future expansion.)

2. Variables are visible throughout their immediately enclosing
   stackfn, once declared in it. See footnote [6] for information on
   their visibility in nested lexical scopes (made by `stackfn>`).

3. You can `<pop>` an empty stack. This differs from other forms which
   pop the stack, because they also read it. (So they generate errors
   on empty stacks.)

4. The `else>` is optional. If omitted, and the top-of-stack test is
   falsey, then control goes right after the end of `if>` branch.

5. `<break>` and `<continue>` must be lexically enclosed in a loop
   (though they may be buried deeper in intervening `if>` branch levels).
   Otherwise there'll be a syntax error when declaring the stack
   function.

6. `stackfn>`'s syntax is just like the Clojure-level `stackfn`. It
   creates a function like any other, so it can be passed outside the
   enclosing stackfn where it was created, invoked in regular Clojure
   or by `invoke>`, etc.

   Once declared with `stackfn>`, stackfns get an immutable lexical
   env from their surrounding context. (i.e. later declarations nor
   assigments in either env won't affect the other env.) Anyone
   wanting mutability can use reference types, like atoms.

7. `invoke>` supports two arities:

   - `(invoke> <argcount>)`: Invokes top-of-stack as a function,
     applying it to the top *argcount - 1* items below it on the
     stack. (So *argcount* must be at least 1.)

   - `(invoke> <fn-name> <argcount>)`: Applies the function named by
     *fn-name* to *argcount* items on top of stack. (So *argcount* can
     be 0 or more.) *fn-name* is either a (Clojure) local variable or var.

## Running unit tests

At the commandline, choose either:

- `clj -X:test` (Ensure you're in the project's root dir.)
- `lein test`
