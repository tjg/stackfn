# Some design notes

Based on:
- [stackfn-spec-1.pdf](./stackfn-spec-1.pdf)
- [stackfn-spec-2.pdf](./stackfn-spec-2.pdf)


## Control-flow semantics

Each parse-tree node basically looks at top-of-stack & moves to the
next node using this logic:

- **if statement**
  - if-start:
    - true → nextsibling
    - false → nextsibling(else(parent)) or nextsibling(parent)
    - empty-stack → error
  - else:
    - true,false,empty-stack → nextsibling(parent)
- **while statement**
  - while-start:
    - true → nextsibling
    - false → nextsibling(parent)
    - empty-stack → error
  - while-end:
    - true,false,empty-stack → first(parent)
- **do-while statement**
  - do-while-start
    - true,false,empty-stack → nextsibling
  - do-while-end
    - true → first(parent)
    - false → nextsibling(parent)  # After linearization, this turns out to be idx(do-while-end)+1
    - empty-stack → error
- **break statement**
  - true,false,empty-stack → nextsibling(nearest-loop-ancestor)
- **continue statement**
  - true,false,empty-stack → first(nearest-loop-ancestor)
- **(all other statements)**:
  - true,false,empty-stack → nextsibling


### Linearizing nodes

Since Clojure targets the JVM — and this is an interpreter for a
JVM-like stack language — it seemed reasonable to act vaguely similar
to Hotspot's interpreter.

So this interpreter runs in a loop, reading whatever index that PC
points to.

Leaf nodes get indexed sequentially & in depth-first order. To
calculate indexes of each node's continuation:
- nextsibling(leaf)   = idx(leaf)+1
- nextsibling(branch) = idx(last-descendant(branch))+1
- first(branch)       = idx(first-descendant(branch)
