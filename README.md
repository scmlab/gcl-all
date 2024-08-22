# gcl-vscode
**gcl-vscode** is a formal verification environment for **Guarded Command Language (GCL)** that extends vscode with the **GCL Language Server**.

For more information, see also:
- [Guarded Command Language](https://en.wikipedia.org/wiki/Guarded_Command_Language)
- [Hoare Logic](https://en.wikipedia.org/wiki/Hoare_logic)
- [GCL Language Server](https://github.com/scmlab/gcl)
- [old homepage](https://scmlab.github.io/guabao/)

## Standard Language Features
- Syntax highlighting on the three languages involved:
    1. the implementation language **GCL**,
    2. the specification language, and
    3. the proof language
- Go to definition (`F12`)
- Type hints on hovers

## Verificaiton-Specific Features
- **Syntax Check** and **Typecheck** on opening `.gcl` files.
- Commands
    - **Reload** (`ctrl+c ctrl+l`) typechecks the code and show hints about the specifications and proof obligations.
    - **Refine** (`ctrl+c ctrl+r`): (when a specification is being focused by the cursor) typechecks the code in the specification and updates the specifications and proof obligations. 
- **Status Panel**, to the right of the main editor, lists all the *specifications, proof obligations, warnings* and *errors*.
- **Inline Hints**, around every specification, displays its pre- and post- conditions.

