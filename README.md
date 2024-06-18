# Front-end of Guabao
Guabao is a formal verification environment for **Guarded Command Language (GCL)**, available as a language extension on vscode marketplace.

The source code of Guabao consists of a front-end, i.e., the vscode language extension, and a back-end, i.e., the GCL language server.

This repository is of the front-end of Guabao.
For more information, also see:
- Guabao Homepage: https://scmlab.github.io/guabao/
- Backend Code Repository: https://github.com/scmlab/gcl

## Standard Language Features
- Syntax highlighting on the three languages involved:
    1. the implementation language **GCL**,
    2. the specification language, and
    3. the proof language
- Go to definition (`F12`)
- Type hints on hovers

## Verificaiton-Specific Features
- **Status Panel**, to the right of the main editor, lists all the *specifications, proof obligations, warnings* and *errors*.
- Commands
    - **Reload** (`ctrl+c ctrl+l`) typechecks the code and updates the status panel accordingly.
    - **Refine** (`ctrl+c ctrl+r`): typechecks the code in a targeted specification and updates the status panel accordingly, if this specification is being focused by the cursor

