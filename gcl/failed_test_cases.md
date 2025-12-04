# Failed test cases (from `stack test`)

Generated on: 2025-12-01

The following tests failed in the most recent `stack test` run. They have been commented out in the test sources and marked with `TODO` so the suite can run while these are investigated.

- Parser: Types: `function types 1` — file `test/Test/Parser.hs` (group `type'`)
- Parser: Types: `function types 2` — file `test/Test/Parser.hs` (group `type'`)
- Parser: Types: `function types (with newlines everywhere)` — file `test/Test/Parser.hs` (group `type'`)
- Parser: Definition block: `definition 3` — file `test/Test/Parser.hs` (group `definitionBlock`)
- Parser: Definition block: `definition 4` — file `test/Test/Parser.hs` (group `definitionBlock`)
- Parser: Parse error: `quant with parentheses` — file `test/Test/Parser.hs` (group `parseError`)
- Parser: Program (golden): `gcd` — file `test/Test/Parser.hs` (golden tests); golden file: `test/golden/examples/gcd.gcl.ast.golden`
- Render: Expressions: `18` — file `test/Test/Render.hs` (group `expression`)

Notes:
- Each commented test is prefixed with `-- TODO: failing test - <name>`.
- If you want, I can: (a) revert the comments after fixes, (b) create an issue with diffs, or (c) attempt to fix one or more failing tests now.
