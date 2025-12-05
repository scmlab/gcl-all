# Plan: 移除 Data.Loc，translateLoc 後使用 Range

將 Lexer 的 `translateLoc` 從輸出 `L Tok` (包含 `Loc`) 改為輸出 `R Tok` (包含 `Range`)，
然後更新所有下游使用者，最終移除 `Data.Loc.Range` 對 `Data.Loc` 的 re-export。

## 背景

- `lexer-applicative` 的 `runLexer` 永遠不會產生 `NoLoc`（已驗證源碼）
- 因此 `translateLoc` 可以安全地輸出 `Range` 而非 `Loc`
- 參考 [DEVELOPMENT.md](DEVELOPMENT.md): 每個 phase 後執行 build + test

---

## Phase 1: Lexer.hs - 改用 R Tok ✅ COMPLETED

### Step 1.1: 修改 Lexer.hs
在 [Lexer.hs](src/Syntax/Parser/Lexer.hs)：
- [x] 將 `TokStream` 從 `TokenStream (L Tok)` 改為 `TokenStream (R Tok)`
- [x] 修改 `translateLoc` 使用 `R` 和 `Range`（使用 `fromInclusiveLoc`）

---

## Phase 2: TokenStream.hs - 更新 Stream instance ✅ COMPLETED

在 [TokenStream.hs](src/Syntax/Parser/TokenStream.hs)：
- [x] 將 `Stream (TokenStream (L tok))` 改為 `Stream (TokenStream (R tok))`
- [x] 更新 `VisualStream` instance
- [x] 更新 `toChunk` 處理 `R Range tok` 而非 `L Loc tok`

---

## Phase 3: Parser 層更新 ✅ COMPLETED

### Util.hs
- [x] 更新 `Bookkeeping` 使用 `Maybe Range` 而非 `Loc`
- [x] 更新 `symbol` 函數返回 `Range`
- [x] 更新所有 `L Tok` 改為 `R Tok`

### Parser.hs
- [x] 更新 `parse` 函數的錯誤處理
- [x] 更新 `parseWithTokList` 使用 `R Tok`
- [x] 更新 `chainOp`, `arithOp`, `typeOp` 使用 `Range`
- [x] 更新 `adapt` 函數使用 `Range`

### Concrete/Types.hs
- [x] 更新 `Spec` constructor 從 `[L Tok]` 改為 `[R Tok]`

### Pretty/Util.hs
- [x] 新增 `PrettyWithLoc (R a)` instance

---

## Phase 4: Refine.hs - 更新 Token 操作 ✅ COMPLETED

在 [Refine.hs](src/Server/Handler/GCL/Refine.hs)：
- [x] 更新 import 使用 `R`, `Range`, `mkRange`
- [x] 更新 `translateTokStream` 使用 `R` 和 `Range`
- [x] 更新 `translateTokenRange` 函數
- [x] 簡化錯誤處理（不再需要 `toMaybeRange`）

---

## Phase 5: Range.hs - 保留必要的 re-exports ⚠️ PARTIALLY COMPLETED

**注意**: 經過分析，`L`, `Loc`, `Located` 等 re-exports 仍然需要保留，因為：
1. `L Pred` 仍在 `GCL/Predicate.hs` 中使用
2. `Located` typeclass 在許多地方使用 `locOf`
3. 其他非 Token 相關的地方仍使用 `L` 包裝位置資訊

在 [Range.hs](src/Data/Loc/Range.hs)：
- [x] 保留 `Pos` 相關函數
- [x] 保留 `L(..)`, `unLoc`, `Loc(..)`, `Located(..)`, `(<-->)` 供其他用途

---

## Phase 6: 清理與驗證 ✅ COMPLETED

- [x] 執行 `stack build --fast` - SUCCESS
- [x] 執行 `stack test` - ALL 202 TESTS PASSED

---

## 結果摘要

**主要改動**:
- Token stream 層從 `L Tok` 成功遷移到 `R Tok`
- `translateLoc` 現在使用 `fromInclusiveLoc` 產生 `Range`
- Parser 層全面使用 `R Tok` 和 `Range`

**保留的向後相容**:
- `Data.Loc.Range` 仍然 re-export `L`, `Loc`, `Located` 等，因為其他部分（如 `GCL/Predicate.hs`）仍然使用這些類型

## 注意事項

1. **Lexer 不產生 NoLoc**: 已驗證 `lexer-applicative` 源碼，`runLexer` 始終使用 `Loc pos1 pos2`
2. **R vs L**: `R Range a` 強制有效範圍，`L Loc a` 允許 `NoLoc`
3. **向後相容**: `L`, `Loc`, `Located` 等仍然保留給非 Token 用途
