# Plan: 將 `Loc` 遷移至 `Maybe Range`

## 目標

將 `Lexer.hs` 的 `translateLoc` 之後的所有程式碼從使用 `Data.Loc.Loc` 改為使用 `Maybe Range`（來自 `Data.Loc.Range`）。

- Lexer 層保留 `L a` 與 `Loc`（因為 token 一定有位置）
- Parser 層開始使用 `Maybe Range`
- 新增 `MaybeRanged` typeclass 處理 `Nothing` 情況（對應原本的 `NoLoc`）

## Build / Test 方法

```bash
# Build（不執行測試）
stack build --test --no-run-tests

# 執行測試
stack test
```

**注意**：過程中可忽略此錯誤訊息：`Ticker: poll failed: Interrupted system call: Interrupted system call`

## 工作流程

每個 Phase 完成後：
1. 執行 `stack build --test --no-run-tests` 確認編譯通過
2. 執行 `stack test` 確認測試通過
3. 更新下方進度追蹤（將 `[ ]` 改為 `[x]`）
4. Commit 變更：`git commit -am "Phase N - <描述>"`

## 進度追蹤

- [x] Phase 1 - 擴展 `Data.Loc.Range`
- [x] Phase 2 - 遷移 Parser Util
- [ ] Phase 3 - 遷移 Syntax.Common
- [ ] Phase 4 - 遷移 Syntax.Abstract
- [ ] Phase 5 - 遷移 Syntax.Concrete 與 Parser
- [ ] Phase 6 - 遷移下游模組

---

## Phase 1 - 擴展 `Data.Loc.Range`

**檔案**：`src/Data/Loc/Range.hs`

**新增內容**：

1. `MaybeRanged` typeclass：
   ```haskell
   class MaybeRanged a where
     maybeRangeOf :: a -> Maybe Range
   ```

2. merge 運算子 `(<->>)`：
   ```haskell
   (<->>) :: Maybe Range -> Maybe Range -> Maybe Range
   Nothing <->> x = x
   x <->> Nothing = x
   Just a <->> Just b = Just (a <> b)
   ```

3. 輔助函式（重新命名現有 `fromLoc` 為更清晰的名稱）：
   ```haskell
   toMaybeRange :: Loc -> Maybe Range
   toMaybeRange NoLoc = Nothing
   toMaybeRange (Loc x y) = Just (mkRange x y)
   ```

---

## Phase 2 - 遷移 Parser Util

**檔案**：`src/Syntax/Parser/Util.hs`

**修改內容**：

1. 將 `Bookkeeping` 中的：
   - `currentLoc :: Loc` 改為 `currentRange :: Maybe Range`
   - `logged :: Map ID Loc` 改為 `logged :: Map ID (Maybe Range)`

2. 新增：
   - `getMaybeRange :: Parser a -> Parser (a, Maybe Range)`
   - `withMaybeRange :: Parser (Maybe Range -> a) -> Parser a`

3. 保留 `getRange`/`withRange` 給必須有位置的情況（內部呼叫 `getMaybeRange` 後處理 `Nothing`）

---

## Phase 3 - 遷移 Syntax.Common

**檔案**：
- `src/Syntax/Common/Types.hs`
- `src/Syntax/Common/Instances/Located.hs`

**修改內容**：

1. 將以下類型的 `Loc` 欄位改為 `Maybe Range`：
   - `Name`
   - `ArithOp`（所有變體）
   - `ChainOp`（所有變體）
   - `TypeOp`

2. 更新 `Located` 實例為 `MaybeRanged` 實例

3. 將 `NoLoc` 用法改為 `Nothing`，`<-->` 改為 `<->>`

---

## Phase 4 - 遷移 Syntax.Abstract

**檔案**：
- `src/Syntax/Abstract/Types.hs`
- `src/Syntax/Abstract/Instances/Located.hs`
- `src/Syntax/Abstract/Operator.hs`

**修改內容**：

1. 將以下類型的 `Loc` 欄位改為 `Maybe Range`：
   - `Program`
   - `Definition`
   - `Declaration`
   - `Stmt`
   - `GdCmd`
   - `Kind`
   - `Interval`
   - `Type`
   - `Expr`
   - `Chain`

2. 更新 `Located` 實例為 `MaybeRanged` 實例

3. 調整 `Syntax.Abstract.Operator` 中的 `NoLoc` 改為 `Nothing`

---

## Phase 5 - 遷移 Syntax.Concrete 與 Parser

**檔案**：
- `src/Syntax/Concrete/Types.hs`
- `src/Syntax/Concrete/Instances/Located.hs`
- `src/Syntax/Concrete/Instances/ToAbstract.hs`
- `src/Syntax/Parser.hs`

**修改內容**：

1. 更新 `Syntax.Concrete.Types` 位置欄位

2. 修改 `Syntax.Parser` 使用新的 `withMaybeRange`

3. 調整 `ToAbstract` 使用 `MaybeRanged` 與 `<->>` 運算子

---

## Phase 6 - 遷移下游模組

**檔案**：
- `src/Server/*`（`SrcLoc.hs`、`Hover.hs`、`GoToDefn.hs` 等）
- `src/GCL/*`（`Predicate.hs`、`WP/` 等）
- `src/Pretty/*`
- `src/Render/*`
- `src/Syntax/Typed/*`

**修改內容**：

1. 將 `locOf` 改為 `maybeRangeOf`

2. 移除 `fromLoc` 呼叫（直接使用 `Maybe Range`）

3. 處理 `Nothing` 情況（原本的 `NoLoc`）

---

## 設計決策

### 1. Lexer 層的 `L a` 保留

保留 `L a`（使用 `Loc`）於 Lexer/TokenStream 層，因為 token 一定有位置。Parser 輸出的 AST 改用 `Maybe Range`。

### 2. `<->>` 運算子語意

```haskell
Nothing <->> x       = x
x       <->> Nothing = x
Just a  <->> Just b  = Just (a <> b)
```

### 3. `Ranged` 與 `MaybeRanged` 並存

- 保留 `Ranged`（回傳 `Range`）給一定有位置的情況
- 新增 `MaybeRanged`（回傳 `Maybe Range`）給可能沒位置的情況
