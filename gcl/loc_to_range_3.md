# Plan: 完全移除 `Data.Loc`，全面改用 `Range`

## TL;DR

前兩階段（loc_to_range.md 和 loc_to_range_cont.md）已完成大部分遷移，但仍有 20 個檔案使用 `Data.Loc`。本計畫將完全移除對 `Data.Loc` 的依賴（`Data.Loc.Inclusive` 例外）。

---

## 目標

**完全停止使用 `Data.Loc`**，改用 `Data.Loc.Range` 中的 `Range` / `Maybe Range`。

### 例外（可以繼續使用）：
- `Data.Loc.Inclusive` - lexer 底層使用，由 vendor 的 `lexer-applicative` 產生

### 最終目標：
- 所有程式碼不再 `import Data.Loc`
- 不再使用 `Loc`、`NoLoc`、`L a`、`unLoc`、`<-->`、`locOf`
- 完全改用 `Range`、`Maybe Range`、`R a`、`unRange`、`<->>`、`maybeRangeOf`

---

## 現況分析

目前仍使用 `Data.Loc` 的檔案（共 20 個）：

### 1. Parser 層（9 個檔案）
| 檔案 | 使用的元素 |
|------|-----------|
| `src/Syntax/Parser/Lexer.hs` | `Loc (..)`, `Pos (..)`, `L (..)`, `unLoc` |
| `src/Syntax/Parser/TokenStream.hs` | `Loc (..)`, `Pos (..)`, `L (..)`, `posLine`, `posCol`, `unLoc` |
| `src/Syntax/Parser/Util.hs` | 整個模組（`<-->`、`Loc`、`NoLoc` 等） |
| `src/Syntax/Parser/Error.hs` | `Pos` |
| `src/Syntax/Parser.hs` | 整個模組 |
| `src/Syntax/Concrete/Types.hs` | `L`, `Loc (Loc)`, `Located (locOf)`, `Pos` |
| `src/Syntax/Concrete/Instances/Located.hs` | `Located (locOf)`, `(<-->)` |
| `src/Syntax/Concrete/Instances/ToAbstract.hs` | `Located (locOf)` |
| `src/Server/Handler/GCL/Refine.hs` | `L (..)`, `Loc (..)` |

### 2. Located Instances 層（3 個檔案）
| 檔案 | 使用的元素 |
|------|-----------|
| `src/Syntax/Abstract/Instances/Located.hs` | `Loc(..)`, `Located(..)`, `(<-->)` |
| `src/Syntax/Common/Instances/Located.hs` | `Located(..)` |
| `src/GCL/Predicate.hs` | `Located (..)` |
| `src/GCL/WP/Types.hs` | `Located (..)` |

### 3. Pretty/Render 層（3 個檔案）
| 檔案 | 使用的元素 |
|------|-----------|
| `src/Pretty/Util.hs` | 整個模組（`Loc`、`Pos`、`DocWithLoc` 使用） |
| `src/Pretty/Concrete.hs` | `locOf` |
| `src/Render/Class.hs` | `Loc`（僅類型） |

### 4. 已註解/未使用（4 個檔案）
| 檔案 | 狀態 |
|------|------|
| `src/GCL/Exec.hs` | 已註解 |
| `src/GCL/Predicate/Located.hs` | 已註解 |
| `src/GCL/Predicate/Util.hs` | 已註解 |
| `src/Pretty/Predicate.hs` | 已註解 |

---

## 轉換規則

| 舊 (Data.Loc) | 新 (Data.Loc.Range) |
|---------------|---------------------|
| `Loc` 類型 | `Maybe Range` |
| `NoLoc` | `Nothing` |
| `Loc p1 p2` | `Just (mkRange p1 p2)` |
| `L a` 類型 | `R a` 或改用 `(Maybe Range, a)` |
| `unLoc` | `unRange` |
| `<-->` | `<->>` |
| `locOf` | `maybeRangeOf` |
| `Located` typeclass | `MaybeRanged` typeclass |
| `import Data.Loc` | `import Data.Loc.Range` |

---

## Build / Test 方法

```bash
# Build（不執行測試）
stack build --test --no-run-tests

# 執行測試
stack test
```

## 工作流程

每個 Phase 完成後：
1. 執行 `stack build --test --no-run-tests` 確認編譯通過
2. 執行 `stack test` 確認測試通過
3. 更新下方進度追蹤（將 `[ ]` 改為 `[x]`）
4. Commit 變更：`git commit -am "Phase N - <描述>"`

---

## 進度追蹤

- [x] Phase 16 - 擴展 Data.Loc.Range（新增 R、toLTok 等）
- [x] Phase 17 - 遷移 Parser 層（Lexer 輸出後轉換）
- [x] Phase 18 - 遷移 Concrete Syntax
- [x] Phase 19 - 移除 Located instances（改用 MaybeRanged）
- [x] Phase 20 - 遷移 Pretty/Render 層
- [x] Phase 21 - 清理已註解程式碼（無需修改，已是註解）
- [x] Phase 22 - 最終驗證與清理（202 tests passed）

---

## Phase 16 - 擴展 Data.Loc.Range

**目標**：讓 `Data.Loc.Range` 提供所有必要的替代品

**檔案**：`src/Data/Loc/Range.hs`

**新增內容**：

1. 從 `Data.Loc.Inclusive` 轉換的函式：
   ```haskell
   -- 將 Inc.L Tok 轉換為 R Tok（用於 Lexer 輸出後）
   fromIncL :: Inc.L a -> R a
   fromIncL (Inc.L loc x) = R (fromIncLoc loc) x
   
   fromIncLoc :: Inc.Loc -> Maybe Range
   fromIncLoc Inc.NoLoc = Nothing
   fromIncLoc (Inc.Loc (Inc.Pos f1 l1 c1 co1) (Inc.Pos f2 l2 c2 co2)) =
     Just $ mkRange (Pos f1 l1 c1 co1) (Pos f2 l2 (c2 + 1) (co2 + 1))
   ```

2. 為 `R a` 新增更多 instances（如果需要）：
   ```haskell
   instance MaybeRanged (R a) where
     maybeRangeOf (R range _) = Just range
   ```

3. 確保 `Pos` 相關函式都有 re-export。

---

## Phase 17 - 遷移 Parser 層

**目標**：Parser 層完全不使用 `Data.Loc`（除了 Lexer 內部的 `Data.Loc.Inclusive`）

### 17.1 Lexer.hs

**檔案**：`src/Syntax/Parser/Lexer.hs`

**修改**：

1. 將 `import Data.Loc` 改為只使用 `Data.Loc.Range`
2. 修改 `TokStream` 類型：
   ```haskell
   -- 舊：type TokStream = TokenStream (L Tok)
   -- 新：type TokStream = TokenStream (R Tok)
   ```
3. 修改 `scan` 函式，將 `Inc.L` 轉換為 `R`（而非 `L`）
4. 更新 `PrettyToken` instance 使用 `unRange`

### 17.2 TokenStream.hs

**檔案**：`src/Syntax/Parser/TokenStream.hs`

**修改**：

1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 將 `L tok` 改為 `R tok`
3. 將 `unLoc` 改為 `unRange`
4. 將 `locOf` 改為 `maybeRangeOf`
5. 處理 `NoLoc` → `Nothing` 的情況

### 17.3 Parser/Util.hs

**檔案**：`src/Syntax/Parser/Util.hs`

**修改**：

1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 修改 `Bookkeeping`：
   ```haskell
   -- 舊
   data Bookkeeping = Bookkeeping
     { currentLoc :: Loc, ... }
   -- 新
   data Bookkeeping = Bookkeeping
     { currentRange :: Maybe Range, ... }
   ```
3. 將 `NoLoc` 改為 `Nothing`
4. 將 `<-->` 改為 `<->>`
5. 更新 `getLoc` 回傳 `Maybe Range`（或改名為 `getMaybeRange`）

### 17.4 Parser/Error.hs

**檔案**：`src/Syntax/Parser/Error.hs`

**修改**：

1. 已經只使用 `Pos`，確認 import 改為 `import Data.Loc.Range (Pos)`

### 17.5 Parser.hs

**檔案**：`src/Syntax/Parser.hs`

**修改**：

1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 處理 `NoLoc` 的 error 改為 `Nothing`
3. 更新 `adapt` 函式處理 `Maybe Range`

---

## Phase 18 - 遷移 Concrete Syntax

### 18.1 Concrete/Types.hs

**檔案**：`src/Syntax/Concrete/Types.hs`

**修改**：

1. 移除 `import Data.Loc (L, Loc (Loc), Located (locOf), Pos)`
2. 改為 `import Data.Loc.Range (R, Range, Ranged (..), MaybeRanged (..), Pos, mkRange)`
3. 將 `L Tok` 改為 `R Tok`（如果有使用）
4. 更新 `Token` 的 `Located` instance 改為 `MaybeRanged` instance

### 18.2 Concrete/Instances/Located.hs

**檔案**：`src/Syntax/Concrete/Instances/Located.hs`

**修改**：

1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 將所有 `Located` instances 改為 `MaybeRanged` instances
3. 將 `<-->` 改為 `<->>`
4. 將 `locOf` 改為 `maybeRangeOf`

### 18.3 Concrete/Instances/ToAbstract.hs

**檔案**：`src/Syntax/Concrete/Instances/ToAbstract.hs`

**修改**：

1. 將 `import Data.Loc (Located (locOf))` 改為 `import Data.Loc.Range (MaybeRanged (..))`
2. 將 `locOf` 改為 `maybeRangeOf`

### 18.4 Server/Handler/GCL/Refine.hs

**檔案**：`src/Server/Handler/GCL/Refine.hs`

**修改**：

1. 將 `import Data.Loc (L (..), Loc (..))` 改為 `import Data.Loc.Range (R (..), Range (..))`
2. 將 `L` 改為 `R`
3. 將 `NoLoc` 改為 `Nothing`
4. 更新 `translateLoc` 處理 `Maybe Range`

---

## Phase 19 - 移除 Located instances

**目標**：完全移除 `Located` typeclass 的使用，只保留 `MaybeRanged`

### 19.1 Abstract/Instances/Located.hs

**檔案**：`src/Syntax/Abstract/Instances/Located.hs`

**修改**：

1. 移除 `import Data.Loc`
2. 移除所有 `Located` instances（因為已有 `MaybeRanged` instances）
3. 將 `<-->` 改為 `<->>`
4. 考慮將此檔案重新命名或合併

### 19.2 Common/Instances/Located.hs

**檔案**：`src/Syntax/Common/Instances/Located.hs`

**修改**：

1. 移除 `import Data.Loc (Located(..))`
2. 移除 `Located` instances（因為已有 `MaybeRanged` instances）
3. 考慮將此檔案重新命名或合併

### 19.3 GCL/Predicate.hs

**檔案**：`src/GCL/Predicate.hs`

**修改**：

1. 移除 `import Data.Loc (Located (..))`
2. 移除 `locOf = maybeRangeToLoc . maybeRangeOf` 這種橋接程式碼
3. 如有需要 `Located`，在該模組局部定義或使用 `maybeRangeOf`

### 19.4 GCL/WP/Types.hs

**檔案**：`src/GCL/WP/Types.hs`

**修改**：

1. 移除 `import Data.Loc (Located (..))`
2. 移除 `Located` instances

---

## Phase 20 - 遷移 Pretty/Render 層

### 20.1 Pretty/Util.hs

**檔案**：`src/Pretty/Util.hs`

**修改**：

1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 修改 `DocWithLoc` 使用 `Maybe Range`：
   ```haskell
   fromDoc :: Maybe Range -> Doc ann -> DocWithLoc ann
   fromDoc Nothing _ = Empty
   fromDoc (Just (Range a b)) x = DocWithLoc x a b
   ```
3. 將 `NoLoc` 改為 `Nothing`
4. 將 `Loc a b` 模式匹配改為 `Just (Range a b)`
5. 更新 `fromRenderAndLocated` 使用 `maybeRangeOf`

### 20.2 Pretty/Concrete.hs

**檔案**：`src/Pretty/Concrete.hs`

**修改**：

1. 將 `import Data.Loc (locOf)` 改為 `import Data.Loc.Range (maybeRangeOf, maybeRangeToLoc)`
2. 將 `locOf` 改為適當的替代（可能是 `maybeRangeToLoc . maybeRangeOf`，或直接使用 `maybeRangeOf`）

### 20.3 Render/Class.hs

**檔案**：`src/Render/Class.hs`

**修改**：

1. 將 `import Data.Loc (Loc)` 改為 `import Data.Loc.Range (Range)`
2. 更新 `tempHandleLoc` 的類型簽名和實作（可能已經使用 `Maybe Range`）

### 20.4 Render/Predicate.hs

**檔案**：`src/Render/Predicate.hs`

**修改**：

1. 將 `NoLoc` 改為 `Nothing`
2. 更新 `makeOpExpr` 和相關函式

---

## Phase 21 - 清理已註解程式碼

**檔案**：
- `src/GCL/Exec.hs`
- `src/GCL/Predicate/Located.hs`
- `src/GCL/Predicate/Util.hs`
- `src/Pretty/Predicate.hs`

**修改**：

1. 移除已註解的 `import Data.Loc`
2. 如果這些檔案的註解程式碼需要保留，更新它們使用 `Data.Loc.Range`
3. 如果不需要，考慮刪除這些檔案

---

## Phase 22 - 最終驗證與清理

**任務**：

1. 執行驗證命令：
   ```bash
   # 應該為空（除了 Data.Loc.Range.hs 內部）
   grep -r "import Data.Loc" src/ --include="*.hs" | grep -v "Data.Loc.Range" | grep -v "Data.Loc.Inclusive"
   
   # 應該為空
   grep -rn "NoLoc" src/ --include="*.hs" | grep -v "Data.Loc"
   
   # 應該為空
   grep -rn "\<locOf\>" src/ --include="*.hs"
   
   # 應該為空
   grep -rn "<-->" src/ --include="*.hs"
   
   # 應該為空（除了定義處）
   grep -rn "unLoc" src/ --include="*.hs" | grep -v "Data.Loc"
   ```

2. 執行測試：
   ```bash
   stack test
   ```

3. 更新 `.cabal` 或 `package.yaml`（如果需要移除 `srcloc` 依賴）

4. 考慮是否刪除或重新命名 `vendor/srcloc-0.6.0.1/Data/Loc.hs`

---

## 設計決策

### 1. `L a` vs `R a`

- `L a` (from `Data.Loc`) 使用 `Loc`（可能為 `NoLoc`）
- `R a` (from `Data.Loc.Range`) 使用 `Range`（保證有位置）
- Token 一定有位置，所以用 `R Tok` 是合理的

### 2. TokenStream 的類型

```haskell
-- 舊：TokenStream (L Tok)
-- 新：TokenStream (R Tok)
```

Lexer 產生的 token 一定有位置，所以 `R Tok` 比 `L Tok`（可能是 `NoLoc`）更精確。

### 3. 向後相容

這是一個 breaking change。所有使用 `Located` typeclass 的外部程式碼需要遷移到 `MaybeRanged`。

如果需要向後相容，可以：
1. 在 `Data.Loc.Range` 中提供 `Located` typeclass（使用 `maybeRangeToLoc . maybeRangeOf`）
2. 或者提供一個相容模組 `Data.Loc.Compat`

### 4. `fromDoc` 函式簽名變更

```haskell
-- 舊
fromDoc :: Loc -> Doc ann -> DocWithLoc ann

-- 新
fromDoc :: Maybe Range -> Doc ann -> DocWithLoc ann
```

---

## 風險評估

| 風險 | 影響 | 緩解措施 |
|------|------|----------|
| Parser 層修改可能導致解析錯誤 | 高 | 先執行所有 golden tests |
| `L` → `R` 的轉換可能遺漏 | 中 | 編譯器會報錯 |
| Pretty printing 可能出問題 | 中 | 檢查 golden test 輸出 |
| 外部程式碼依賴 `Located` | 低 | 這是內部專案 |

---

## 預估時間

| Phase | 預估時間 |
|-------|----------|
| Phase 16 | 30 分鐘 |
| Phase 17 | 2 小時 |
| Phase 18 | 1 小時 |
| Phase 19 | 1 小時 |
| Phase 20 | 1 小時 |
| Phase 21 | 30 分鐘 |
| Phase 22 | 30 分鐘 |
| **總計** | **約 7 小時** |

---

## 結論

完成此遷移後，codebase 將：
1. 完全不依賴 `Data.Loc`（只依賴 `Data.Loc.Inclusive` 和 `Data.Loc.Range`）
2. 使用更精確的類型（`Range` 保證有位置，`Maybe Range` 明確表示可能沒有）
3. 消除 `NoLoc` 這個特殊值，改用標準的 `Maybe` 類型
