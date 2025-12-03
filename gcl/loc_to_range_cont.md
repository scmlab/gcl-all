# Plan: 消滅所有 `Data.Loc.Loc` 使用（續）

## TL;DR

Phase 1-6 已完成類型定義的遷移，但實際程式碼中仍有約 50 個檔案使用 `Loc`、`NoLoc`、`<-->`、`locOf` 等。本計畫將系統性地清除這些殘留用法，改用 `Range` / `Maybe Range`。

---

## 目標

**消滅所有 `Data.Loc.Loc` 的使用**，改用 `Range` / `Maybe Range`。

### 例外（可以繼續使用 `Loc`）：
- `vendor/srcloc-0.6.0.1/Data/Loc/Inclusive.hs` - 底層 Loc 定義
- `src/Syntax/Lexer.hs` - Lexer 層，因為 token 一定有位置
- `src/Data/Loc/Range.hs` - Range 的定義與 Loc 轉換函式

### 轉換規則：
| 舊 | 新 |
|---|---|
| `Loc` 類型 | `Maybe Range` |
| `NoLoc` | `Nothing` |
| `Loc p1 p2` | `Just (mkRange p1 p2)` |
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

**注意**：過程中可忽略此錯誤訊息：`Ticker: poll failed: Interrupted system call: Interrupted system call`

## 工作流程

每個 Phase 完成後：
1. 執行 `stack build --test --no-run-tests` 確認編譯通過
2. 執行 `stack test` 確認測試通過
3. 更新下方進度追蹤（將 `[ ]` 改為 `[x]`）
4. Commit 變更：`git commit -am "Phase N - <描述>"`

---

## 進度追蹤

- [ ] Phase 7 - Syntax.Parser 周邊
- [ ] Phase 8 - Syntax.Common Instances
- [ ] Phase 9 - Syntax.Abstract Instances
- [ ] Phase 10 - Syntax.Concrete Instances
- [ ] Phase 11 - Syntax.Typed
- [ ] Phase 12 - GCL 模組群
- [ ] Phase 13 - Server 模組群
- [ ] Phase 14 - Pretty/Render 模組群
- [ ] Phase 15 - 清理與驗證

---

## Phase 7 - Syntax.Parser 周邊

**檔案**：
- `src/Syntax/Parser/Util.hs`
- `src/Syntax/Parser.hs`
- `src/Syntax/ConstExpr.hs`
- `src/Syntax/Substitution.hs`

**修改**：
1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 將 `NoLoc` 改為 `Nothing`
3. 將 `<-->` 改為 `<->>`
4. 將 `locOf` 改為 `maybeRangeOf`
5. 將 `Loc from to` 模式匹配改為 `Just range` 並使用 `rangeStart`/`rangeEnd`

---

## Phase 8 - Syntax.Common Instances

**檔案**：
- `src/Syntax/Common/Instances/Located.hs`
- `src/Syntax/Common/Instances/Json.hs`

**修改**：
1. 將 `Located` 實例改為 `MaybeRanged` 實例
2. 更新 JSON 序列化使用 `Maybe Range`

---

## Phase 9 - Syntax.Abstract Instances

**檔案**：
- `src/Syntax/Abstract/Instances/Located.hs`
- `src/Syntax/Abstract/Instances/Json.hs`
- `src/Syntax/Abstract/Operator.hs`

**修改**：
1. 將所有 `Located` 實例改為 `MaybeRanged` 實例
2. 將 `<-->` 改為 `<->>`
3. 將 `NoLoc` 改為 `Nothing`

---

## Phase 10 - Syntax.Concrete Instances

**檔案**：
- `src/Syntax/Concrete/Instances/Located.hs`
- `src/Syntax/Concrete/Instances/ToAbstract.hs`

**修改**：
1. 將 `Located` 實例改為 `MaybeRanged` 實例
2. 更新 `ToAbstract` 中的位置合併邏輯

---

## Phase 11 - Syntax.Typed

**檔案**：
- `src/Syntax/Typed/Types.hs`
- `src/Syntax/Typed/Instances/Located.hs`
- `src/Syntax/Typed/Instances/Substitution.hs`
- `src/Syntax/Typed/Instances/Free.hs`
- `src/Syntax/Typed/Util.hs`
- `src/Syntax/Typed.hs`

**修改**：
1. 將類型中的 `Loc` 欄位改為 `Maybe Range`
2. 將 `Located` 實例改為 `MaybeRanged` 實例
3. 將 `NoLoc` 改為 `Nothing`

---

## Phase 12 - GCL 模組群

**檔案**：
- `src/GCL/Common.hs`
- `src/GCL/Predicate.hs`
- `src/GCL/Predicate/Util.hs`
- `src/GCL/Substitution.hs`
- `src/GCL/WP.hs`
- `src/GCL/WP/Type.hs`
- `src/GCL/WP/WP.hs`
- `src/GCL/WP/SP.hs`
- `src/GCL/Type.hs`
- `src/GCL/Exec.hs`
- `src/GCL/Exec/ExecMonad.hs`

**修改**：
1. 將 `import Data.Loc` 改為 `import Data.Loc.Range`
2. 將所有 `Loc` 類型用法改為 `Maybe Range`
3. 將 `NoLoc` 改為 `Nothing`
4. 將 `locOf` 改為 `maybeRangeOf`

---

## Phase 13 - Server 模組群

**檔案**：
- `src/Server/SrcLoc.hs`
- `src/Server/GoToDefn.hs`
- `src/Server/Hover.hs`
- `src/Server/Handler.hs`
- `src/Server/Highlighting.hs`
- `src/Server/Load.hs`
- `src/Server/Monad.hs`
- `src/Server/PositionMapping.hs`
- `src/Server/IntervalMap.hs`

**修改**：
1. 更新 LSP 位置轉換邏輯（`Loc` → `Maybe Range` → LSP `Range`）
2. 處理 `Nothing` 情況（原本的 `NoLoc`）

---

## Phase 14 - Pretty/Render 模組群

**檔案**：
- `src/Pretty/*.hs`
- `src/Render/*.hs`
- `src/Render/Syntax/*.hs`

**修改**：
1. 將 `locOf` 改為 `maybeRangeOf`
2. 更新渲染邏輯處理 `Nothing`

---

## Phase 15 - 清理與驗證

**任務**：

1. 執行以下命令確認無殘留（除例外檔案外）：
   ```bash
   # 檢查 import Data.Loc（應只剩 Lexer.hs 和 Range.hs）
   grep -r "import Data.Loc" src/ --include="*.hs" | grep -v "Data.Loc.Range"
   
   # 檢查 NoLoc（應為 0）
   grep -r "NoLoc" src/ --include="*.hs"
   
   # 檢查 <-->（應為 0）
   grep -r "<-->" src/ --include="*.hs"
   
   # 檢查 locOf（應為 0，除了 Lexer）
   grep -r "locOf" src/ --include="*.hs"
   ```

2. 執行 `stack test` 確認所有測試通過

3. 考慮是否移除或重新命名 `Data.Loc` 模組

---

## 設計決策

### 1. `Located` vs `MaybeRanged`

- `Located` typeclass（回傳 `Loc`）將被 `MaybeRanged`（回傳 `Maybe Range`）取代
- 如果某些地方確實保證有位置，可使用 `Ranged` typeclass（回傳 `Range`）

### 2. 模式匹配轉換

舊：
```haskell
case locOf x of
  NoLoc -> ...
  Loc p1 p2 -> ...
```

新：
```haskell
case maybeRangeOf x of
  Nothing -> ...
  Just range -> ... -- 使用 rangeStart range, rangeEnd range
```

### 3. 位置合併轉換

舊：
```haskell
locOf a <--> locOf b
```

新：
```haskell
maybeRangeOf a <->> maybeRangeOf b
```

---

## Further Considerations

1. **效能考量**：`Maybe Range` 比 `Loc` 多一層包裝，但這應該可忽略。

2. **向後相容**：如果有外部程式碼依賴 `Located` typeclass，需要提供遷移指南或相容層。

3. **測試覆蓋**：確保 golden tests 涵蓋有位置和無位置（`Nothing`）的情況。
