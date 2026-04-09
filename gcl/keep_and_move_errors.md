# Plan: Keep and Move Errors in FileState

## Goal

Store errors in `FileState` so that error positions can be tracked and moved
as the user edits, just like specs/holes/POs/warnings.

## Design

### FileState 新增欄位

```haskell
data FileState = FileState
  { fsErrors          :: ![Error]   -- 新增
  , fsSpecifications  :: ![Spec]
  , fsHoles           :: ![Hole]
  , fsProofObligations :: ![PO]
  , fsWarnings        :: ![StructWarning]
  , fsIdCount         :: !Int
  , fsSemanticTokens  :: ![LSP.SemanticTokenAbsolute]
  , fsDefinitionLinks :: !(IntervalMap OriginTargetRanges)
  , fsHoverInfos      :: !(IntervalMap LSP.Hover)
  }
```

### 各情境的 FileState 內容

| 情境 | 其他欄位 | fsErrors |
|---|---|---|
| Load 成功 | 全有 | 空 |
| Load 失敗 | 全空 | 有 |
| Refine 成功 | 全有（merge） | 保留 base 的 fsErrors（不清空） |
| Refine fragment 有 error | base 的欄位 | 累加新 error |

Load 失敗時：其他欄位全空，只留 error。不保留上一次成功的狀態。

Refine fragment 有 error 時：base FileState 的其他欄位保留，新 error **累加**（`++`）到現有的 `fsErrors` 上。

這樣可以支援多個 spec 各自 refine 失敗的情境：
- Spec A 失敗 → `fsErrors = [errA]`
- Spec B 失敗 → `fsErrors = [errA, errB]`（errA 不被蓋掉）

同一個 spec retry 會有重複 error，但可接受，下次 load 成功時全部清掉。

Refine 成功時：`mergeFileState` 使用 `fsErrors = fsErrors moved`，**不清空** base 的 errors。
這樣 spec B 成功不會抹掉 spec A 之前的 error。

### Refine 失敗的兩種路徑

Refine 的 error 有兩種層次，行為完全不同：

**Syntax error（outer `Either` 失敗）**：
- `sendEditTextsWithVersion` **不會**被呼叫，spec 維持在 source 裡
- 走 `Left errs -> sendErrorNotification filePath errs`
- **不存進 FileState**，下次 refine 時 spec 還在

**Type/struct error（inner `Either` 失敗）**：
- `sendEditTextsWithVersion` **會**被呼叫，spec 被 implementation code 取代
- error 累加進 `fsErrors`，設為 pending edit
- spec 消失 → 使用者無法再 refine 同一個 spec
- 下次 load 清掉 error

### 為什麼「errA 殘留到 spec A 成功 refine」不會發生

看起來 `fsErrors = fsErrors moved` 會讓 errA 在 spec A 成功後仍殘留，
但實際上這個情境**不存在**：

能進 `fsErrors` 的只有 type/struct error。這類 error 發生時，spec 的 source 已被
implementation code 取代，spec 從 FileState 消失。使用者無法再對同一個 spec 做 refine——
能 refine 的 spec 都是「上次 load 發現、且尚未被 refine 過的 spec」。

因此 `fsErrors` 裡的 error 和現有 spec 之間沒有交集，不存在「同一個 spec 先失敗再成功」的路徑。

### applyMovesToError 的策略

**Error 永遠不會在 didChange 時被 drop。**

- 編輯在 error range **前面**：range 正常位移
- 編輯在 error range **後面**：range 不動
- 編輯和 error range **重疊**：**freeze — range 原地不動，error 繼續顯示**

只有 load 有資格清除 error（load 會把整個 FileState 換掉，完全依照當時 load
的結果，不考慮之前的狀態）。

這和 Spec/Hole/PO/Warning 的語意不同：
- Spec/Hole/PO/Warning 代表程式結構，結構被破壞就消失，是正確的
- Error 代表「上次 load 發現的問題」，只有 load 有資格清除它

實作上，`applyMovesToError` 回傳 `Error`（不是 `Maybe Error`），
`applyMovesToFileState` 用 `map` 而非 `mapMaybe`。

range 的位移用 `fromMaybe r (foldM applyGCLMove r moves)`：
重疊時 `foldM` 回傳 `Nothing`，`fromMaybe` 保留原始 range。

### applyMovesToError 的 range 分類

Error 的 range 分兩種處理：

- **容器（`applyGCLMoveToContainerRange`）**：
  - `StructError MissingAssertion` — range 是整個 `do...od` loop，和 `MissingBound` 同性質

- **非容器（`applyGCLMove`）**：
  - `StructError MissingPostcondition` — range 是最後一個 statement
  - `StructError MultiDimArrayAsgnNotImp` — range 是特定語句
  - `ParseError` — 指向出錯的位置
  - `TypeError` — 指向出錯的 expression/name

`TypeError` 大部分 constructor 都有行號（透過 `Name.loc` 或直接的 `Maybe Range`），
前端也有顯示。唯一例外是 `RedundantExprs`，沒有 location 欄位，原封不動保留即可。

`TypeError` 的 range 有兩種來源：
- 直接的 `Maybe Range`（`UnifyFailed`, `KindUnifyFailed`, `RecursiveType`, `PatternArityMismatch`）
- 藏在 `Name.loc` 裡（`NotInScope`, `AssignToConst`, `UndefinedType`, `DuplicatedIdentifiers`,
  `RedundantNames`, `MissingArguments`）

移動時兩種都要更新。

### StructWarning 不需要改

`MissingBound` 是目前唯一的 constructor，已經用 `applyGCLMoveToContainerRange`，正確。

## 需要修改的地方

1. **`Server/FileState.hs`** — 加 `fsErrors :: ![Error]`

2. **`Server/Load.hs`** — load 失敗時呼叫 `setFileState` with empty fields + errors，
   而不只是 `sendErrorNotification`

3. **`Server/Refine.hs`** — refine fragment 失敗時，把 error 存進 base FileState
   的 `fsErrors`，再呼叫 `setPendingEdit`

4. **`Server/Move.hs`** — 實作 `applyMovesToError`，加進 `applyMovesToFileState`

5. **`Server/Handler/OnDidChangeTextDocument.hs`** — `applyTranslation` 之後，
   若 `fsErrors` 非空，重新送 error notification（位置已更新）

6. **`Server/Monad.hs`** / **`Server/Notification/Error.hs`** — `sendErrorNotification`
   改為從 FileState 讀 `fsErrors`，或在 didChange handler 裡直接送

7. **`Server/Refine.hs` `mergeFileState`** — 加上 `fsErrors` 的合併邏輯
   （refine 成功時保留 base 的 `fsErrors`，fragment 失敗時累加新 error）

## 前端

**這次不動前端。** server 繼續透過現有的 `gcl/error` notification 送 errors，
前端不需要修改。差別只是 `gcl/error` 會在 `didChange` 時也送（帶移動後的位置），
而不只是在 load 時送。

## Future Direction

目前 server 和 client 用兩個 channel：
- `gcl/error` → errors
- `gcl/update` → specs/holes/POs/warnings

未來可以統一成一個 `gcl/update`，把 errors 也包進去，
讓 client state 的結構和 server 的 `FileState` 完全對應。
這樣 server 是唯一的 source of truth，client 只是鏡像。
