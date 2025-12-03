# Plan: 將 Data.Loc 作為內部模組隔離

## TL;DR

`loc_to_range_3.md` 已完成大部分遷移。本計畫將 `Data.Loc` 視為「內部模組」，只被 `Data.Loc.Range` 使用，確保應用程式碼（`src/` 下其他檔案）不直接 import `Data.Loc`。

---

## 目標

1. **隔離 `Data.Loc`**：只有 `Data.Loc.Range` 可以 import `Data.Loc`
2. **清理殘留**：移除已註解的 `import Data.Loc`
3. **加入文件說明**：在 `Data.Loc.Range` 加入註解說明使用方式

### 最終架構
```
應用程式碼 ──import──▶ Data.Loc.Range ──import──▶ Data.Loc (內部)
                              │
                              └──import──▶ Data.Loc.Inclusive (lexer 用)
```

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

- [x] Phase 23 - 清理已註解的 import Data.Loc
- [x] Phase 24 - 加入文件說明
- [x] Phase 25 - 最終驗證（202 tests passed）

---

## Phase 23 - 清理已註解的 import Data.Loc

**目標**：移除已註解但仍存在的 `import Data.Loc` 行

**檔案**：

### 23.1 src/GCL/Predicate/Util.hs

移除已註解的 import：
```haskell
-- 移除這行（第 11 行）
import Data.Loc (Loc (..), Located (locOf), unLoc)
```

### 23.2 src/GCL/Predicate/Located.hs

移除已註解的 import：
```haskell
-- 移除這行（第 8 行）
import Data.Loc (Loc (..), Located, locOf)
```

### 23.3 src/Pretty/Predicate.hs

移除已註解的 import：
```haskell
-- 移除這行（第 5 行）
-- import Data.Loc (unLoc)  -- unused, code using this is commented out
```

---

## Phase 24 - 加入文件說明

**目標**：在 `Data.Loc.Range` 加入清晰的文件說明

**檔案**：`src/Data/Loc/Range.hs`

**修改**：在模組開頭加入文件說明：

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Data.Loc.Range
-- Description : Position and range types for source locations
-- 
-- This module provides the primary API for working with source locations.
-- 
-- == Usage
-- 
-- All application code should import this module instead of 'Data.Loc':
-- 
-- @
-- import Data.Loc.Range (Range, Pos, MaybeRanged(..), Ranged(..), ...)
-- @
-- 
-- == Design
-- 
-- * 'Range' represents a span with start and end positions (end-exclusive)
-- * 'Maybe Range' replaces the old 'Loc' type (where 'Nothing' = 'NoLoc')
-- * 'Ranged' typeclass for values that always have a range
-- * 'MaybeRanged' typeclass for values that may or may not have a range
-- 
-- == Internal Dependencies
-- 
-- This module re-exports types from 'Data.Loc' (e.g., 'Pos', 'L', 'Located')
-- for backward compatibility with the lexer\/parser layer.
-- 'Data.Loc' should be considered an internal module and not imported directly
-- by application code.
-- 
-- == Conversion from Inclusive Locations
-- 
-- The lexer produces end-inclusive locations ('Data.Loc.Inclusive').
-- Use 'fromInclusiveLoc' to convert to end-exclusive 'Range'.

module Data.Loc.Range
  ( -- ... 保持原本的 export list
```

---

## Phase 25 - 最終驗證

**目標**：確認 `Data.Loc` 已被正確隔離為內部模組

**驗證命令**：

```bash
# 1. 檢查 src/ 下只有 Data.Loc.Range.hs import Data.Loc
#    應該只顯示 src/Data/Loc/Range.hs
grep -r "import Data\.Loc[^.]" src/ --include="*.hs" | grep -v "Data.Loc.Range" | grep -v "Data.Loc.Inclusive"

# 2. 檢查已註解的 import 也已移除
#    應該為空（或只有 Data.Loc.Range.hs 內部）
grep -rn "import Data\.Loc[^.]" src/ --include="*.hs"

# 3. 執行測試
stack test
```

**預期結果**：
- 第 1 個命令：無輸出
- 第 2 個命令：只顯示 `src/Data/Loc/Range.hs:65:import Data.Loc hiding (fromLoc)`
- 第 3 個命令：所有測試通過

---

## 設計決策

### 為何不移動 Data/Loc.hs？

保持 `vendor/srcloc-0.6.0.1/Data/Loc.hs` 在原位置的原因：

1. **來源清晰**：保留 vendor 目錄結構，明確標示這是第三方程式碼
2. **版本追蹤**：目錄名包含版本號，方便追蹤來源
3. **最小變更**：不需修改 `.cabal` 或 `package.yaml` 的 source-dirs
4. **隔離已達成**：透過 `Data.Loc.Range` 的 re-export，應用程式碼已不需直接 import

### Data.Loc vs Data.Loc.Inclusive

| 模組 | 用途 | 誰可以 import |
|------|------|--------------|
| `Data.Loc` | 提供 `Pos`、`L`、`Located` 等基礎類型 | 只有 `Data.Loc.Range` |
| `Data.Loc.Inclusive` | Lexer 產生的 end-inclusive 位置 | `Data.Loc.Range`、`lexer-applicative` |
| `Data.Loc.Range` | 主要 API，提供 `Range`、`MaybeRanged` 等 | 所有應用程式碼 |

---

## 預估時間

| Phase | 預估時間 |
|-------|----------|
| Phase 23 | 10 分鐘 |
| Phase 24 | 15 分鐘 |
| Phase 25 | 5 分鐘 |
| **總計** | **約 30 分鐘** |

---

## 結論

完成此計畫後：
1. `Data.Loc` 成為內部模組，只被 `Data.Loc.Range` import
2. 所有應用程式碼統一使用 `Data.Loc.Range`
3. 文件說明清楚指引開發者正確使用方式
4. 保持 vendor 目錄結構不變，來源清晰
