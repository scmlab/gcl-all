# Plan: 使用 Qualified Import 區分 End-Inclusive Loc

## 狀態: ✅ 完成

## 背景

`lexer-applicative` 產生的 `Loc` 是 end-inclusive（表示 "AB" 結尾為 2），而上層使用的 `Loc` 是 end-exclusive（結尾為 3）。目前在 `Lexer.hs` 的 `translateLoc` 做轉換，但兩者使用相同類型，容易混淆。

## 目標

透過不同的 module 區分兩種語義，使用 `qualified ... as Inc` 讓 inclusive 版本更明顯。

## Steps

1. **✅ 建立 `Data.Loc.Inclusive` 模組**
   - 複製 `vendor/srcloc-0.6.0.1/Data/Loc.hs` 至 `vendor/srcloc-0.6.0.1/Data/Loc/Inclusive.hs`
   - 將 `module Data.Loc` 改為 `module Data.Loc.Inclusive`
   - 加上註解說明這是 end-inclusive 版本

2. **✅ 修改 `Applicative.hs`**
   - 將 `vendor/lexer-applicative-2.1.0.2/src/Language/Lexer/Applicative.hs` 第 24 行
   - `import Data.Loc` 改為 `import Data.Loc.Inclusive`

3. **✅ 更新 `Lexer.hs` 的 imports**
   - 在 `src/Syntax/Parser/Lexer.hs` 加入 `import qualified Data.Loc.Inclusive as Inc`
   - 將 `translateLoc` 明確處理 `Inc.L` -> `L` 的轉換
   - 新增 `TokStreamInc` 類型別名表示從 lexer 來的 end-inclusive token stream
   - `convertLoc` 函數負責 `Inc.Loc` -> `Loc` 轉換

4. **✅ 調整 `TokenStream.hs`**
   - 更新 imports 使用 `Data.Loc` (end-exclusive) 和 `qualified Data.Loc.Inclusive as Inc`

## 備註

- `Pos` 本身沒有 inclusive/exclusive 語義問題，兩個模組各自定義相同結構
- `LexicalError` 定義在 `Language.Lexer.Applicative`，使用 `Inc.Pos`
- 所有 202 個測試通過
