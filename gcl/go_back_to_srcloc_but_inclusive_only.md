# Refactoring Plan: Remove vendor/ and Use Original srcloc Package

## Background

Currently, the `separate_loc` branch includes vendored copies of `srcloc` and `lexer-applicative` packages with minimal modifications:

1. **srcloc**: Created `Data.Loc.Inclusive` which is essentially a copy of `Data.Loc` with only the module name changed
2. **lexer-applicative**: Only changed `import Data.Loc` to `import Data.Loc.Inclusive`

**Key Insight**: The original `Data.Loc` module already uses end-inclusive semantics, so creating `Data.Loc.Inclusive` was unnecessary.

## Goal

Remove the `vendor/` directory and use the original `srcloc` and `lexer-applicative` packages from Hackage, with minimal changes to our codebase.

## 執行步驟

### Phase 1: 更新 package.yaml

編輯 `gcl/package.yaml`：

```yaml
dependencies:
  # ... other dependencies ...
  - srcloc              # 取消註解
  - lexer-applicative   # 取消註解
  # ... other dependencies ...

library:
  source-dirs:
    - src
    # 移除以下兩行：
    # - vendor/lexer-applicative-2.1.0.2/src
    # - vendor/srcloc-0.6.0.1
```

執行驗證：
```bash
stack build --test --no-run-tests
stack test
```

### Phase 2: 更新 Lexer.hs

編輯 `gcl/src/Syntax/Parser/Lexer.hs`，修改 import：

```haskell
-- 修改前:
import qualified Data.Loc.Inclusive as Inc

-- 修改後:
import qualified Data.Loc as Inc
```

**說明**: 原始的 `Data.Loc` 本身就是 end-inclusive 語義，這正是 lexer 需要的。

執行驗證：
```bash
stack build --test --no-run-tests
stack test
```

### Phase 3: 更新 Range.hs

編輯 `gcl/src/Data/Loc/Range.hs`：

1. 修改 import：
```haskell
-- 修改前:
import qualified Data.Loc.Inclusive as Inc

-- 修改後:
import qualified Data.Loc as Inc
```

2. 更新模組文件註解，將 `Data.Loc.Inclusive` 改為 `Data.Loc`

3. 函式名稱 `fromInclusiveLoc` 和 `fromInclusivePos` 可以保持不變（為了清楚表達語義），或改名為 `fromDataLoc` / `fromDataLocPos`

執行驗證：
```bash
stack build --test --no-run-tests
stack test
```

### Phase 4: 移除 vendor 目錄

```bash
rm -rf gcl/vendor
```

執行驗證：
```bash
stack build --test --no-run-tests
stack test
```

### Phase 5: 最終驗證

1. **確認沒有 vendor 相關的參考**
   ```bash
   grep -r "vendor" gcl/package.yaml gcl/gcl.cabal || echo "No vendor references found - OK"
   ```

2. **確認沒有 Data.Loc.Inclusive 的使用**
   ```bash
   grep -r "Data\.Loc\.Inclusive" gcl/src --include="*.hs" || echo "No Data.Loc.Inclusive found - OK"
   ```

3. **確認 Data.Loc 的 import 都是 qualified**
   ```bash
   grep -r "import.*Data\.Loc" gcl/src --include="*.hs"
   # 應該只看到:
   # - Data.Loc.Range (我們自己的模組)
   # - qualified Data.Loc as Inc (在 Lexer.hs 和 Range.hs)
   ```

4. **最後一次完整測試**
   ```bash
   stack clean
   stack build --test --no-run-tests
   stack test
   ```

## 優點

1. ✅ **更簡單的依賴管理**: 使用標準 Hackage 套件而非 vendored 複本
2. ✅ **更容易更新**: 可透過正常的依賴更新來升級 `srcloc` 和 `lexer-applicative`
3. ✅ **更少的維護成本**: 移除約 600 行 vendored 程式碼
4. ✅ **更清晰的架構**: `Data.Loc` 用於 end-inclusive（lexer 層），`Data.Loc.Range` 用於 end-exclusive（應用層）

## 風險評估與應對

### 風險 1: 套件版本不相容
- **應對**: 當前分支已經使用這些套件（只是 vendored 版本），版本相容性已經驗證過

### 風險 2: 預期外的行為差異
- **應對**: vendored 的 `Data.Loc.Inclusive` 除了模組名稱和註解外，與 `Data.Loc` 完全相同

### 風險 3: Build 系統問題
- **應對**: 執行 `stack build --test --no-run-tests` 會立即發現任何問題

## 工作流程

每個 Phase 完成後：
1. 執行 `stack build --test --no-run-tests` 確認編譯通過
2. 執行 `stack test` 確認測試通過
3. Commit 變更

過程中出現以下警告可以忽略：
```
Ticker: poll failed: Interrupted system call: Interrupted system call
```

## 備選方案: 保留 vendor 但簡化

如果擔心完全移除 vendor 依賴，可以採用折衷方案：

1. 保留 `vendor/srcloc-0.6.0.1/Data/Loc.hs` (原始版本)
2. 刪除 `vendor/srcloc-0.6.0.1/Data/Loc/Inclusive.hs`
3. 更新 imports 改用 `Data.Loc` 而非 `Data.Loc.Inclusive`
4. 保留 package.yaml 中的 vendor source-dirs

此方案提供了中間路線，但仍建議使用 Hackage 套件為佳。

## 結論

此重構透過移除不必要的 vendored 依賴來簡化程式碼庫。原始的 `Data.Loc` 模組已經提供我們需要的 end-inclusive 語義，使得 `Data.Loc.Inclusive` 複本變得多餘。
