## Build / Test 方法

```bash
# Build（不執行測試）
stack build --test --no-run-tests

# 執行測試
stack test
```

過程中出現以下警告可以忽略
```
Ticker: poll failed: Interrupted system call: Interrupted system call
```

## 工作流程

每個 Phase 完成後：
1. 執行 `stack build --test --no-run-tests` 確認編譯通過
2. 執行 `stack test` 確認測試通過
3. 更新進度
4. Commit 變更
