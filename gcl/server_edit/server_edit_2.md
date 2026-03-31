現在 client 端有 (text, version)

server 端也有 (text, version)

有 client to server 和 server to client 兩個 queue 可以送訊息

一開始 client 會送 "didOpen" message 讓兩邊同步成相同的 text 和 version

client 可以塞 "didChange" message (edits, new version) 給 server 來讓 server 同步

server 在 versionN 時, 可以塞 "applyEdit" message (edits, versionN) 給 client
讓 client 在文件若還是 versionN 才接受編輯
我們的實作只讓 server 送出這種指定版本的編輯
(LSP 中 client 會送 response 表示成功失敗, 但這邊我們忽視這個 response)

client 端可以由 user 或 extension 進行編輯.

使用者編輯:
改變 text / 增加 version / 送出 (edits, version)
"假設"這三件事情是 atomic 的

client 端由 extension 接收到 server 的編輯請求:
檢查版本 / 改變 text / 增加 version / 送出 (edits, version)
"假設"這四件事情是 atomic

注意: LSP 中 version 在 client 端只保證會嚴格遞增, 但不保證每次只 +1 .


Load 時看到 v10, 那 queue 前面一個應該就是 v10
發去如果能成功, client 端必為 v10, 前面不會有別的, 不然不會還 v10

### 性質: NextDidChange

我們主張以下的性質:  
server 送出的 applyEdit 如果成功, 則 server 收到的下一個 didChange 必屬於此 edit 所造成的. 不是 client edit 造成的.

原因:  
如果使用者在 extension 處理 applyEdit 前打字, 版本會改變, 導致 applyEdit 因版本不符而失敗. 因此 "applyEdit 成功" => "版本沒改" => "沒有使用者輸入插隊" => "下一個 didChange 必來自此 edit".

### TLA+ 模型驗證 (edit.tla)

用 TLA+ 對 NextDidChange 性質進行 model checking.

**變數:**

| 變數 | 說明 |
|---|---|
| `clientVersion` / `serverVersion` | 各端的版本號 |
| `c2sQueue` | client→server FIFO, 每個訊息帶 `source ∈ {"user","server"}` 和 `version` |
| `s2cQueue` | server→client FIFO, 每個訊息帶 `version` |
| `awaitingServerEdit` | applyEdit 成功後設 TRUE, server 收到下一個 didChange 時重設 FALSE |

**動作:**

- `UserEdit`: 使用者編輯 — 增加 clientVersion, 在 c2sQueue 塞入 source="user" 的 didChange
- `ServerSendApplyEdit`: server 送出 applyEdit(serverVersion) 到 s2cQueue
- `ClientReceiveApplyEdit`: client 從 s2cQueue 取出 applyEdit, 版本符合則成功 (增加 clientVersion, 在 c2sQueue 塞入 source="server" 的 didChange, 設 awaitingServerEdit=TRUE), 版本不符則丟棄
- `ServerReceiveDidChange`: server 從 c2sQueue 取出 didChange, 更新 serverVersion, 重設 awaitingServerEdit=FALSE

**不變量 NextDidChange:**

```
(awaitingServerEdit ∧ c2sQueue ≠ ⟨⟩) ⟹ Head(c2sQueue).source = "server"
```

即: applyEdit 成功後 (awaitingServerEdit=TRUE), 若 c2sQueue 非空, 其最前面的訊息必為 server-sourced.

**TLC 驗證結果:** MaxVersion=6, MaxQueueLen=3 下, 窮舉 485 個可達狀態, 不變量在所有狀態均成立.
