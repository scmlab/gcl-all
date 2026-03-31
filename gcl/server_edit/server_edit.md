Server 送出 workspace/applyEdit 時, 將指定目前的 VFS 版本, 強制 client 只有在相同版本時才進行編輯.

Server 將在送出 edit request 後的下一個 didChange 中, 用內容比對直接判斷 edit 是否生效, 不等待也不使用 edit response.

### workspace/applyEdit 背景知識

在 TextDocumentEdit 中可以對一個 uri (file) 指定版本, 並給出多個 range -> newText 的修改.

這些 range 指向編輯前的位置. 同時編輯, 沒有先後順序. (和 didChange 不同)

### Client response 和 notification 的順序不保證

由於 LSP spec 中並沒有保證 client response 和 notification 的順序, 所以 edit response 可能在我們收到 0 個 / 1 個 / 甚至更多個 didChange 後才收到. 這會讓情況相當複雜.

### 性質: NextDidChange

我們主張以下的性質:  
送出的 workspace/applyEdit 如果成功, 則 server 收到的下一個 didChange notification 必屬於此 edit.

原因:  
因為版本鎖定機制, 如果使用者在 client 處理 applyEdit 前打字, VFS 版本會改變, 導致 applyEdit 因版本不符而失敗. 因此 "applyEdit 成功" => "版本沒改" => "沒有使用者輸入插隊" => "下一個 didChange 必來自此 edit".

### 容許 content 的不同產生路徑

我們用 content 算出對應的 state. 即使到達某 content 的方式不同 (使用者手動輸入 vs server edit), 只要 content 相同, 我們就接受這個對應的 state.

### 在 didChange 判斷是否編輯成功

Server 送出 applyEdit 前, 會在 server 端預先計算一份 edit 結果, 並依此結果預先計算一份 state.

根據 "NextDidChange", server 送出 workspace/applyEdit 後, 我們將不依賴 client applyEdit 的 response 來得知是否編輯成功, 而是比對 "server 預先計算的 edit 結果" 和 "實際處理完 didChange 後的結果" 這兩者是否相同.
- 結果不同: 拋棄預先計算的 state.
- 結果相同: 採用預先計算的 state.

此處有一個可能性極低的分支: 使用者先進行了和 server 將送來的 edit 一樣的動作, 以至於雖然 server 晚送來的 edit 不能 apply, 但實際的內容剛好相等. 但因為我們主要在意的是編輯後的結果是否與 state 相符, 所以是誰編輯的並不重要.


### 每個 edit 獨立計算

產生 edit request 時不考慮前面的 edits, 只根據發送 request 時指定版本的 VFS 內容來計算.

### 性質: AtMostOnePendingEdit

我們只需要等待一個未完成的 edit request.

從 edit request 中指定的版本來分析:
1. 兩個 edit request 中指定同一個版本: 因為第一個成功後版本會變, 第二個必定失敗. 所以我們將這視為 coding error, 並擋下這種狀況.
2. 兩個 edit request 中指定不同的版本: 若 Edit2 的版本 > Edit1 的版本, 表示中間發生過 didChange (版本才會改變), 而那個 didChange 會清除 Edit1 的 pending 狀態.


### Pending Edit 機制

Server 維護一個 `Maybe PendingEdit` 來追蹤正在等待的 edit:

```haskell
data PendingEdit = PendingEdit
  { expectedContent :: Text
  , precomputedState :: State
  }
```

流程:
1. **發送 edit 前**: 檢查是否有 pending edit
   - 有 → 立即失敗 (不等待)
   - 無 → 計算 edit, 設置 pending edit, 發送 request

2. **收到 didChange 時**: 檢查是否有 pending edit
   - 無 → 正常處理使用者輸入
   - 有 → 比對 content
     - 符合 → 採用 precomputed state
     - 不符 → 拋棄
     - 無論結果, 清除 pending edit


### 設計的缺點

- 這邊假設 client (VS Code) 會把一個 server edit request 中的多個 edit 的結果在一個 didChange 送回來, 如果不是這樣則整個設計不成立.
  - 如果真的遇到壞的狀況, 最差我們可以把多個 edit 區塊合一.

### workspace/applyEdit 的 sample request

https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#applyWorkspaceEditParams

```json
{
    "edit": {
        "documentChanges": [
            {
                "edits": [
                    {
                        "newText": "[!\n\n!]",
                        "range": {
                            "end": {
                                "character": 1,
                                "line": 0
                            },
                            "start": {
                                "character": 0,
                                "line": 0
                            }
                        }
                    },
                    {
                        "newText": "[!\n\n!]",
                        "range": {
                            "end": {
                                "character": 1,
                                "line": 1
                            },
                            "start": {
                                "character": 0,
                                "line": 1
                            }
                        }
                    }
                ],
                "textDocument": {
                    "uri": "file:///workspaces/gcl-all/gcl-vscode/example/a.gcl",
                    "version": 0
                }
            }
        ]
    },
    "label": "Resolve Spec"
}
```
