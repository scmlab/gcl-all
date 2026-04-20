現在有個功能想改.

目前 load / refine 在決定 edits 之後, 會在 memory 裡面把 edits apply 在 source code 上, 然後用這個 source code 再去做後續處理.
如果 edit 後的 source 有 type error, 則會用新的 line number 來回報錯誤. 

有沒有可能改成如果 edit 後的 source 有 error, 則不要送出 edit, 但是後面階段的 error 用編輯前的 line number 來回報?

---

source code 中的 "?" 隨著上下文會 parse 成 SpecQM 或是 HoleQM.
期待 statement 的地方會變成 SpecQM, 而期待 expression 的地方會變成 HoleQM.

我們 load 的邏輯是:
把文字一路處理到 concrete 以確認裡面有沒有 "?" (SpecQM 或 HoleQM)
如果有, 則用文字取代把 "?" 換成 `[!!]` 或 `{!!}` (Spec 或 Hole), 重新處理文字到 concrete
從 concrete 接著往下處理.

我們 refine 的邏輯是:
找到游標所在的 Spec 或 Hole (`[!!]` 或 `{!!}`)
用頭尾內縮的座標把裡面的文字切出來.
後續處理類似於 load, 但多了一些 input (如 type env), output 的座標也要轉換.

---

現在我們要求: 不論編輯前後只要有任何 error, 則不動原本的 source, 也就是不會對前端送出任何 edits, 並用原本的座標顯示錯誤訊息.
但是文字取代後, 往下處理產生的 error 的座標會需要調整, 因為他們的座標是編輯後的而不是編輯前的.

我們的編輯應該會產生 `[(range, newText)]` 這種 list.
其中 range 是修改前頭尾的 line column 座標.
這些 range 是 "同時發生" 的, 也就是每個 range 都是用原始文字中的座標. (而不是前一個編輯後的座標, 這裡和 onDidChange 不一樣)
我們假設這個 list 中的 ranges 互不重疊, 且依照檔案中的順序出現.
我需要一個 function, 用這些資訊來轉換 errors.

---

在編輯後的 source 所產生的 error, 我們要把它轉回到編輯前的 source 上.
這個轉換可能需要特殊的對待.
例如以下編輯前後的五段範圍: R1 -> R1', R2 -> R2'
A R1 B R2 C
A R1' B R2' C
理想上 error range 如果單純住在 A B C 裡面, 那由編輯後轉編輯前是明確的.
當 error range 沾到 R1' 或 R2' 時, 正常來說應該是拋棄這個 range, 因為其座標不存在於舊的 source. 不過我們現在要盡量保留它, 以及做一些頭尾特別的處理 -- 擴張:
(A, R1') 類型的 range 要變成 (A, R1尾)
(R1', B) 類型的 range 要變成 (R1頭, B)

舉例來說 
``` 
12abc3456
12pqrst3456
```
現在我們把 `abc` 編輯成 `pqrst`  
然後做後處理, 得到一個編輯後座標的 error range

考慮某個 error range 的開頭 (inclusive)  
如果開頭位置在 12 3456 這種沒被編輯的位置則不用動  
如果開頭位置在 pqrst 則回傳 a 的位置, 讓 range 擴張

考慮某個 error range 的結尾 (exclusive)  
如果結尾位置在 12 3456 這種沒被編輯的位置則不用動  
如果結尾位置在 p 也不用動  
如果結尾位置在 qrst 則回傳 3 的位置, 讓 range 擴張

