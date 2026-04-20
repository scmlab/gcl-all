現在有個功能想改.

目前 load / refine 在決定 edits 之後, 會在 memory 裡面把 edits apply 在 source code 上, 然後用這個 source code 再去做後續處理.
如果 edit 後的 source 有 type error, 則會用新的 line number 來回報錯誤. 

有沒有可能改成如果 edit 後的 source 有 error, 則不要送出 edit, 但是後面階段的 error 用編輯前的 line number 來回報?

---

想像中有兩種做法:
1. 如果編輯後的階段遇到錯誤, 則我們把這些錯誤訊息的座標轉回編輯前的座標
2. 強行用舊的資料結構做後續處理, 如果後續沒錯則將結果的座標改成編輯後的座標

這兩種作法都對編輯內容做了些假設.  
做法 1 假設比方說 "?" 被編輯成 "[!!]" 之後的新內容是不會在後續階段產生錯誤的, 所以不需轉換回去.  
做法 2 假設比方說 "?" 被編輯成 "[!!]" 之後的新內容不會產生東西, 不然會和後面的內容座標重疊.

這邊將採取做法 1. Happy path "先文字編輯, parse, 座標都是新的" 比較容易想像.

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

現在我們要求: 如果有任何 error, 則不動原本的 source, 也就是不會對前端送出任何 edits, 並用原本的座標顯示錯誤訊息.
但是文字取代後, 往下處理產生的 error 的座標會需要調整, 因為他們的座標是編輯後的而不是編輯前的.

我們的編輯應該會產生 `[(range, newText)]` 這種 list.
其中 range 是修改前的 line column 座標.
這些 range 是 "同時發生" 的, 也就是每個 range 都是用原始文字中的座標. (而不是前一個編輯後的座標, 這裡和 onDidChange 不一樣)
我們假設這個 list 中的 ranges 互不重疊, 且依照檔案中的順序出現.
我需要一個 function, 用這些資訊來轉換 errors.

---

在編輯後的 source 所產生的 error, 我們要把它轉回到編輯前的 source 上.
這個轉換可能需要特殊的對待.
例如以下編輯前後的五段範圍: R1 -> R1', R2 -> R2'
A R1 B R2 C
A R1' B R2' C
理想上 error range 如果單純住在 A B C 裡面, 那由後轉前是明確的.
當 error range 沾到 R1' 或 R2' 時, 正常來說應該是拋棄這個 range, 因為其座標不存在於舊的 source. 不過我們現在要盡量保留它, 以及做一些頭尾特別的處理:
(A, R1') 類型的 range 可能要變成 (A, R1尾)
(R1', B) 類型的 range 可能要變成 (R1頭, B)

注意: 這邊的編輯使用 [(Range, Text)] 格式. Range 全部指向原始座標, 同時編輯.

我想中間拆一個 function: 給 [(Range, Text)] , 新 pos , 頭尾mode 算出 舊 pos
如果新 pos 在 A 或 B 或 C 這種編輯前後不變的區段, 就傳回一個長度 1 的原本座標的Range
如果新 pos 在 R1' 或 R2' 這種編輯後的區段, 則
if mode == 頭 then 傳回對應舊 Range 的開頭 // 擴張
else // mode == 尾
  if pos 在新 Range 的頭 then 傳回對應舊 Range 的開頭 // 不動
  else 傳回對應舊 Range 的結尾 // 擴張

然後用上面那個 function 寫一個 function, 把新 range 的端點用頭尾兩個模式分別轉成舊 pos 再合成 舊座標的 range