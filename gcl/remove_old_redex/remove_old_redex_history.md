# 舊版 Redex 機制（`RedexShell` / `RedexKernel`）— 生產歷史時間線

本文補充 [`remove_old_redex.md`](./remove_old_redex.md)，回答「這段 dead code 是什麼時候
變死的、歷史上是否曾被生產」。

## 三個核心問題與結論

### Q1. 現在 type2 / typed pipeline 有沒有生產 `RedexShell`？

**沒有。** Typed / `GCL.Type2` 這條 pipeline 從不建構 Abstract 的
`RedexShell` / `RedexKernel`。`src/GCL/Type2/Infer.hs` 對它們甚至是
`infer (RedexShell _ _) = undefined`——等於宣告「這種 node 不該出現在 typed 流程」。

### Q2. commit `3ca5132d` 是相關的分界點嗎？

**不是。** `3ca5132d`（2025-06-16）是 **gcl-all 這個 repo 的 README initial commit**，
那個時間點 repo 裡一個 `.hs` 都沒有——gcl/ 原始碼是後來用 git subtree 併進來的
（subtree merge：`3971f254 Add 'gcl/' from commit 'ce80f0e7...'`），自帶一段更早的歷史。

所以「在 3ca5132d 之前」在 code 層面沒有意義（當時還沒有任何 Haskell）。

至於 `RedexShell` **constructor 本身**：它在 Abstract `Expr` 型別裡存在已久，遠早於
3ca5132d，到今天都還在（只是已是 dead constructor）。

### Q3. 真正「停止生產」的分界點是哪個 commit？

**`6e50788e`（2024-08-01）**：

```
6e50788e  2024-08-01  [new] adapted WP.SP, Struct, Explanation, etc. to typed syntax.
```

這個 commit 把舊的 **`src/GCL/Substitution.hs`** 大砍（357 行 → 剩 stub），而那支檔案正是
**歷史上唯一真正生產 `RedexShell` / `RedexKernel` 的地方**。同一個 commit 把代換改到 typed
那側（新建 `src/Syntax/Typed/Instances/Substitution.hs`）。從此 redex 不再被生產。

被刪掉的生產碼（節錄自 `6e50788e` 的 diff，`-` 表示刪除）：

```haskell
-        index <- countUp
-        let e = RedexKernel ...
-        return $ RedexShell index e            -- ← 真正的建構：countUp 產生那個 Int id
-    RedexShell _ e -> RedexShell <$> countUp <*> subst mapping e
-    RedexShell _ e -> RedexShell <$> countUp <*> reduce (App e x' l1)
-    RedexShell i e -> RedexShell i <$> reduce e
```

可見舊機制的本質：在舊 type 系統下，`subst` / `reduce` 對 Abstract `Expr` 做代換時，會用
`countUp`（一個遞增計數器）產生 `Int` id，把代換結果包成 `RedexShell` / `RedexKernel` node
存進 tree——這就是 redex 的「生產」。

## 完整時間線

| 時間 | commit | 事件 | `RedexShell` 生產狀態 |
|---|---|---|---|
| ~2024-08-01 前 | — | 舊 `GCL/Substitution.hs` 在跑（`subst`/`reduce` 用 `countUp` 造 redex） | ✅ **有生產**（舊 type 系統下） |
| 2024-08-01 | `6e50788e` | WP 改用 typed syntax，砍掉舊 Substitution 生產碼 | ❌ **停止生產** |
| 2025-06-16 | `3ca5132d` | gcl-all 的 README initial commit（無 .hs） | ❌（早已停產約 10 個月） |
| 2025-11-13 | `c60a358e` | 引入 `GCL.Type2`（"basic structure"） | ❌ |
| 2026-05-26 | `94f42642` | 新的 path-based redex + HTML（`Redex [Int]`）上線 | ❌ |

## 對「那時有生產嗎？」的直接回答

以 `3ca5132d`（2025-06-16）為時間點：**constructor 存在，但已經不生產了**——因為真正停產發生
在更早的 `6e50788e`（2024-08-01），早了將近 10 個月。

## 總結

這段 dead code **不是最近才死的**。它從 2024-08 WP 改用 typed syntax（`6e50788e`）那一刻起
就已經不再被生產，只是 constructor 與相關的 render / instance / `Sbst` inline 一直沒清掉而已。
這也佐證了 `remove_old_redex.md` 的判斷：移除它是安全的，且只是補上一個拖了很久的 cleanup。

---

# 設計動機與歷史脈絡：為什麼當初做在 Abstract 層？

## 兩個查證到的事實

**事實 1：舊架構整條 WP 就跑在 Abstract 層。**
`6e50788e` 之前，`src/GCL/WP.hs` 只 `import Syntax.Abstract`、完全沒有 `Syntax.Typed`；改動後
才多了 `import qualified Syntax.Typed as T`。換言之——**proof obligation 的計算與輸出，當年
本來就是 Abstract `Expr`**。

**事實 2：redex 機制的本名是「DisplaySubst」，2021 年就有了。**

```
92f5fd69  2021-10-08  [new] AST.Redex
5b5be1d0  2021-10-11  [change] Rename DisplaySubst => RedexStem
0bca2b2e  2021-10-18  [refactor] Memoization free vars of a SubstStem ...
```

原始名字 **DisplaySubst（顯示用的代換）** 直接點破用途：它不是為了「計算」，是為了「**顯示**」。

## 為什麼想在 Abstract 層生？

因為 redex 本質上是 **WP 輸出的「顯示問題」**，而當年 WP 的輸出就住在 Abstract 層。

WP 充滿代換，例如 `wp (x := E) Q = Q[x\E]`。若**急著（eagerly）做掉**這個代換，會得到一坨
又大又難讀的展開式。舊設計選擇「**先不做、原樣顯示 `Q[x\E]`，讓使用者點一下再一步步展開
（reduce）**」。要承載這種「尚未求值、可顯示、還帶一個給前端點擊的 `Int` id」的東西，就必須
在「WP 會產生、而且會被送去顯示」的那個表示法裡放一個 node——那就是 Abstract `Expr`。所以
`RedexShell` / `RedexKernel` 自然做成 Abstract 的 constructor。

## 不需要到 elaborated 嗎？

**當年確實不需要**，有兩個層面：

1. **架構上不需要**：舊世界的 type check（`GCL.Type`）是一個**獨立的驗證 pass**，WP 並沒有
   消費一個「elaborated / typed」的中間表示——WP 直接吃 Abstract、吐 Abstract。既然沒有 typed
   表示參與 WP / 顯示流程，redex 當然只能落在 Abstract。
2. **本質上也不需要**：要**顯示**一個代換 `Q[x\E]`，根本用不到型別資訊。redex 是純粹的呈現
   需求，型別對「畫出這個式子」沒有幫助，所以「為了做 redex 而先 elaborate」在當時沒有動機。

## 那為什麼現在變成在 typed 層？

不是因為 redex「需要」型別，而是因為**整條 WP 在 `6e50788e`（2024-08）被遷移到 typed syntax**。
WP 的輸出改成 Typed `Expr`，顯示流程跟著搬家，於是 redex 的顯示機制也得在 typed 側**重做一次**
——也就是後來（`94f42642`, 2026-05）的 path-based `redexRT` + `Redex [Int]`。

舊的 Abstract redex 就是在這次遷移中被「孤兒化」的：WP 不再產生它，但 constructor 和處理它的
程式碼沒人順手刪，於是變成現在要清的 dead code。

## 一句話總結

> redex 原本叫 DisplaySubst，是 WP 輸出的「延遲顯示代換」功能；舊架構 WP 整條跑在 Abstract 層、
> type check 是獨立 pass，顯示代換用不到型別，所以它自然做在 Abstract。2024-08 WP 遷到 typed
> syntax 後（`6e50788e`），這套顯示機制在 typed 側重做，Abstract 版就成了 dead code。
