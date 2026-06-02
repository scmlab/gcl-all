# 移除舊版 Redex 機制（`RedexShell` / `RedexKernel`）— 安全性查證與結論

## 背景

GCL 有兩套「redex（可化簡點）」標記機制：

- **舊：Abstract 語法側** — `RedexShell` / `RedexKernel` 是 `Syntax.Abstract.Expr` 的
  constructor，把「已記錄但尚未執行的代換」當作 **實體 node 塞進 Expr tree**，並用一個
  `Int` id 供前後端指認。render 時經 `Render/Syntax/Abstract.hs` 變成 `Sbst Int Inlines`
  這個 inline。
- **新：Typed 語法側** — `Typed.Expr` 裡 **沒有** redex node；改由 `Syntax/Typed/Reduce.hs`
  的 `redexRT` 從一個普通 `Expr` 即時算出一棵平行的 `RT`（`RTree Bool`），用 **path `[Int]`**
  定位 redex。render 經 `Render/Syntax/Typed.hs` 的 `handleExprRZ` → `Redex [Int] Inlines`
  → `inlinesToHtml` 產生 `<span class="gcl-redex" data-redex="0,1">`。

新機制是在 commit `94f42642`（2026-05-26，"render Proof Obligation expressions as HTML
with clickable redex paths"）一次引入的。PO 的顯示與點擊互動現已 **完全由新機制接手**。

## 結論

**可以安全刪除舊的 `RedexShell` / `RedexKernel`，且這是真正的 dead code，而非「暫時沒用到」。**

最關鍵的證據：**代換器 `subst` 已不再生產 redex**。舊設計中代換 `P[x\a]` 會把結果包成
`RedexShell`/`RedexKernel`；現在 `Syntax/Abstract/Instances/Substitution.hs` 的 `subst`
是 **直接、eager 的代換**，那兩個 redex case 已退化為：

```haskell
subst _ (RedexKernel _ _ _ _) = error "not knowing what is going on here"
subst _ (RedexShell _ _)      = error "not knowing what is going on here"
```

連歷史上唯一的生產源頭（代換器）都已改成「視為不該出現的輸入」。全 repo 確認沒有任何地方
**建構** 這兩個 constructor。

## 查證結果（各面向）

| 查證項目 | 結果 |
|---|---|
| 生產端（建構 `RedexShell`/`RedexKernel`） | ❌ 完全沒有（只剩 `GoToDefn.hs` 一行註解） |
| `subst` 是否還產 redex | ❌ 已改 eager，兩個 case 是 `error` |
| `FromJSON Expr`（Haskell 解碼依賴） | ❌ 不存在 |
| 前端 TS 是否依賴這些型別 | ❌ 無（前端只認新的 `gcl-redex` / `data-redex` HTML） |
| `ToJSON Expr` | generic 自動衍生，但既然產不出這些值，JSON 永遠不含這些 tag → 移除無影響 |
| PO 現在的 render 路徑 | 走 Typed 的 `renderPOPredRZ`（`Server/ToClient.hs:154`、`Render/Predicate.hs:86`），與 Abstract redex 無關 |

## 刪除時必須同步移除的「消費端」（共 7 處，否則編譯錯）

> 重新查證（2026-06-02）：以下 7 處行號與內容已對著現行 code 逐一核對無誤；移除這兩個
> constructor 後各函式仍 exhaustive（皆為逐 constructor 列舉、無 catch-all 需求）。

刪 constructor 必須同步移除所有 pattern match clause。這些 clause 全是 no-op / error /
render，移除都安全：

1. `src/Syntax/Abstract/Types.hs:183-196` — constructor 本體（`RedexKernel` 183、`RedexShell` 193）
2. `src/Render/Syntax/Abstract.hs:87,95` — `handleExpr` 兩個 render case（注意 95-96 行的
   `RedexShell` case 內含唯一的 `substE` 生產者）
3. `src/Server/GoToDefn.hs:215-217` — 含 215 行註解 + 兩個 `-> return ()`（no-op）
4. `src/GCL/Common.hs:162,163` — `freeVars`
5. `src/GCL/Type2/Infer.hs:164,165` — `undefined`
6. `src/Syntax/Abstract/Instances/Located.hs:79,80` — `maybeRangeOf`
7. `src/Syntax/Abstract/Instances/Substitution.hs:42,43` — `error`

## 可順手一起清的相依物（連鎖 dead code）

- **`type Mapping = Map Text Expr`**（`src/Syntax/Abstract/Types.hs:208`）+
  **`instance Render Mapping`**（`src/Render/Syntax/Abstract.hs:109`）：`Mapping` 只被
  `RedexKernel` 用到（`Types.hs:189` 的 `NonEmpty Mapping` 欄位，會隨 constructor 一起消失；
  render 端只有 `RedexKernel` 那個 case 會 `map render` 這些 `Mapping`）。
  - 注意：`src/Server/Refine.hs` 的 `holeMapping` 是同名的 local 變數、**不是** 這個型別，無關。
  - `src/GCL/WP/WP.hs:165-166` 有 `A.Mapping` 但**已被註解掉**（`-- toMapping :: ...`），是死註解，
    可順手清掉。
- **`Sbst` / `substE`**（`src/Render/Element.hs`）：`Sbst` 的唯一生產者就是
  `Render/Syntax/Abstract.hs:96` 的 `RedexShell` render。拔掉 `RedexShell` 後 `substE`
  變孤兒，`Sbst` 這個 `Inline` constructor 也可一併移除。
  - `Sbst` 與新的 `Redex` 是 **不同** 的 `Inline` constructor，刪 `Sbst` 不影響新機制。
  - 完整編輯點（共 6 處，重新查證 2026-06-02）：
    1. `Element.hs:17` — export list 的 `substE`（注意：`Sbst` constructor 本身**未** export，
       只有 smart constructor `substE` 在 export list）
    2. `Element.hs:164` — `elemIsEmpty (Sbst _ xs)`
    3. `Element.hs:191-192` — `substE` 型別簽章與定義
    4. `Element.hs:237` — `Sbst Int Inlines` constructor
    5. `Element.hs:260` — `pretty (Sbst _i xs)`
    6. `Element.hs:283` — `inlineToHtml (Sbst _ xs)`

## 提醒

1. **`gcl/proof_obligation/ShowExprAst.hs`** 有引用這兩個 constructor，但它位於 scratch 目錄
   （`proof_obligation/`，內含多個 `.md`），**不是 build target**（`package.yaml` / `.cabal`
   未收錄），所以 **不會擋編譯**。刪 constructor 後該 scratch 檔將無法獨立編譯（已過時）。
2. 建議分兩步：
   - **Step 1**：刪 `RedexShell`/`RedexKernel` + 上述 7 個消費端 + `Mapping` 型別與其
     `Render` instance，跑 `stack build` 確認。
   - **Step 2**：再決定是否連 `Sbst` / `substE` 一併收尾。

## 旁註（更正：2026-06-02 重新查證）

先前版本曾推測「`Render/Syntax/Abstract.hs` 整個 `handleExpr` 可能是 dead code」。**這個推測
不正確、已更正**：

- `Render/Syntax/Abstract.hs` **這個模組是活的**，不能整個刪。它被以下三處依賴：
  - `src/Render/Error.hs:13` — `import Render.Syntax.Abstract ()`（取其 `Render` instances）
  - `src/Render/Predicate.hs:11` — 同上
  - `src/Render/Syntax/Typed.hs:8` — `import Render.Syntax.Abstract hiding (handleExpr)`
    （Typed render 重用此模組的 helper / instance，僅排除 `handleExpr`）
- 換言之，本次只是從 `handleExpr` 移除兩個 redex case（再加上 secondary cleanup 的
  `instance Render Mapping`），**模組本身保留**。
- 「`instance Render A.Expr`（即 `handleExpr` 本身）是否仍可達」是個更窄、價值更低的獨立問題，
  與本次移除 `RedexShell`/`RedexKernel` 無關，不影響本計畫。

---

# `Sbst` inline 移除的專項查證（2026-06-02）

針對「移除 `Sbst` 是否真的安全」做了一次更深入的查證，重點在 `Inline` 有 `ToJSON`、**會不會
被序列化送到前端而造成 decode 問題**。結論：**完全安全**，而且比預期更安全。

## 關鍵發現：前端根本不接收 `Inline` 的 JSON

`Server/ToClient.hs` 定義了**自己的** client-facing 型別（對齊 TypeScript interface），欄位
都是字串，不是 `Inlines`：

```haskell
data Specification = Specification
  { preCondition  :: String      -- ← String，不是 Inlines
  , postCondition :: String }

data ProofObligation = ProofObligation
  { pred :: Text.Text }          -- ← HTML 字串（含 data-redex）
```

轉換時就地把 server 資料變成字串送出：

```haskell
convertSpec ... = Specification { preCondition = show $ pretty specPreCond, ... }  -- 走 Pretty → String
convertPO  ... = ProofObligation { pred = inlinesToHtml (renderPOPredRZ e), ... }  -- 走 HTML → Text
```

前端（`Specification.ts`、`ProofObligation.ts`）就是把這些字串直接插進 HTML
（`<td>${specification.preCondition}</td>`、`<span>${proofObligation.pred}</span>`）。
整個 `gcl-vscode/src` 掃過，**沒有任何按 Inline tag（`Snpt`/`Parn`/`Horz`/`Sbst`/`Redex`…）
render 的程式碼**。

→ 因此 `instance ToJSON Inline` / `Inlines` / `Section` / `Block` 的 **JSON 序列化不參與前後端
通訊**，client 協定走的是 `String` / `Text`。

## 釐清（更正 2026-06-02）：純文字其實也經過 Inlines

上面 `convertSpec` 註解寫「走 Pretty → String」容易被誤解成「純文字不經過 Inlines」。**這不精確**：
純文字與 HTML **共用 `Expr → render → Inlines` 這前半段**，只差最後一步輸出。原因在 Typed `Expr`
的 `Pretty` instance 是委派給 Render 的：

```haskell
-- Pretty/Typed.hs
instance Pretty Expr     where pretty   = prettyPrec NoContext
instance PrettyPrec Expr where prettyPrec = fromRenderPrec
-- Pretty/Util.hs
fromRenderPrec n x = pretty (renderPrec n x)   -- 先 render 成 Inlines，再 Pretty Inlines
```

| | 前半段 | 後半段 |
|---|---|---|
| 純文字（舊 PO / 現在的 Spec） | `Expr → render → Inlines` | `Pretty Inlines → Doc → String` |
| HTML（2026-05 起的新 PO） | `Expr → renderExprRZ → Inlines` | `inlinesToHtml → HTML` |

差別只在：HTML 用 `renderExprRZ`（會把 redex 包成 `Redex` 標記），純文字用普通 `render`（不標 redex）。

所以「前端不消費 Inline 的 **JSON**」仍然成立、刪 `Sbst` 仍然安全；但 **`Inlines` 這個型別本身
是被用到的**（兩種輸出的共同中介），不可誤會成沒用。真正可能 legacy 的只是 `Inline` 的
**`ToJSON` instance**（序列化），而非 `Inlines` 型別。

## 三個長得像「subst」的東西別搞混

這次討論浮現一個重要釐清：`[x \ y]` 這個代換符號**不是 `Sbst` 畫的**。有三個不同層的東西：

| 名稱 | 是什麼 | 誰產生 | 狀態 |
|---|---|---|---|
| **`Subst Expr [(Name,Expr)]`** | **Typed `Expr` 的 constructor**，代表 lazy 代換 `e[x\y]` | WP（`syntaxSubst e = Subst e ...`） | **活著** |
| `Sbst Int Inlines` | `Inline` 的 wrapper，標記舊 Abstract redex 供互動 | 只有 Abstract render 的 `RedexShell` case | 2024-08 已死，本次刪除 |
| `Redex [Int] Inlines` | `Inline` 的 wrapper，標記 typed redex | Typed 的 `renderExprRZ` | 2026-05 新增 |

`[x \ y]` 是從 **Typed `Subst` 節點**用普通 inlines 畫出來的（`Render/Syntax/Typed.hs` 的
`Subst` case：`e <+> "[" <+> vs <+> "\\" <+> es <+> "]"`，那些括號/反斜線都是普通 `Text`）。
`Sbst`（inline）從頭到尾跟畫 `[x\y]` 無關——它只是舊 Abstract 機制額外**包在外面的互動標記**
（詳見下方「關鍵分別」）。

WP 在 2024-08 搬到 typed 後，PO 是 Typed `Expr`、由 **Typed render** 處理，而 Typed render
**從不產生 `Sbst`**。所以「加 HTML 之前」那段純文字時期看到的 `e[x\y]`，全程是
**Typed `Subst` 節點 → 普通 inlines → Pretty 文字**，沒有 `Sbst` 參與。

### 關鍵分別：特殊 inline 是「包在外面」，不是「生出 `[x\y]`」

`[x\y]` 的**可見字元**（`[`、`x`、`\`、`y`、`]`）是普通的 `Text` inline，由 render `Subst`
這個 **Expr** 節點時拼出來的（`OverloadedStrings` 把 `"["` 等字面量變成 `Text` inline）。
`Sbst` / `Redex` 這類特殊 inline **自己不貢獻任何可見字元**，只是（在需要互動時）**包在那串
普通 inlines 外面**的透明 wrapper。

兩條路徑對同一串 `[x\y]` 的處理：

- **純文字路徑**（`render` / `handleExpr`）：完全沒有特殊 inline 包裝，就是一串普通 `Text`：
  `e [ x \ y ]`。
- **HTML 路徑**（`renderExprRZ` / `handleExprRZ`）：因為 `redexRT (Subst …) = Node True …`
  把 `Subst` 標成 redex，那串普通 inlines 會**額外被 `Redex` 包一層**（純為可點擊）：
  `Redex [path] (e [ x \ y ])` → `<span class="gcl-redex" data-redex="…">e [ x \ y ]</span>`。
  `Redex` 只加 `<span>` 與 `data-redex`，**沒有**生出任何 `[x\y]` 字元；裡面仍是普通 `Text`。

舊的 `Sbst` 同理：透明 wrapper、只標記不畫內容（純文字裡 `pretty (Sbst _ xs) = pretty xs`，
完全透明）。

> 一句話：`[x\y]` 的**字形**來自 render Typed `Subst` (Expr) 節點所產生的普通 `Text` inline；
> `Sbst` / `Redex` 從不生出 `[x\y]`，它們只是 HTML 路徑上包在那串普通 inlines 外面的可點擊標記，
> 純文字路徑連這層 wrapper 都沒有。

### 兩階段管線（解釋上述「畫」與「輸出」發生在不同步驟）

```
階段一  Expr ──render / handleExpr / renderExprRZ──▶ Inlines   （讀 Expr，pattern match Subst 等）
階段二  Inlines ──Pretty Inlines / inlinesToHtml──▶ text / html （只讀 Inline，Expr 已不在）
```

- 階段一型別是 `Expr -> Inlines`：`Subst` 的字形（`[x\y]`）就是在這步被定下來的。
- 階段二型別是 `Inlines -> Doc` / `Inlines -> Text`：簽名裡**沒有 `Expr`**，只把階段一產生的
  inlines 攤平輸出。

所以**沒有任何一步「同時用 `Expr` 和 `Inlines` 產出文字」**：先 `Expr → Inlines`（階段一讀 Expr），
再 `Inlines → text/html`（階段二只讀 Inline）。

## 三重保險

1. **生產端早已沒了**：`Sbst` 的唯一來源 `substE` 只被 Step 1 移除的 `RedexShell` render case
   呼叫；沒有任何 `Sbst` 值會被產生。
2. **`pretty` / `inlineToHtml` 裡的 `Sbst` case 是 dead branch**：產不出 `Sbst` 值就永遠跑不到，
   刪掉等於沒事。
3. **就算 Inline JSON 真的有送（事實上沒有）**：aeson 的 `TaggedObject` 用 **constructor 名稱**
   當 tag，移除一個 constructor **不會改變其他 constructor 的編碼**——`Text`/`Redex`/`Horz`
   的 JSON 一模一樣。

## `Redex` 是否足以取代 `Sbst`？

是的——在 `Inline`（rendered output）這一層，新的 `Redex` 就是 `Sbst` 的後繼者，功能上完全足夠
取代它。這也正是為什麼刪掉 `Sbst` 不留任何功能缺口。

兩者扮演**同一種角色**：把一段 inline 包起來、標記成「可點擊、可化簡」的東西，並帶一個給前端
互動用的識別子。

| | `Sbst`（舊） | `Redex`（新） |
|---|---|---|
| 結構 | `Sbst Int Inlines` | `Redex [Int] Inlines` |
| 識別子 | 單一 `Int` id | path `[Int]`（更通用，支援巢狀、任意深度定位） |
| 來源機制 | Abstract 的 `RedexShell` | Typed 的 `redexRT` |
| 使用者體驗 | 點擊展開代換 | 點擊展開 redex（同樣的「逐步化簡顯示」） |

但「取代」要加一點精確度——時間線上它們**沒有重疊**：

- `Sbst`（連同舊 Abstract redex）**自 2024-08 起就已是 dead code**，沒人產生它了。
- `Redex`（新機制）是 **2026-05** 才加進來的。

所以嚴格講，不是「拿 `Redex` 換掉一個還活著的 `Sbst`」，而是：舊機制先死了很久、留下 `Sbst`
這具殘留；新機制後來在 typed pipeline 上把同樣的功能用 `Redex` 重做一遍；這次只是把那具早就
停產的殘留清掉。

## 附帶觀察（另一個獨立 cleanup，本次不做）

這次查證意外確認：前端**從不**消費 `Inline` 的 **JSON**（PO 走 HTML、Spec 走 Pretty 出來的
String，兩者最終都是 `String`/`Text`）。因此 `Render/Element.hs` 裡 `Section` / `Block` /
`Inline` / `Inlines` 的那些 **`ToJSON` instance** 很可能是 legacy，值得另開評估。

注意區分：可能 legacy 的是 **`ToJSON` instance（序列化）**，**不是 `Inlines` 型別本身**——
後者是純文字與 HTML 兩種輸出的共同中介（見上節「純文字其實也經過 Inlines」），仍在使用中。
此議題與本次移除無關。

---

# 執行紀錄（2026-06-02，已完成）

Step 1 與 Step 2 皆已執行，`stack build` 通過、**241 個測試全數通過**、src 內零殘留。

## 實際改動（9 個 `.hs`，純刪除約 -64 行）

| 檔案 | 改動 |
|---|---|
| `Syntax/Abstract/Types.hs` | 移除 `RedexKernel`/`RedexShell` constructor、`type Mapping`，及孤兒 import `NonEmpty`/`Map`/`Set` |
| `Render/Syntax/Abstract.hs` | 移除 `handleExpr` 兩個 redex case、`instance Render Mapping`，及孤兒 import `toList`/`Map` |
| `Render/Element.hs` | 移除 `Sbst` constructor、`substE`、export，及 `elemIsEmpty`/`pretty`/`inlineToHtml` 三個 case |
| `GCL/Common.hs` | 移除 `freeVars` 兩個 case |
| `GCL/Type2/Infer.hs` | 移除兩個 `undefined` case |
| `Syntax/Abstract/Instances/Located.hs` | 移除 `maybeRangeOf` 兩個 case |
| `Syntax/Abstract/Instances/Substitution.hs` | 移除兩個 `error` case |
| `Server/GoToDefn.hs` | 移除註解 + 兩個 no-op case |
| `GCL/WP/WP.hs` | 移除死註解 `toMapping`（含 `A.Mapping`） |

## 與計畫的差異

- 比原計畫多清掉：`Types.hs` 的 3 個孤兒 import、`Abstract.hs` 的 2 個孤兒 import、`WP.hs`
  的死註解（避免 `-Wall` 噪音）。
- `proof_obligation/ShowExprAst.hs`（未追蹤的本地 scratch 檔，非 build target）已由作者手動刪除。

## 建議 commit message

```
refactor: remove unused Abstract-layer redex mechanism (RedexShell/RedexKernel) and Sbst inline
```
