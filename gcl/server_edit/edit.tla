---- MODULE edit ----
EXTENDS Sequences, Integers, TLC

CONSTANTS MaxVersion, MaxQueueLen \* 從 .cfg 傳入

VARIABLES
  clientVersion,       \* client 端的版本號
  serverVersion,       \* server 端最後看到的版本號
  c2sQueue,            \* client-to-server 的 FIFO queue:
                       \*   [source: {"user","server"}, version: Nat,
                       \*    ghost_prevVersion: Nat]
                       \*   其中 ghost_prevVersion 不存在於實際 LSP message 中, 僅供驗證用
  s2cQueue,            \* server-to-client 的 FIFO queue: [version: Nat]
  ghost_expectedPrevVersion  \* (ghost variable) 0: 不在等待;
                             \* >0: 期待下一個 didChange 的 ghost_prevVersion 等於此值
                             \* (即 applyEdit 成功時 client 的版本)

vars == <<clientVersion, serverVersion, c2sQueue, s2cQueue, ghost_expectedPrevVersion>>

Init ==
  /\ clientVersion = 1
  /\ serverVersion = 1
  /\ c2sQueue = <<>>
  /\ s2cQueue = <<>>
  /\ ghost_expectedPrevVersion = 0

\* --------- Actions ---------

\* 使用者在 client 編輯文件 (atomic: 改 text / 增 version / 送 didChange)
\* LSP 只保證 version 嚴格遞增, 不保證 +1, 因此用 \E 選取任意更大的版本
UserEdit ==
  /\ clientVersion < MaxVersion
  /\ Len(c2sQueue) < MaxQueueLen
  /\ \E newVer \in (clientVersion + 1)..MaxVersion :
       /\ clientVersion' = newVer
       /\ c2sQueue' = Append(c2sQueue,
            [source |-> "user", version |-> newVer,
             ghost_prevVersion |-> clientVersion])
  /\ UNCHANGED <<serverVersion, s2cQueue, ghost_expectedPrevVersion>>

\* Server 送出 applyEdit(serverVersion) 給 client
ServerSendApplyEdit ==
  /\ Len(s2cQueue) < MaxQueueLen
  /\ s2cQueue' = Append(s2cQueue, [version |-> serverVersion])
  /\ UNCHANGED <<clientVersion, serverVersion, c2sQueue, ghost_expectedPrevVersion>>

\* Client 收到並處理 server 的 applyEdit
\* 版本符合 => 成功, 改 text / 增 version / 送 didChange (atomic)
\* 版本不符 => 失敗, 丟棄
ClientReceiveApplyEdit ==
  /\ s2cQueue /= <<>>
  /\ LET msg == Head(s2cQueue)
     IN \/ \* 版本符合且未超出 model-checking 上界：成功
           /\ msg.version = clientVersion
           /\ clientVersion < MaxVersion
           /\ Len(c2sQueue) < MaxQueueLen
           /\ \E newVer \in (clientVersion + 1)..MaxVersion :
                /\ clientVersion' = newVer
                /\ c2sQueue' = Append(c2sQueue,
                     [source |-> "server", version |-> newVer,
                      ghost_prevVersion |-> clientVersion])
           /\ ghost_expectedPrevVersion' = clientVersion
        \/ \* 版本不符, 或版本符合但已達上界 (model artifact)：丟棄
           /\ \/ msg.version /= clientVersion
              \/ clientVersion >= MaxVersion
              \/ Len(c2sQueue) >= MaxQueueLen
           /\ UNCHANGED <<clientVersion, c2sQueue, ghost_expectedPrevVersion>>
  /\ s2cQueue' = Tail(s2cQueue)
  /\ UNCHANGED <<serverVersion>>

\* Server 收到 client 的 didChange, 更新 serverVersion
ServerReceiveDidChange ==
  /\ c2sQueue /= <<>>
  /\ serverVersion' = Head(c2sQueue).version
  /\ c2sQueue' = Tail(c2sQueue)
  /\ ghost_expectedPrevVersion' = 0
  /\ UNCHANGED <<clientVersion, s2cQueue>>

\* --------- Spec ---------

Next ==
  \/ UserEdit
  \/ ServerSendApplyEdit
  \/ ClientReceiveApplyEdit
  \/ ServerReceiveDidChange

Spec == Init /\ [][Next]_vars

\* --------- Property: NextDidChange ---------
\* 「applyEdit 成功後, server 收到的下一個 didChange 必來自此 edit」
\*
\* 當 ghost_expectedPrevVersion > 0 時 (applyEdit 剛成功),
\* c2sQueue 若非空, 其 Head 必為 server-sourced,
\* 且其 ghost_prevVersion 必等於 ghost_expectedPrevVersion,
\* 確認這個 didChange 確實是由那個 applyEdit 觸發的.
\*
\* 推理:
\*   applyEdit(V) 成功 => clientVersion 當時等於 V => 這之前沒有 user edit
\*   => c2sQueue 在此 didChange 之前沒有 user-sourced message
\*   => server-sourced didChange 在 queue 最前面, 且 prevVersion = V
NextDidChange ==
  (ghost_expectedPrevVersion > 0 /\ c2sQueue /= <<>>)
    => /\ Head(c2sQueue).source = "server"
       /\ Head(c2sQueue).ghost_prevVersion = ghost_expectedPrevVersion

====
\* sudo apt-get install openjdk-25-jdk-headless
\* install the VS Code extension "TLA+ (Temporal Logic of Actions)  v2026.3.270015"
\* cd /workspaces/gcl-all/gcl/server_edit && java -jar /home/vscode/.vscode-server/extensions/tlaplus.vscode-ide-2026.3.270015/tools/tla2tools.jar -config edit.cfg edit.tla 2>&1 | tail -20
