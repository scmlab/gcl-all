---- MODULE edit ----
EXTENDS Sequences, Integers, TLC

CONSTANTS MaxVersion, MaxQueueLen

VARIABLES
  clientVersion,       \* client 端的版本號
  serverVersion,       \* server 端最後看到的版本號
  c2sQueue,            \* client-to-server 的 FIFO queue: [source: {"user","server"}, version: Nat]
  s2cQueue,            \* server-to-client 的 FIFO queue: [version: Nat]
  ghost_awaitingServerEdit   \* (ghost variable) TRUE: applyEdit 成功了, 等待 server 收到對應的 didChange

vars == <<clientVersion, serverVersion, c2sQueue, s2cQueue, ghost_awaitingServerEdit>>

Init ==
  /\ clientVersion = 1
  /\ serverVersion = 1
  /\ c2sQueue = <<>>
  /\ s2cQueue = <<>>
  /\ ghost_awaitingServerEdit = FALSE

\* --------- Actions ---------

\* 使用者在 client 編輯文件 (atomic: 改 text / 增 version / 送 didChange)
UserEdit ==
  /\ clientVersion < MaxVersion
  /\ Len(c2sQueue) < MaxQueueLen
  /\ clientVersion' = clientVersion + 1
  /\ c2sQueue' = Append(c2sQueue, [source |-> "user", version |-> clientVersion + 1])
  /\ UNCHANGED <<serverVersion, s2cQueue, ghost_awaitingServerEdit>>

\* Server 送出 applyEdit(serverVersion) 給 client
ServerSendApplyEdit ==
  /\ Len(s2cQueue) < MaxQueueLen
  /\ s2cQueue' = Append(s2cQueue, [version |-> serverVersion])
  /\ UNCHANGED <<clientVersion, serverVersion, c2sQueue, ghost_awaitingServerEdit>>

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
           /\ clientVersion' = clientVersion + 1
           /\ c2sQueue' = Append(c2sQueue, [source |-> "server", version |-> clientVersion + 1])
           /\ ghost_awaitingServerEdit' = TRUE
        \/ \* 版本不符, 或版本符合但已達上界 (model artifact)：丟棄
           /\ \/ msg.version /= clientVersion
              \/ clientVersion >= MaxVersion
              \/ Len(c2sQueue) >= MaxQueueLen
           /\ UNCHANGED <<clientVersion, c2sQueue, ghost_awaitingServerEdit>>
  /\ s2cQueue' = Tail(s2cQueue)
  /\ UNCHANGED <<serverVersion>>

\* Server 收到 client 的 didChange, 更新 serverVersion
ServerReceiveDidChange ==
  /\ c2sQueue /= <<>>
  /\ serverVersion' = Head(c2sQueue).version
  /\ c2sQueue' = Tail(c2sQueue)
  /\ ghost_awaitingServerEdit' = FALSE
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
\* 當 ghost_awaitingServerEdit = TRUE 時 (applyEdit 剛成功),
\* c2sQueue 若非空, 其 Head 必為 server-sourced.
\*
\* 推理:
\*   applyEdit(V) 成功 => clientVersion 當時等於 V => 這之前沒有 user edit
\*   => c2sQueue 在 V message 之後沒有 user-sourced message
\*   => server-sourced didChange 在 queue 最前面
NextDidChange ==
  (ghost_awaitingServerEdit /\ c2sQueue /= <<>>) => Head(c2sQueue).source = "server"

====
