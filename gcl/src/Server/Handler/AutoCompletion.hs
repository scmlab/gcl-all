{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handler.AutoCompletion where

import Data.Text (Text)
import Language.LSP.Protocol.Types

-- To control the behaviour of autocomplete
-- see https://github.com/haskell/lsp/blob/bf95cd94f3301fe391093912e6156de7cb5c1289/lsp-types/src/Language/LSP/Types/Completion.hs
handler :: (Monad m) => Position -> Maybe CompletionContext -> m CompletionList
handler position completionContext
  | shouldTriggerUnicodeCompletion completionContext =
      return $ CompletionList True Nothing (unicodeCompletionItems position)
  | shouldTriggerDighole completionContext =
      return $ CompletionList False Nothing [specBracketsCompletionItem position]
  | otherwise =
      return $ CompletionList True Nothing []

-- https://github.com/haskell/lsp/blob/bf95cd94f3301fe391093912e6156de7cb5c1289/lsp-types/src/Language/LSP/Types/Completion.hs#L360-L371
-- trigger Unicode symbol completion when:
--  1. a backslash "\" is being typed
--  2. current completion is incomplete
shouldTriggerUnicodeCompletion :: Maybe CompletionContext -> Bool
shouldTriggerUnicodeCompletion (Just (CompletionContext CompletionTriggerKind_TriggerCharacter (Just "\\"))) =
  True
shouldTriggerUnicodeCompletion (Just (CompletionContext CompletionTriggerKind_TriggerForIncompleteCompletions _)) =
  True
shouldTriggerUnicodeCompletion _ =
  False

-- turn '?' into spec brackets '[! !]'
shouldTriggerDighole :: Maybe CompletionContext -> Bool
shouldTriggerDighole (Just (CompletionContext CompletionTriggerKind_TriggerCharacter (Just "?"))) =
  True
shouldTriggerDighole _ =
  False

-- list of `CompletionItem`s for unicode symbols
unicodeCompletionItems :: Position -> [CompletionItem]
unicodeCompletionItems position =
  mconcat
    [ makeItems
        position
        [" "]
        (Just CompletionItemKind_Operator)
        "\\"
        "\"\\\" Backward slash"
        "Inserting \"\\\"",
      makeItems
        position
        ["->", "rightarrow", "r", "to"]
        (Just CompletionItemKind_Operator)
        "→"
        "\"→\" Rightwards Arrow"
        "The Unicode variant of \"->\"",
      makeItems
        position
        ["/=", "neq", "!="]
        (Just CompletionItemKind_Operator)
        "≠"
        "\"≠\" Not Equal To"
        "The Unicode variant of \"/=\"",
      makeItems
        position
        [">=", "ge", "gte"]
        (Just CompletionItemKind_Operator)
        "≥"
        "\"≥\" Greater-Than or Equal To"
        "The Unicode variant of \">=\"",
      makeItems
        position
        ["<=", "le", "lte"]
        (Just CompletionItemKind_Operator)
        "≤"
        "\"≤\" Less-Than or Equal To"
        "The Unicode variant of \"<=\"",
      makeItems
        position
        ["==>", "Rightarrow", "implies", "R"]
        (Just CompletionItemKind_Operator)
        "⇒"
        "\"⇒\" Rightwards Double Arrow"
        "The Unicode variant of \"=>\"",
      makeItems
        position
        ["<==", "Leftarrow", "ffrom", "L"]
        (Just CompletionItemKind_Operator)
        "⇐"
        "\"⇐\" Leftwards Double Arrow"
        "The Unicode variant of \"<=\"",
      makeItems
        position
        ["&&", "wedge", "and"]
        (Just CompletionItemKind_Operator)
        "∧"
        "\"∧\" Logical And"
        "The Unicode variant of \"&&\"",
      makeItems
        position
        ["||", "vee", "or"]
        (Just CompletionItemKind_Operator)
        "∨"
        "\"∨\" Logical Or"
        "The Unicode variant of \"||\"",
      makeItems
        position
        ["~", "neg", "-"]
        (Just CompletionItemKind_Operator)
        "¬"
        "\"¬\" Not Sign"
        "The Unicode variant of \"~\"",
      makeItems
        position
        ["<|", "langle", "<"]
        (Just CompletionItemKind_Value)
        "⟨"
        "\"⟨\" Left Angle Bracket"
        "The Unicode variant of \"<|\"",
      makeItems
        position
        ["|>", "rangle", ">"]
        (Just CompletionItemKind_Value)
        "⟩"
        "\"⟩\" Right Angle Bracket"
        "The Unicode variant of \"|>\"",
      makeItems
        position
        ["min", "downarrow", "d"]
        (Just CompletionItemKind_Value)
        "↓"
        "\"↓\" Downwards Arrow"
        "The Unicode variant of \"min\"",
      makeItems
        position
        ["max", "uparrow", "u"]
        (Just CompletionItemKind_Value)
        "↑"
        "\"↑\" Upwards Arrow"
        "The Unicode variant of \"max\"",
      makeItems
        position
        ["sum", "Sigma", "sigma", "Gs"]
        (Just CompletionItemKind_Value)
        "Σ"
        "\"Σ\" Sum"
        "The Unicode variant of \"sum\"",
      makeItems
        position
        ["product", "Pi", "pi", "Gp"]
        (Just CompletionItemKind_Value)
        "∏"
        "\"∏\" Product"
        "The Unicode variant of \"product\"",
      makeItems
        position
        ["forall", "all", "A"]
        (Just CompletionItemKind_Value)
        "∀"
        "\"∀\" Forall"
        "The Unicode variant of \"forall\"",
      makeItems
        position
        ["exists", "ex", "E"]
        (Just CompletionItemKind_Value)
        "∃"
        "\"∃\" Exists"
        "The Unicode variant of \"exists\"",
      makeItems
        position
        ["<=>", "equiv", "iff", "==="]
        (Just CompletionItemKind_Operator)
        "≡"
        "\"≡\" If and only if"
        "The Unicode variant of \"<=>\"",
      makeItems
        position
        ["sconj"]
        (Just CompletionItemKind_Operator)
        "٭"
        "\"٭\" SConj"
        "SConj"
    ]

-- See https://github.com/haskell/lsp/blob/bf95cd94f3301fe391093912e6156de7cb5c1289/lsp-types/src/Language/LSP/Types/Completion.hs#L288
makeItems ::
  Position ->
  [Text] ->
  Maybe CompletionItemKind ->
  Text ->
  Text ->
  Text ->
  [CompletionItem]
makeItems position labels kind symbol detail doc = flip map labels $ \label ->
  CompletionItem
    label -- The label of this completion item.
    -- By default also the text that is inserted when selecting this completion.
    Nothing
    kind -- could be CIOperator, CiValue or whatever
    Nothing -- for marking deprecated stuff
    (Just detail) -- human-readable string
    (Just $ InL doc) -- also human-readable string
    Nothing -- deprecated
    Nothing -- select thie item when showing
    Nothing -- how to sort completion items
    Nothing -- how to filter completion items
    (Just symbol) -- the symbol we wanna insert
    (Just InsertTextFormat_PlainText) -- could be a "Snippet" (with holes) or just plain text
    Nothing -- how whitespace and indentation is handled during completion
    Nothing -- TextEdit to be applied when this item has been selected (but not completed yet)
    Nothing
    removeSlash -- TextEdit to be applied when this item has been completed
    (Just [" ", "\\"]) -- commit characters
    Nothing -- command to be executed after completion
    Nothing -- ???
  where
    Position ln col = position
    removeSlash =
      Just $ [TextEdit (Range (Position ln (col - 1)) position) ""]

-- tempReplaceWithSymbol = Just $ CompletionEditText $ TextEdit (Range position (Position ln (col + 1 ))) "symbol"

-- WARN: the following TODOs are added in the newer version of the library,
-- not sure what to put in these field so left as `Nothing` by default
-- see also: https://hackage.haskell.org/package/lsp-types-2.3.0.1/docs/Language-LSP-Protocol-Types.html#t:CompletionItem
specBracketsCompletionItem :: Position -> CompletionItem
specBracketsCompletionItem position =
  CompletionItem
    { _label = "?",
      _labelDetails = Nothing, -- TODO: not sure what to put here
      _kind = Just CompletionItemKind_Snippet,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Just (InL "Type \"?\" and a space to insert a spec."),
      _deprecated = Nothing,
      _preselect = Just True,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Just "[! !]",
      _insertTextFormat = Just InsertTextFormat_PlainText,
      _insertTextMode = Nothing,
      _textEdit = removeQuestionMark,
      _textEditText = Nothing, -- TODO: not sure what to put here
      _additionalTextEdits = Nothing,
      _commitCharacters = Just [" "],
      _command = Nothing,
      _data_ = Nothing -- TODO: not sure what to put here
    }
  where
    Position line column = position
    removeQuestionMark =
      Just $ InL $ TextEdit (Range (Position line (column - 1)) position) ""
