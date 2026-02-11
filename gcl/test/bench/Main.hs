{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (foldl')
import GCL.Range (mkPos, mkRange)
import Language.LSP.Protocol.Types
  ( Position (Position),
    SemanticTokenAbsolute (SemanticTokenAbsolute),
    SemanticTokenTypes (SemanticTokenTypes_Variable),
    TextDocumentContentChangeEvent (TextDocumentContentChangeEvent),
    TextDocumentContentChangePartial (TextDocumentContentChangePartial),
    type (|?) (InL),
  )
import qualified Language.LSP.Protocol.Types as LSP
import Server.IntervalMap (IntervalMap)
import qualified Server.IntervalMap as IntervalMap
import Server.PositionMapping
  ( PositionDelta (..),
    PositionResult (..),
    mkDelta,
  )
import System.CPUTime
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Helpers: construct a realistic LSP change event

-- | Create a change event: insert a new line at the given (0-based) line
mkInsertLineChange :: Int -> TextDocumentContentChangeEvent
mkInsertLineChange line =
  TextDocumentContentChangeEvent $
    InL $
      TextDocumentContentChangePartial
        (LSP.Range (Position l 0) (Position l 0))
        Nothing
        "  new line\n"
  where
    l = fromIntegral line

-- | Create a change event: replace text within a line (0-based line, 0-based col)
mkReplaceChange :: Int -> Int -> Int -> TextDocumentContentChangeEvent
mkReplaceChange line colStart colEnd =
  TextDocumentContentChangeEvent $
    InL $
      TextDocumentContentChangePartial
        (LSP.Range (Position l cs) (Position l ce))
        Nothing
        "replaced"
  where
    l = fromIntegral line
    cs = fromIntegral colStart
    ce = fromIntegral colEnd

--------------------------------------------------------------------------------
-- Realistic eager update: apply PositionDelta to IntervalMap

eagerUpdateIntervalMap ::
  PositionDelta -> IntervalMap token -> IntervalMap token
eagerUpdateIntervalMap delta m =
  let entries = IntervalMap.toList m
      mapped = concatMap (mapEntry delta) entries
   in IntervalMap.fromList mapped
  where
    mapEntry delta ((startOrd, endOrd), token) =
      -- Reconstruct approximate GCL positions from posOrd
      -- posOrd = line * 10000000 + col
      let startLine = startOrd `div` 10000000
          startCol = startOrd `mod` 10000000
          endLine = endOrd `div` 10000000
          endCol = endOrd `mod` 10000000
          startLSP = Position (fromIntegral startLine) (fromIntegral startCol)
          endLSP = Position (fromIntegral endLine) (fromIntegral endCol)
       in case (toDelta delta startLSP, toDelta delta endLSP) of
            (PositionExact (Position newSL newSC), PositionExact (Position newEL newEC)) ->
              let newStartOrd =
                    fromIntegral newSL * 10000000
                      + fromIntegral newSC
                  newEndOrd =
                    fromIntegral newEL * 10000000
                      + fromIntegral newEC
               in [((newStartOrd, newEndOrd), token)]
            -- Entry overlaps with edit region -> remove it
            _ -> []

-- Realistic eager update: apply PositionDelta to SemanticTokens
eagerUpdateTokens ::
  PositionDelta -> [SemanticTokenAbsolute] -> [SemanticTokenAbsolute]
eagerUpdateTokens delta = concatMap mapToken
  where
    mapToken (SemanticTokenAbsolute l c len ty mods) =
      let pos = Position (fromIntegral l) (fromIntegral c)
       in case toDelta delta pos of
            PositionExact (Position newL newC) ->
              [SemanticTokenAbsolute (fromIntegral newL) (fromIntegral newC) len ty mods]
            _ -> [] -- token overlaps with edit -> remove

--------------------------------------------------------------------------------
-- Build realistic data

buildHoverInfos :: Int -> IntervalMap String
buildHoverInfos n =
  mconcat
    [ IntervalMap.singleton
        (mkRange (mkPos line col) (mkPos line (col + 5)))
        ("type info " ++ show i)
      | i <- [0 .. n - 1],
        let line = (i `div` 5) + 1 -- ~5 tokens per line, 1-based
            col = (i `mod` 5) * 10 + 1
    ]

buildDefinitionLinks :: Int -> IntervalMap String
buildDefinitionLinks n =
  mconcat
    [ IntervalMap.singleton
        (mkRange (mkPos line col) (mkPos line (col + 3)))
        ("def link " ++ show i)
      | i <- [0 .. n - 1],
        let line = (i `div` 2) + 1 -- ~2 links per line
            col = (i `mod` 2) * 20 + 1
    ]

buildSemanticTokens :: Int -> [SemanticTokenAbsolute]
buildSemanticTokens n =
  [ SemanticTokenAbsolute
      (fromIntegral line)
      (fromIntegral col)
      5
      SemanticTokenTypes_Variable
      []
    | i <- [0 .. n - 1],
      let line = i `div` 5
          col = (i `mod` 5) * 10
  ]

--------------------------------------------------------------------------------
-- Benchmarking helpers

timeIt :: String -> IO a -> IO a
timeIt label action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (1e6 :: Double) -- microseconds
  printf "  %-55s %10.1f us\n" label diff
  return result

forceIntervalMap :: IntervalMap String -> ()
forceIntervalMap m =
  let entries = IntervalMap.toList m
   in foldl' (\() ((a, b), v) -> a `seq` b `seq` v `seq` ()) () entries

forceTokens :: [SemanticTokenAbsolute] -> ()
forceTokens = foldl' (\() (SemanticTokenAbsolute l c len _ _) -> l `seq` c `seq` len `seq` ()) ()

--------------------------------------------------------------------------------
-- Benchmarks

-- | Single didChange: insert a line, update all three structures
benchAllStructures :: Int -> IO ()
benchAllStructures n = do
  let hover = buildHoverInfos n
      defLinks = buildDefinitionLinks (n `div` 3)
      tokens = buildSemanticTokens n
  forceIntervalMap hover `seq` forceIntervalMap defLinks `seq` forceTokens tokens `seq` return ()

  -- Simulate: user presses Enter at line 2 (near top, most entries need shifting)
  let change = mkInsertLineChange 2
      delta = mkDelta [change]

  _ <- timeIt (printf "all 3 structures  n=%-5d" n) $ do
    let hover' = eagerUpdateIntervalMap delta hover
        defLinks' = eagerUpdateIntervalMap delta defLinks
        tokens' = eagerUpdateTokens delta tokens
    return $! forceIntervalMap hover' `seq` forceIntervalMap defLinks' `seq` forceTokens tokens'
  return ()

-- | Single didChange with replace (not just insert)
benchReplace :: Int -> IO ()
benchReplace n = do
  let hover = buildHoverInfos n
  forceIntervalMap hover `seq` return ()

  -- Simulate: user replaces some text on line 5
  let change = mkReplaceChange 5 3 8
      delta = mkDelta [change]

  _ <- timeIt (printf "IntervalMap replace  n=%-5d" n) $ do
    let hover' = eagerUpdateIntervalMap delta hover
    return $! forceIntervalMap hover'
  return ()

-- | 10 rapid changes (simulating fast typing)
benchRapidChanges :: Int -> Int -> IO ()
benchRapidChanges n numChanges = do
  let hover = buildHoverInfos n
  forceIntervalMap hover `seq` return ()

  _ <- timeIt (printf "IntervalMap %2d rapid changes n=%-5d" numChanges n) $ do
    let result =
          foldl'
            ( \m i ->
                let change = mkInsertLineChange (2 + i)
                    delta = mkDelta [change]
                 in eagerUpdateIntervalMap delta m
            )
            hover
            [0 .. numChanges - 1]
    return $! forceIntervalMap result
  return ()

-- | Lookup after rebuild (sanity check)
benchLookup :: Int -> IO ()
benchLookup n = do
  let m = buildHoverInfos n
  let pos = mkPos (n `div` 10) 1
  _ <- timeIt (printf "IntervalMap lookup    n=%-5d" n) $ do
    let result = IntervalMap.lookup pos m
    return $! case result of
      Nothing -> ()
      Just v -> v `seq` ()
  return ()

main :: IO ()
main = do
  putStrLn "=== Eager Update Benchmark (realistic) ==="
  putStrLn ""
  putStrLn "Single didChange: update hoverInfos + definitionLinks + semanticTokens together:"
  mapM_ benchAllStructures [100, 500, 1000, 2000, 5000, 10000]
  putStrLn ""
  putStrLn "Single didChange: IntervalMap with replace (not just insert):"
  mapM_ benchReplace [100, 500, 1000, 2000, 5000, 10000]
  putStrLn ""
  putStrLn "10 rapid changes (fast typing), IntervalMap only:"
  mapM_ (\n -> benchRapidChanges n 10) [100, 500, 1000, 2000, 5000, 10000]
  putStrLn ""
  putStrLn "IntervalMap lookup (for reference):"
  mapM_ benchLookup [100, 500, 1000, 2000, 5000, 10000]
  putStrLn ""
  putStrLn "Reference: typical keystroke interval = 70,000 - 100,000 us"
