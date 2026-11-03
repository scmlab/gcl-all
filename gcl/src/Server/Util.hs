{-# LANGUAGE OverloadedStrings #-}

module Server.Util (evalSMTProofs) where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as Text
import GCL.Predicate (PO (..))
import GCL.SMT.Proof (evaluateAsString)
import GHC.Clock (getMonotonicTimeNSec)
import Language.LSP.Server (getConfig)
import Server.Config (Config (..))
import Server.FileState (FileState (..))
import Server.Monad (ServerM, logText)
import Syntax.Typed.Reduce4 (reduceAll)

evalSMTProofs :: FileState -> ServerM FileState
evalSMTProofs fs = do
  config <- getConfig
  logText "Load: start evaluating proof obligations in SMT prover"
  started <- liftIO getMonotonicTimeNSec
  -- All proof obligations share one time budget and configuration snapshot.
  let deadline = toInteger started + smtTimeoutMs config * 1000000
  let defnEnv = fsDefinitions fs
  let tryReduce = either id id . reduceAll (smtReductionLimit config) defnEnv
  let reducedGlobalProps = map tryReduce (fsGlobalProps fs)
  po <-
    mapM
      ( \po -> do
          let reducedPred = tryReduce (poReducedPred po)
          now <- liftIO getMonotonicTimeNSec
          let remainingMs = (deadline - toInteger now) `div` 1000000
          smtResult <- liftIO $ evaluateAsString remainingMs reducedGlobalProps reducedPred
          return $ po {poSMTResult = Text.pack smtResult}
      )
      (fsProofObligations fs)
  logText "Load: evaluated all proof obligations in SMT prover"
  return $ fs {fsProofObligations = po}
