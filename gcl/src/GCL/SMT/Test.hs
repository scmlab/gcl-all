{-# LANGUAGE OverloadedStrings #-}

module GCL.SMT.Test where

import qualified Data.Text as Text
import Error (Error (..))
import qualified Syntax.Typed.Types as T
import Control.Monad.Except (ExceptT(..))
import qualified Hack
import Debug.Trace (trace)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Error.Class (MonadError(..))
import qualified Syntax.Parser as Parser
import Control.Monad.State (evalState)
import qualified Syntax.Concrete as C
import qualified Syntax.Abstract.Types as A
import GCL.SMT.Evaluator (evaluate)
import qualified GCL.Type2.ToTyped as Type2

test :: IO ()
test = do
  result <- simpleLoad "DONT CARE" "False => True"
  case result of
    Left err -> print $ show err
    Right typed -> do
      -- print $ show typed
      evaluate typed

simpleLoad filepath source = runExceptT $ catchError run handler
  where
    run = do
      concrete <- ExceptT $ parse filepath source
      -- lift $ print concrete
      abstract <- ExceptT $ toAbstract concrete
      -- lift $ print abstract
      -- typed <- ExceptT $ typecheck abstract
      typed <- ExceptT $ toTyped2 abstract
      -- lift $ print typed
      return typed
    handler err =
      trace (Hack.sshow err) (throwError err)

    parse :: FilePath -> Text.Text -> IO (Either Error C.Expr)
    parse filepath' source' =
      case Parser.scanAndParse Parser.expression filepath' source' of
        Left err -> do
          -- TODO: more error reporting here
          return $ Left (ParseError err)
        Right concrete -> return $ Right concrete

    toAbstract :: C.Expr -> IO (Either Error A.Expr)
    toAbstract concrete = return $ Right (evalState (C.toAbstract concrete) 0)

    toTyped2 :: A.Expr -> IO (Either Error T.Expr)
    toTyped2 abstract =
      case Type2.runToTyped abstract mempty of
        Left err -> do
          -- TODO: more error reporting here
          return $ Left (TypeError $ Hack.toOldError err)
        Right (typed, _) -> return $ Right typed
