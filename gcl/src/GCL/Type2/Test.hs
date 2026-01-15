module GCL.Type2.Test where

import Control.Monad.Except
  ( ExceptT (ExceptT),
    runExceptT,
  )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Error (Error (..))
import qualified GCL.Type as Type
import qualified GCL.Type2.Elaborate as Type2
import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import qualified Syntax.Parser as Parser
import qualified Syntax.Typed as T

loadFromFile filepath = do
  source <- TE.decodeUtf8 <$> BS.readFile filepath
  simpleLoad filepath source

loadFromString source = do
  simpleLoad "DONTCARE" source

simpleLoad filepath source = runExceptT $ do
  concrete <- ExceptT $ parse filepath source
  -- lift $ print concrete
  abstract <- ExceptT $ toAbstract concrete
  -- lift $ print abstract
  -- typed <- ExceptT $ typecheck abstract
  typed2 <- ExceptT $ typecheck2 abstract
  -- lift $ print typed
  return ()
  where
    parse :: FilePath -> Text -> IO (Either Error C.Program)
    parse filepath' source' =
      case Parser.scanAndParse Parser.program filepath' source' of
        Left err -> do
          -- TODO: more error reporting here
          return $ Left (ParseError err)
        Right concrete -> return $ Right concrete

    toAbstract :: C.Program -> IO (Either Error A.Program)
    toAbstract concrete = return $ Right (C.toAbstract concrete)

    typecheck :: A.Program -> IO (Either Error T.Program)
    typecheck abstract = do
      case Type.runElaboration abstract mempty of
        Left err -> do
          -- TODO: more error reporting here
          return $ Left (TypeError err)
        Right typed -> return $ Right typed

    typecheck2 :: A.Program -> IO (Either Error T.Program)
    typecheck2 abstract =
      case Type2.runElaboration abstract mempty of
        Left err -> do
          -- TODO: more error reporting here
          return $ Left (TypeError err)
        Right typed -> return $ Right typed
