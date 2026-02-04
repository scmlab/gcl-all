module Syntax.Parser.Types where

import Control.Monad.State
import Control.Monad.Writer (Writer, runWriter)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Endo (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void
import GCL.Range
import Syntax.Parser.Lexer
  ( Tok (..),
    TokStream,
  )
import Text.Megaparsec

type Parser = ParsecT Void TokStream M

-- | Source location bookkeeping
type M = StateT Bookkeeping (Writer (Endo [String]))

type ID = Int

data Bookkeeping = Bookkeeping
  { currentRange :: Maybe Range, -- current Range mark (Nothing = no range yet)
    lastToken :: Maybe Tok, -- the last accepcted token
    opened :: Set ID, -- waiting to be moved to the "logged" map
    -- when the starting position of the next token is determined
    logged :: Map ID (Maybe Range), -- waiting to be removed when the ending position is determined
    index :: Int, -- for generating fresh IDs
    indentStack :: [R Tok] -- Recording the tokens to indent/align to.
    -- see the section: "## State (Bookkeeping) actions for indentation"
  }

runM :: StateT Bookkeeping (Writer (Endo [String])) a -> (a, String)
runM f =
  let (a, pl) = runWriter $ evalStateT f (Bookkeeping Nothing Nothing Set.empty Map.empty 0 [])
   in (a, "parsing log:\n" <> intercalate "\n" (appEndo pl []))
