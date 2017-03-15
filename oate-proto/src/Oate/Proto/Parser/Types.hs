{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Oate.Proto.Parser.Types where

import           Data.Map.Lazy    (Map)
import qualified Data.Map.Lazy    as Map

import           Control.Lens.TH

import           Oate.Proto.Types

data ParseState =
  ParseState
  { _parseStateDefnIds :: Map TypeName Identifier
  , _parseStateFileId :: Identifier
  }

makeFields ''ParseState
