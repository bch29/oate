{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Oate.Proto.Types where

import           Data.Data       (Data)
import           GHC.Generics    (Generic)

import           Data.Text       (Text)
import           Data.Word       (Word64)

import           Control.Lens.TH

data Proto =
  Proto
  { _protoIdentifier :: Identifier
  , _protoDefns      :: [TypeDefn]
  }
  deriving (Show, Eq, Ord, Data, Generic)

newtype Identifier = Identifier Word64
  deriving (Show, Eq, Ord, Data, Generic)

newtype TypeName = TypeName Text
  deriving (Show, Eq, Ord, Data, Generic)

newtype VarName = VarName Text
  deriving (Show, Eq, Ord, Data, Generic)

data Field' id =
  Field
  { _field'Name  :: VarName
  , _field'PType :: PType' id
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data EnumClause' id =
  EnumClause
  { _enumClause'Name   :: TypeName
  , _enumClause'Fields :: [Field' id]
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data PType' id
  = PTDefined id
  | PTPrim PrimType
  | PTRef (PType' id)
  | PTArray (PType' id)
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data RawDefn' id
  = RDEnum [EnumClause' id]
  | RDStruct [Field' id]
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data TypeDefn' id =
  TypeDefn
  { _typeDefn'Identifier :: id
  , _typeDefn'Name       :: TypeName
  , _typeDefn'Raw        :: RawDefn' id
  }
  deriving (Show, Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

data PrimType
  = BTInt32
  | BTInt64
  | BTUInt32
  | BTUInt64
  | BTFloat32
  | BTFloat64
  | BTText
  deriving (Show, Eq, Ord, Data, Generic)

primTypeStrings :: [(PrimType, String)]
primTypeStrings =
  [ (BTInt32, "Int32")
  , (BTInt64, "Int64")
  , (BTUInt32, "UInt32")
  , (BTUInt64, "UInt64")
  , (BTFloat32, "Float32")
  , (BTFloat64, "Float64")
  , (BTText, "Text")
  ]

type Field = Field' Identifier
type EnumClause = EnumClause' Identifier
type PType = PType' Identifier
type RawDefn = RawDefn' Identifier
type TypeDefn = TypeDefn' Identifier

{- [NOTE] Identifiers

Most of the types in this module are polymorphic over the type of identifier.
But 'Proto' is not. Why is this?

Since types can be declared after use, when parsing a proto we don't always know
the identifiers associated with each type until the whole document is parsed.
But we would still like to build the AST directly while we parse, without using
different types entirely.

So during parsing, 'TypeName's are used as identifiers directly. Then, in the
final step, identifiers are generated for each type name and they are replaced
in the syntax tree.
-}

makeWrapped ''Identifier
makeWrapped ''TypeName
makeWrapped ''VarName

makeFields ''Field'
makeFields ''EnumClause'
makePrisms ''PType'
makePrisms ''RawDefn'
makeFields ''TypeDefn'
makePrisms ''PrimType
makeFields ''Proto
