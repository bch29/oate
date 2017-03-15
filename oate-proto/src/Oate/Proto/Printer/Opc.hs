{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Oate.Proto.Printer.Opc (pprProto) where

import           Data.List              (intersperse, lookup)
import           Data.Monoid            ((<>))
import           Data.String            (IsString (..))
import           Numeric                (showHex)

import           Control.Lens
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           Oate.Proto.Types
import           Oate.Proto.Printer

pprProto :: Proto -> Text
pprProto = toText . pprProto'

type NameMap = Map Identifier TypeName

buildNameMap :: Proto -> NameMap
buildNameMap proto =
  let pairs = map (\defn -> (defn ^. identifier, defn ^. name)) (proto ^. defns)
  in Map.fromList pairs

pprProto' :: Proto -> PrettyPrinter
pprProto' proto =
  let nameMap = buildNameMap proto
  in pprIdentifier (proto ^. identifier) <>
     newline <> newline <>
     mconcat (intersperse newline (map (pprTypeDefn nameMap) (proto ^. defns)))

pprTypeDefn :: NameMap -> TypeDefn -> PrettyPrinter
pprTypeDefn nameMap d = case d ^. raw of
  RDEnum cs ->
    "enum " <>
    fromText (d ^. name._Wrapped) <>
    pprClauses nameMap cs <>
    newline
  RDStruct fs ->
    "struct " <>
    fromText (d ^. name._Wrapped) <>
    pprFields nameMap fs <>
    newline

pprFields :: NameMap -> [Field] -> PrettyPrinter
pprFields _  [] = " {}"
pprFields nameMap fs
  = mappend " "
  . braces
  . mconcat
  . intersperse newline
  . map (pprField nameMap)
  $ fs

pprField :: NameMap -> Field -> PrettyPrinter
pprField nameMap f =
  fromText (f ^. name._Wrapped) <> ": " <> pprPType nameMap (f ^. pType) <> ";"

pprClauses :: NameMap -> [EnumClause] -> PrettyPrinter
pprClauses nameMap =
  mappend " " . braces . mconcat . intersperse newline . map (pprClause nameMap)

pprClause :: NameMap -> EnumClause -> PrettyPrinter
pprClause nameMap c =
  fromText (c ^. name._Wrapped) <>
  pprFields nameMap (c ^. fields)

pprPType :: NameMap -> PType -> PrettyPrinter
pprPType nameMap (PTDefined typeId)
  | Just (TypeName typeName) <- nameMap ^. at typeId = fromText typeName
  | otherwise = error ("Identifier found that wasn't in map. This is a bug. The identifier was " ++ show typeId)
pprPType _ (PTPrim primType) = pprPrimType primType
pprPType nameMap (PTRef ty) = "&" <> pprPType nameMap ty
pprPType nameMap (PTArray ty) = "[" <> pprPType nameMap ty <> "]"

pprPrimType :: PrimType -> PrettyPrinter
pprPrimType pt | Just str <- lookup pt primTypeStrings = fromString str
               | otherwise = error ("Primitive type is not listed in strings: " ++ show pt)

pprIdentifier :: Identifier -> PrettyPrinter
pprIdentifier (Identifier x) = "@" <> fromString (showHex x "")
