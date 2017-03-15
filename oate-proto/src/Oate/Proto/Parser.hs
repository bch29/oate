{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module Oate.Proto.Parser (parseProto) where

import           Control.Monad           (when)
import           Data.Functor            (($>))
import           Data.Word               (Word64)
import           Numeric                 (showHex)

import           Control.Lens
import qualified Crypto.Hash.SHA1        as SHA1
import           Data.ByteString         (ByteString)
import qualified Data.Map                as Map
import           Data.Serialize          as Serialize
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Text.Lens

import           Text.Megaparsec
import           Text.Megaparsec.Prim    (MonadParsec)

import           Oate.Proto.Parser.Lexer
import           Oate.Proto.Types

--------------------------------------------------------------------------------
--  Main parser
--------------------------------------------------------------------------------

parseProto :: (Token s ~ Char, MonadParsec e s m) => m Proto
parseProto = do
  consumeSpaces
  protoId <- parseIdentifier
  protoDefns <- some parseTypeDefn

  let definedTypeNames = foldMapOf (traversed . name) Set.singleton protoDefns

  when (Set.size definedTypeNames < length protoDefns) $
    fail "Two type definitions with the same name exist!"

  let typeIds = Map.fromSet (genIdFrom protoId) definedTypeNames

      -- Tries to find a name in the dictionary. If it fails, returns 'Left'
      -- with the source position where the name is used. This is useful for
      -- error reporting.
      findName dict (nm, sourcePos) =
        case Map.lookup nm dict of
          Just ident -> Right ident
          Nothing    -> Left (nm, sourcePos)

  case protoDefns & traversed . traversed %%~ findName typeIds of
    Right protoDefns' -> return $ Proto protoId protoDefns'
    Left (nm, sourcePos) ->
      do setPosition sourcePos
         fail ("The type name `" ++ Text.unpack (nm^._Wrapped) ++ "` is used but undefined")

--------------------------------------------------------------------------------
--  Sub-parsers
--------------------------------------------------------------------------------

type TempName = (TypeName, SourcePos)

parseIdentifier :: (Token s ~ Char, MonadParsec e s m) => m Identifier
parseIdentifier = lexeme $ Identifier <$> do
  _ <- char '@'
  val <- hexadecimal

  if val < fromIntegral (maxBound :: Word64)
    then return $ fromIntegral val
    else fail $ "Identifier with value " ++ showHex val "" ++ " is too large. It must fit into 64 bits."

parseTypeName :: (Token s ~ Char, MonadParsec e s m) => m TypeName
parseTypeName = lexeme $ TypeName <$> do
  nameHead <- upperChar
  nameTail <- many alphaNumChar
  return (Text.pack (nameHead : nameTail))

parseTempName :: (Token s ~ Char, MonadParsec e s m) => m TempName
parseTempName = do
  sourcePos <- getPosition
  tyName <- parseTypeName
  return (tyName, sourcePos)

parseVarName :: (Token s ~ Char, MonadParsec e s m) => m VarName
parseVarName = lexeme $ VarName <$> do
  nameHead <- lowerChar
  nameTail <- many (alphaNumChar <|> char '_')
  return (Text.pack (nameHead : nameTail))

parseField :: (Token s ~ Char, MonadParsec e s m) => m (Field' TempName)
parseField = do
  fName <- parseVarName
  _ <- colon
  fType <- parseType
  _ <- semicolon
  return $ Field fName fType

parseFields :: (Token s ~ Char, MonadParsec e s m) => m [Field' TempName]
parseFields = ((semicolon $> []) <|> (braces $ many parseField))

parseEnumClause :: (Token s ~ Char, MonadParsec e s m) => m (EnumClause' TempName)
parseEnumClause =
  EnumClause <$> parseTypeName
             <*> parseFields

parseType :: (Token s ~ Char, MonadParsec e s m) => m (PType' TempName)
parseType = choice
  [ PTRef <$> (ampersand *> parseType)
  , PTArray <$> brackets parseType
  , nameToType <$> parseTempName
  ]

parseTypeDefn :: (Token s ~ Char, MonadParsec e s m) => m (TypeDefn' TempName)
parseTypeDefn = do
  isStruct <- (symbol "struct" $> True) <|> (symbol "enum" $> False)
  tempName@(typeName, _) <- parseTempName

  typeRawDefn <-
    if isStruct
    then RDStruct <$> parseFields
    else RDEnum <$> braces (some parseEnumClause)

  return (TypeDefn tempName typeName typeRawDefn)

--------------------------------------------------------------------------------
--  Utility functions
--------------------------------------------------------------------------------

nameToType :: TempName -> PType' TempName
nameToType tempName@(theName, _) =
  let primMap = Map.fromList (map (\(x, y) -> (y, x)) primTypeStrings)
      prim = primMap ^. at (theName ^. _Wrapped . unpacked)

  in case prim of
       Just pt -> PTPrim pt
       Nothing -> PTDefined tempName

word64ToBS :: Word64 -> ByteString
word64ToBS = Serialize.runPut . Serialize.putWord64host

bsToWord64 :: ByteString -> Word64
bsToWord64 = either (const 0) id . Serialize.runGet getWord64host

-- | Generates an identifier for a type with the given name, by hashing it with
-- the top-level document identifier.
genIdFrom :: Identifier -> TypeName -> Identifier
genIdFrom (Identifier topLevelId) (TypeName nm) =
  let m = word64ToBS topLevelId `mappend` Text.encodeUtf8 nm
      hash = SHA1.hash m
  in Identifier (bsToWord64 hash)
