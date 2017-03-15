{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}

module Oate.Proto.Parser.Lexer where

import           Control.Monad         (void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Prim  (MonadParsec)

skipLineComment :: (Token s ~ Char, MonadParsec e s m) => m ()
skipLineComment = L.skipLineComment "//"

skipBlockComment :: (Token s ~ Char, MonadParsec e s m) => m ()
skipBlockComment = L.skipBlockComment "/*" "*/"

consumeSpaces :: (Token s ~ Char, MonadParsec e s m) => m ()
consumeSpaces = L.space (void spaceChar) skipLineComment skipBlockComment

lexeme :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
lexeme = L.lexeme consumeSpaces

symbol :: (Token s ~ Char, MonadParsec e s m) => String -> m String
symbol = L.symbol consumeSpaces


braces :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
braces = between (symbol "{") (symbol "}")

parens :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
parens = between (symbol "(") (symbol ")")

brackets :: (Token s ~ Char, MonadParsec e s m) => m a -> m a
brackets = between (symbol "[") (symbol "]")

semicolon :: (Token s ~ Char, MonadParsec e s m) => m String
semicolon = symbol ";"

colon :: (Token s ~ Char, MonadParsec e s m) => m String
colon = symbol ":"

ampersand :: (Token s ~ Char, MonadParsec e s m) => m String
ampersand = symbol "&"

hexadecimal :: (Token s ~ Char, MonadParsec e s m) => m Integer
hexadecimal = L.hexadecimal
