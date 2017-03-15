{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Oate.Proto.Printer
  ( PrettyPrinter (..)
  , fromText
  , toText
  , indentedBy
  , newline
  , newlines
  , braces
  ) where

import           Data.Monoid            ((<>))
import           Data.String            (IsString (..))

import           Data.Text              (Text)
import qualified Data.Text.Lazy         as Lazy
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder

{-|
A 'PrettyPrinter' is a function from an indentation level to a text builder.
-}
newtype PrettyPrinter = PPR { runPpr :: Int -> Builder }
  deriving (Monoid)

{-|
A pretty printer that increases the indentation level by the given amount while
printing a sub-expression.
-}
indentedBy :: Int -> PrettyPrinter -> PrettyPrinter
indentedBy moreIndent (PPR f) = PPR (\indent -> f (indent + moreIndent))

{-|
Prints the contents of a strict 'Text' object.
-}
fromText :: Text -> PrettyPrinter
fromText = PPR . const . Builder.fromText

{-|
Runs the pretty printer with indentation 0 at the top level, and outputs the
result as strict 'Text'.
-}
toText :: PrettyPrinter -> Text
toText = Lazy.toStrict . Builder.toLazyText . flip runPpr 0

{-|
Prints a new line. This should always by used over @'fromText' "\n"@ to make sure
indentation is correct.
-}
newline :: PrettyPrinter
newline = PPR $ \indent -> "\n" <> Builder.fromString (replicate indent ' ')

{-|
Prints the given number of newlines. Doesn't indent empty lines.
-}
newlines :: Int -> PrettyPrinter
newlines 0 = mempty
newlines n = mconcat (replicate (n - 1) "\n") <> newline

{-|
Prints the argument inside braces, indented by 4 spaces.
-}
braces :: PrettyPrinter -> PrettyPrinter
braces inside =
  "{" <>
  indentedBy 4 (newline <> inside) <>
  newline <>
  "}"

instance IsString PrettyPrinter where
  fromString = PPR . const . fromString
