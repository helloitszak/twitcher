module Twitcher.Table
  (
    Align(..)
  , Cell(..)
  , TableSet(..)
  , toText
  , toCsv
  , left
  , right
  , center
  , rows
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString, fromString)

data Align = AlignLeft | AlignCenter | AlignRight
data Cell = Cell { text :: Text
                 , alignment :: Align
                 }

left :: Text -> Cell
left t = Cell t AlignLeft

right :: Text -> Cell
right t = Cell t AlignRight

center :: Text -> Cell
center t = Cell t AlignRight

instance IsString Cell where
  fromString s = Cell (T.pack s) AlignLeft

instance Show Cell where
  show (Cell s _) = T.unpack s
    
newtype TableSet = TableSet [[Cell]] deriving (Show)


getLengths :: TableSet -> [[Int]]
getLengths (TableSet cells) = map (map (T.length . text)) $ cells

toCsv :: TableSet -> Text
toCsv (TableSet cells) = T.unlines $ map (T.intercalate "," . map quotifyCell) cells
  where
    quotifyCell = addQuoteIfNeeded . text

rows :: TableSet -> Int
rows (TableSet cells) = length cells

toText :: Text -> TableSet -> Text
toText sep set@(TableSet cells) = T.unlines $ map (T.intercalate sep . alignRow) cells
  where
    alignRow row = map alignCellTuple $ zip row cellLengths
    alignCellTuple (c,i) = alignCell c i ' '
    cellLengths = maxRowLengths $ getLengths set

addQuoteIfNeeded :: Text -> Text
addQuoteIfNeeded a
  | needsQuotes = wrapQuotes . replaceUnescapedQuotes $ a
  | otherwise = a
  where
    needsQuotes = T.any hasQuote a
    replaceUnescapedQuotes = (T.replace "\"" "\\\"")
    wrapQuotes = (T.cons '"') . (flip T.snoc $ '"')

hasQuote :: Char -> Bool
hasQuote '"' = True
hasQuote _ = False

zipWithPadding :: a -> [a] -> [a] -> [(a,a)]
zipWithPadding a (x:xs) (y:ys) = (x,y) : zipWithPadding a xs ys
zipWithPadding a [] ys = zip (repeat a) ys
zipWithPadding a xs [] = zip xs (repeat a)

maxTuple :: Ord a => (a,a) -> a
maxTuple (a,b) = max a b

maxRowLengths :: [[Int]] -> [Int]
maxRowLengths [] = []
maxRowLengths (row:rows) = foldr folder row rows
  where
    folder a b = map maxTuple $ zipWithPadding 0 a b

alignCell :: Cell -> Int -> Char -> Text
alignCell (Cell t AlignLeft) n c = T.justifyLeft n c t
alignCell (Cell t AlignCenter) n c = T.center n c t
alignCell (Cell t AlignRight) n c = T.justifyRight n c t
