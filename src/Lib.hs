module Lib
    (
    ) where

import Control.Applicative (Alternative(..))
import Data.List (isSuffixOf)



data Markdown = Heading Int String | Paragraph String deriving (Show, Eq)
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    f `fmap` Parser g = Parser $ \input -> do
        (x,input') <- g input
        return (f x, input')


instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    Parser p1 <*> Parser p2 = Parser $ \input -> do
        (f, input') <- p1 input
        (a, input'') <- p2 input'
        return (f a, input'')

instance Monad Parser where
    (Parser x) >>= f = Parser $ \input -> do
       (y, input') <- x input
       runParser (f y) input'

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser x <|> Parser y =
        Parser $ \input -> x input <|> y input


charP :: Char -> Parser Char
charP ch = Parser f
    where
        f (x:xs)
            | x == ch = Just (x, xs)
            | otherwise = Nothing
        f [] = Nothing


stringP :: String -> Parser String
stringP = mapM charP

h1 :: Parser Markdown
h1 = headingOfLevel 1

h2 :: Parser Markdown
h2 = headingOfLevel 2

h3 :: Parser Markdown
h3 = headingOfLevel 3

h4 :: Parser Markdown
h4 = headingOfLevel 4

h5 :: Parser Markdown
h5 = headingOfLevel 5

h6 :: Parser Markdown
h6 = headingOfLevel 6

headingOfLevel :: Int -> Parser Markdown
headingOfLevel level = Parser $ \input ->
    if "# " `isSuffixOf` take (level + 1) input
    then Just (Heading level (takeWhile (/= '\n') (drop (level + 1) input)), dropWhile (/= '\n') input)
    else Nothing

headingParser = h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6

paragraphParser :: Parser Markdown
paragraphParser = Parser $ \input -> Just (Paragraph $ takeWhile (/= '\n') input, dropWhile (/= '\n') input)

markdownParser :: Parser Markdown
markdownParser = headingParser <|> paragraphParser
