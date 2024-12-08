module Lib
    (
    ) where

import Control.Applicative (Alternative(..))



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
h1 = Parser $ \input ->
    if take 2 input == "# "
    then Just (Heading 1 (takeWhile (/= '\n') (drop 2 input)), dropWhile (/= '\n') input)
    else Nothing

h2 :: Parser Markdown
h2 = Parser $ \input ->
    if take 3 input == "## "
    then Just (Heading 2 (takeWhile (/= '\n') (drop 3 input)), dropWhile (/= '\n') input)
    else Nothing

h3 :: Parser Markdown
h3 = Parser $ \input ->
    if take 4 input == "### "
    then Just (Heading 3 (takeWhile (/= '\n') (drop 4 input)), dropWhile (/= '\n') input)
    else Nothing


p :: Parser Markdown
p = Parser $ \input -> Just (Paragraph $ takeWhile (/= '\n') input, dropWhile (/= '\n') input)

heading :: Parser Markdown
heading = h1 <|> h2 <|> h3 <|> p