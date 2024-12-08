module Parse () where
import Data.List (isPrefixOf)


-- if heading. read until \n
-- if normal text, read until \n
-- 
-- 

data Markdown = Heading Int String | Paragraph String deriving (Show, Eq)

type Zipper = (String, String)

readChar :: Zipper -> Zipper
readChar (xs, y:xy) = (y:xs, xy)
readChar (_, []) = ([], [])

h1 :: Zipper -> Maybe Markdown
h1 (tag, txt)
  | tag == " #" = Just $ Heading 1 txt
  | otherwise = Nothing


getPrefix :: String -> String
getPrefix txt = takeWhile (\x -> x /= '#') txt



parse :: String -> Maybe [Markdown]
parse txt = undefined
   