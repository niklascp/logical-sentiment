{-# LANGUAGE ImplicitParams #-}
module LexiconParser (
    Lexicon,

    parseLexicon
  ) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec hiding (token)
import Text.ParserCombinators.Parsec.Expr

import Lambda;
import CCG hiding (agreement,category,word);

type Lexicon = Map String [Word]

lexicon :: Parser [(String, [Word])]
lexicon = entry `endBy` newline

entry :: Parser (String, [Word])
entry = do t <- token
           (string " :- ")
           ws <- (word t) `sepBy` (string ", ")
           return $ (t, ws)

token :: Parser String
token = many1 $ lower <|> digit <|> oneOf "-'$."

word :: String -> Parser Word
word t = do c <- categoryExpr
            return $ Word t c (LVar "?")

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

categoryExpr :: Parser Category
categoryExpr = buildExpressionParser categoryOpTable category

categoryOpTable :: OperatorTable Char st Category
categoryOpTable = [ [op "/"  (:/) AssocLeft], 
                    [op "\\" (:\) AssocLeft] ]
                  where op s f assoc = Infix (do string s; return f) assoc

category :: Parser Category
category =     parens categoryExpr
           <|> category' "S" S
           <|> try (category' "NP" NP)
           <|> category' "N" N
           <|> category' "CONJ" CONJ
           <?> "category" 

category' :: String -> (Agreement -> Category) -> Parser Category
category' s c = do string s
                   space
                   a <- agreement
                   return $ c a

agreement :: Parser Agreement
agreement =     (many1 lower >> return [])
            <|> (brackets $ feature `sepBy` (char ','))

feature :: Parser Feature
feature =     try (string "Sg"    >> return Sg  )
          <|> try (string "Pl"    >> return Pl  )
          <|> try (string "Fst"   >> return Fst )
          <|> try (string "Snd"   >> return Snd )
          <|> try (string "Thrd"  >> return Thrd)
          <|> try (string "Gen"   >> return Gen )
          <|> try (string "Dcl"   >> return Dcl )
          <|> try (string "Adj"   >> return Adj )
          <|> try (string "Pt"    >> return Pt  )
          <|> (string "Ng"        >> return Ng  )
          <?> "feature"

parseLexicon :: String -> Lexicon
parseLexicon str = 
  case parse lexicon "Parse error:" str of
    Left e  -> error $ show e
    Right r -> Map.fromList r
