{-# LANGUAGE ImplicitParams #-}
module Parser (
    parseLexicon,
    parseTree,

    CcEnv,
    createCcEnv,
    closeCcEnv,
    runCcEnv
  ) where

-- Misc.
import Control.Monad
import Data.Char
import Data.Maybe

-- Parsec
import Text.ParserCombinators.Parsec hiding (token)
import Text.ParserCombinators.Parsec.Expr

-- Proccess handling
import System.Process
import GHC.IO.Handle

-- Data Structures
import CCG
import Annotate

pLexicon :: Parser [Word]
pLexicon = pLexiconEntry `endBy` newline

pLexiconEntry :: Parser Word
pLexiconEntry = do (string "w(")
                   many1 digit
                   (string ", ")
                   many1 digit
                   (string ", '")
                   t <- pToken
                   (string "', '")
                   lemma <- pToken
                   (string "', '")
                   pos <- pToken
                   (string "', '")
                   pToken
                   (string "', '")
                   pToken
                   (string "', '")
                   c <- pCategoryExpr
                   (string "').")
                   return $ Word t lemma pos c (Var "?")

pTree :: Lexicon -> Parser PTree
pTree l = do (string "ccg(")
             many1 digit
             (string ",")
             t <- pSubtree l
             (string ").")
             return t

pSubtree :: Lexicon -> Parser PTree
pSubtree l = do     try (pBRule l "fa"  $ \c t1 t2 -> PFwdApp   c (reduce $ App (nodeExpr t1) (nodeExpr t2)) t1 t2 )
                <|> try (pBRule l "ba"  $ \c t1 t2 -> PBwdApp   c (reduce $ App (nodeExpr t2) (nodeExpr t1)) t1 t2 )                
                <|> try (pBRule l "fc"  $ \c t1 t2 -> PFwdComp  c (reduce $ Abs "x" $ App (nodeExpr t1) (App (nodeExpr t2) (Var "x"))) t1 t2 )
                <|> try (pBRule l "bc"  $ \c t1 t2 -> PBwdComp  c (reduce $ Abs "x" $ App (nodeExpr t2) (App (nodeExpr t1) (Var "x"))) t1 t2 )
                <|> try (pBRule l "bx"  $ \c t1 t2 -> PBwdXComp c (reduce $ Abs "x" $ App (nodeExpr t2) (App (nodeExpr t1) (Var "x"))) t1 t2 )
                <|> try (pURule l "tr"  $ \c t     -> PFwdTR    c (reduce $ Abs "f" (App (Var "f") $ nodeExpr t)) t )
                <|> try (pConj l)
                <|> try (pConj'' l)
                <|> try (pConj''' l)
                <|> try (pLex l)
                <|> try (pWord l)
                <?> "subtree" 

pURule :: Lexicon -> String -> (Category -> PTree -> PTree) -> Parser PTree
pURule l f r = do (string f)
                  (string "('")
                  c <- pCategoryExpr
                  (string "',")
                  t <- pSubtree l
                  (string ")")
                  return $ r c t

pBRule :: Lexicon -> String -> (Category -> PTree -> PTree -> PTree) -> Parser PTree
pBRule l f r = do (string f)
                  (string "('")
                  c <- pCategoryExpr
                  (string "',")
                  t1 <- pSubtree l
                  (string ",")
                  t2 <- pSubtree l
                  (string ")")
                  return $ r c t1 t2

pConj :: Lexicon -> Parser PTree
pConj l = do r <- (try (string "conj") <|> (string "lp"))
             (string "('")
             t <- pToken
             (string "','")
             c1 <- pCategoryExpr
             (string "','")
             c2 <- pCategoryExpr
             (string "',")
             t1 <- pSubtree l
             (string ",")
             t2 <- pSubtree l
             (string ")")
             let t1' = case t1 of
                         PWord w   -> Just $ PWord $ annotateConj c1 $ w { category = (c1 :\ c1) :/ c1 }
                         otherwise -> Nothing
             if (isNothing t1') then
               unexpected ("Left child of a conjunction rule '" ++ r ++ "' should be a word.")
             else
               return $ PFwdApp c2 (reduce $ App (nodeExpr (fromJust t1')) (nodeExpr t2)) (fromJust t1') t2

pConj''' :: Lexicon -> Parser PTree
pConj''' l = do r <- (try (string "lp") <|> (string "ltc"))
                (string "('")
                c <- pCategoryExpr
                (string "',")
                t1 <- pSubtree l
                (string ",")
                t2 <- pSubtree l
                (string ")")
                let t1' = case t1 of
                            PWord w   -> Just $ PWord $ 
                              case r of
                                "lp"  -> annotateConj' $ w { category = c :/ c }
                                "ltc" -> annotateLtc l (w { category = c :/ (nodeCategory t2) })
                            otherwise -> Nothing
                if (isNothing t1') then
                  unexpected $ "Left child of a conjunction rule '" ++ r ++ "' should be a word."
                else
                  return $ PFwdApp c (reduce $ App (nodeExpr (fromJust t1')) (nodeExpr t2)) (fromJust t1') t2

pConj'' :: Lexicon -> Parser PTree
pConj'' l = do r <- (string "rp")
               (string "('")
               c1 <- pCategoryExpr
               (string "',")
               token <- optionMaybe (do { char '\''; t <- pToken; char '\''; char ','; return t }) 
               c2 <- optionMaybe (do { char '\''; c <- pCategoryExpr; char '\''; char ','; return c }) 
               t1 <- pSubtree l
               (string ",")
               t2 <- pSubtree l
               (string ")")
               let t2' = case t2 of
                           PWord w   -> Just $ PWord $ annotateConj' $ w { category = c1 :\ c1 }
                           otherwise -> Nothing
               if (isNothing t2') then
                 unexpected $ "Right child of a conjunction rule '" ++ r ++ "' should be a word."
               else
                 return $ PBwdApp c1 (reduce $ App (nodeExpr (fromJust t2')) (nodeExpr t1)) t1 (fromJust t2')

pLex :: Lexicon -> Parser PTree
pLex l = do (string "lex('")
            c1 <- pCategoryExpr
            (string "','")
            c2 <- pCategoryExpr
            (string "',")
            t <- pSubtree l
            (string ")")
            return $ PLexRaise c2 (nodeExpr t) t

pWord :: Lexicon -> Parser PTree
pWord l = do (string "lf(")
             (many1 digit)
             (string ",")
             wordIndex <- (many1 digit)
             (string ",'")
             c <- pCategoryExpr
             (string "')")             
             return $ PWord (l !! ((read wordIndex :: Int) - 1))

pToken :: Parser String
pToken = many1 $ upper <|> lower <|> digit <|> oneOf "_-$,.!?" <|> escaped <|> (char '’' >> return '\'')

escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
escapedChar code replacement = char code >> return replacement
codes        = ['\'', '"']
replacements = ['\'', '"']

pParens :: Parser a -> Parser a
pParens = between (char '(') (char ')')

pBrackets :: Parser a -> Parser a
pBrackets = between (char '[') (char ']')

pCategoryExpr :: Parser Category
pCategoryExpr = buildExpressionParser pCategoryOpTable pCategory

pCategoryOpTable :: OperatorTable Char st Category
pCategoryOpTable = [ [ op "/"  (:/) AssocLeft, 
                       op "\\" (:\) AssocLeft ] ]
                   where 
                     op s f a = Infix ( string s >> return f ) a

pCategory :: Parser Category
pCategory =         pParens pCategoryExpr
            <|>     (pCategory' "S"     S)
            <|> try (pCategory' "NP"    NP)
            <|>     (pCategory' "N"     N)
            <|>     (pCategory' "PP"    PP)
            <|>     (pCategory' "conj"  CONJ)
            <|>     (pCategory' "."     Punctuation)
            <|>     (pCategory' ","     Comma)
            <?> "category" 

pCategory' :: String -> (Agreement -> Category) -> Parser Category
pCategory' s c = do string s
                    a <- pAgreement
                    return $ c a

pAgreement :: Parser Agreement
pAgreement = option [] (pBrackets $ pFeature `sepBy` (char ','))

pFeature :: Parser Feature
pFeature =     try (string "dcl"   >> return SDcl )
           <|> try (string "adj"   >> return SAdj )
           <|> try (string "pt"    >> return SPt  )
           <|> try (string "nb"    >> return SNb  )
           <|> try (string "ng"    >> return SNg  )
           <|> try (string "em"    >> return SEm  )
           <|> try (string "inv"   >> return SInv )
           <|> try (string "pss"   >> return SPss )
           <|> try (string "b"     >> return SB   )
           <|> try (string "to"    >> return To   )
           <|> try (string "thr"   >> return FThr )
           <|> try (string "wq"    >> return FWq  )
           <|> try (string "qem"   >> return FQem )
           <|> try (string "q"     >> return FQ   )
           <|> try (string "for"   >> return FFor )
           <|> ( do { v <- many1 upper; return $ FVar v } )
           <?> "feature"

parseLexicon :: String -> Lexicon
parseLexicon str = 
  case parse pLexicon "Parse error:" str of
    Left e  -> error $ show e
    Right r -> r

parseTree :: Lexicon -> String -> (Maybe PTree)
parseTree l str = 
  case parse (pTree l) "Parse error:" str of
    Left e  -> Nothing -- error $ show e
    Right r -> Just r

getSection :: Handle -> String -> IO (Maybe String)
getSection h s = 
    do -- hWaitForInput h (-1)
       eof <- hIsEOF h
       if eof then
         return Nothing
       else do
         inpStr <- hGetLine h
         if inpStr == "" then
           return $ Just s
         else do 
           -- putStr ("Got line: " ++ inpStr ++ "\n")
           getSection h (s ++ inpStr ++ "\n")

data CcEnv = CcEnv { 
     --serverInHandle  :: Handle,
     --serverOutHandle :: Handle
}

createCcEnv :: IO (CcEnv)
createCcEnv = do
  -- Create C & C server process
  (_, _, _, serverHandle) <- 
     createProcess  (proc "bin/soap_server" [
         "--candc", "models",
         "--server", "localhost:9000",
         "--log", "log/candc.log",
         "--candc-parser-noisy_rules","false",
         "--candc-printer", "prolog"]) 
  putStr "Starting C & C server process...\n"
  return $ CcEnv -- inHandle outHandle

closeCcEnv :: CcEnv -> IO () 
closeCcEnv env = do
  --hClose (serverInHandle env)
  --hClose (serverOutHandle env)
  return ()

runCcEnv :: CcEnv -> (Word -> Word) -> String -> IO (Maybe PTree)
runCcEnv env a s = do 
  (Just inHandle, Just outHandle, _, processHandle) <- 
     createProcess  (proc "bin/soap_client_fix" [
         "--url", "http://localhost:9000"]) {
       std_in = CreatePipe,
       std_out = CreatePipe
     }
  hSetBuffering inHandle LineBuffering
  hSetBuffering outHandle LineBuffering

  putStr "\n" 
  putStr "Parsing:\n" 
  putStr s
  putStr "\n" 
  hPutStr inHandle s
  hPutStr inHandle "\n"  
  
  getSection outHandle ""            -- Discard comments
  getSection outHandle ""            -- Discard functor declarations
  tree    <- getSection outHandle "" -- Tree
  lexicon <- getSection outHandle "" -- Lexicon
   
  if (isJust tree) && (isJust lexicon) then
    do let l = map a $ parseLexicon $ fromJust lexicon
       putStr "\n"
       --putStr "Lexicon:\n"
       -- putStr lexicon  
       --putStr $ unlines $ map show (l)
       -- putStr "\n"
       -- putStr $ fromJust tree
       let tree' = filter (not . isSpace) $ fromJust tree
       let t = parseTree l tree'   
       -- if (isJust t)
       -- putStr "Tree:\n"  
       -- putStr tree'
       -- putStr "\n"
       return t
  else
    return Nothing