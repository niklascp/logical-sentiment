{-# LANGUAGE ImplicitParams #-}
module Parser (
    Lexicon,
    
    parseLexicon,
    parseTree,
    runCC
  ) where

-- Misc.
import Control.Monad
import Debug.Trace
import Data.Char

-- Parsec
import Text.ParserCombinators.Parsec hiding (token)
import Text.ParserCombinators.Parsec.Expr

-- Proccess handling
import System.Process
import GHC.IO.Handle

-- Data Structures
import CCG;

type Lexicon = [Word]

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
                   return $ Word t lemma pos c (LVar "?")

pTree :: Lexicon -> Parser PTree
pTree l = do (string "ccg(")
             many1 digit
             (string ",")
             t <- pSubtree l
             (string ").")
             return t

pSubtree :: Lexicon -> Parser PTree
pSubtree l = do     try (pBRule l "fa" $ \c t1 t2 -> PFwdApp  c (reduce $ LApp (term t1) (term t2)) t1 t2 )
                <|> try (pBRule l "ba" $ \c t1 t2 -> PBwdApp  c (reduce $ LApp (term t2) (term t1)) t1 t2 )                
                <|> try (pBRule l "fc" $ \c t1 t2 -> PFwdComp c (reduce $ LAbs "x" $ LApp (term t1) (LApp (term t2) (LVar "x"))) t1 t2 )
                <|> try (pBRule l "bc" $ \c t1 t2 -> PBwdComp c (reduce $ LAbs "x" $ LApp (term t2) (LApp (term t1) (LVar "x"))) t1 t2 )
                <|> try (pBRule l "bx" $ \c t1 t2 -> PBwdXComp c (reduce $ LAbs "x" $ LApp (term t2) (LApp (term t1) (LVar "x"))) t1 t2 )
                <|> try (pURule l "tr" $ \c t     -> PFwdTR   c (reduce $ LAbs "f" (LApp (LVar "f") $ term t)) t )
                <|> try (pConj l)
                <|> try (pConj' l)
                <|> try (pConj'' l)
                <|> try (pConj''' l)
                <|> try (pLex l)
                <|> try (pWord l)
                -- <|> try (pBRule2 l "lp" $ \c1 c2 t1 t2 -> PFwdApp  c2 (reduce $ LApp (term t1) (term t2)) t1 t2 )
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
-- Special binary rule of commas and 
pBRule2 :: Lexicon -> String -> (Category -> Category -> PTree -> PTree -> PTree) -> Parser PTree
pBRule2 l f r = do (string f)
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
                   return $ r c1 c2 t1 t2

annotateConj cat w@(Word { lemma = l }) =
  w { lTerm = LAbs "x" $ LAbs "y" $ constructTerm [] cat }
  where 
    newVar used = head $ dropWhile (`elem` used) $ (iterate (++ "'") "z")
    constructTerm :: [String] -> Category -> LTerm
    constructTerm used c = case c of
      a :\ _ -> let v = newVar used in LAbs v (constructTerm (v:used) a)
      a :/ _ -> let v = newVar used in LAbs v (constructTerm (v:used) a)
      _      -> LSeq $ map (\term -> foldr (flip LApp) term $ reverse $ map LVar used) [LVar "x", LVar "y"]


flatten :: Category -> [Category]
flatten (a :\ b) = b:flatten a
flatten (a :/ b) = b:flatten a
flatten a        = [a]

annotateConj' w@(Word { lemma = l, category = c }) =
  let f = flatten c
      c1 = f !! 0
      c2 = f !! 1
      cr = f !! 2
  in
    case length f of
      1 -> error "Annotate Conj: We espect least t -> t."
      2 -> w { lTerm = lid }  -- Dummy conjection, just return identity, for instance , and . in "funny , and happy ."
      _ -> w { lTerm = LAbs "x" $ LAbs "y" $ constructTerm [] c2 }     
  where 
    newVar used = head $ dropWhile (`elem` used) $ zVars
    constructTerm :: [String] -> Category -> LTerm
    constructTerm used c = case c of
      a :\ _ -> let v = newVar used in LAbs v (constructTerm (v:used) a)
      a :/ _ -> let v = newVar used in LAbs v (constructTerm (v:used) a)
      _      -> LSeq $ map (\term -> foldr (flip LApp) term $ reverse $ map LVar used) [LVar "x", LVar "y"]


pConj :: Lexicon -> Parser PTree
pConj l = do (string "conj")
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
                         PWord w   -> PWord $ annotateConj c1 $ w { category = (c1 :\ c1) :/ c1 }
                         otherwise -> error "Left child of a conjunction should be a word?"
                 
             return $ PFwdApp c2 (reduce $ LApp (term t1') (term t2)) t1' t2

-- TODO : REWRITE THESE PUNCTIATION RULES

pConj' :: Lexicon -> Parser PTree
pConj' l = do (string "lp")
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
                          PWord w   -> PWord $ annotateConj c1 $ w { category = (c1 :\ c1) :/ c1 }
                          otherwise -> error "Left child of a conjunction should be a word?"
                  
              return $ PFwdApp c2 (reduce $ LApp (term t1') (term t2)) t1' t2

pConj''' :: Lexicon -> Parser PTree
pConj''' l = do (string "lp")
                (string "('")
                c <- pCategoryExpr
                (string "',")
                t1 <- pSubtree l
                (string ",")
                t2 <- pSubtree l
                (string ")")
                let t1' = case t1 of
                            PWord w   -> PWord $ annotateConj' $ w { category = c :/ c }
                            otherwise -> error "Left child of a conjunction should be a word?"
                    
                return $ PFwdApp c (reduce $ LApp (term t1') (term t2)) t1' t2

pConj'' :: Lexicon -> Parser PTree
pConj'' l = do (string "rp")
               (string "('")
               c <- pCategoryExpr
               (string "',")
               t1 <- pSubtree l
               (string ",")
               t2 <- pSubtree l
               (string ")")
               let t2' = case t2 of
                           PWord w   -> PWord $ annotateConj' $ w { category = c :\ c }
                           otherwise -> error "Left child of a conjunction should be a word?"
                   
               return $ PBwdApp c (reduce $ LApp (term t2') (term t1)) t1 t2'

pLex :: Lexicon -> Parser PTree
pLex l = do (string "lex('")
            c1 <- pCategoryExpr
            (string "','")
            c2 <- pCategoryExpr
            (string "',")
            t <- pSubtree l
            (string ")")
            return $ PNounRaise c2 (term t) t

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
pToken = many1 $ upper <|> lower <|> digit <|> oneOf "-$,."

pParens :: Parser a -> Parser a
pParens = between (char '(') (char ')')

pBrackets :: Parser a -> Parser a
pBrackets = between (char '[') (char ']')

pCategoryExpr :: Parser Category
pCategoryExpr = buildExpressionParser pCategoryOpTable pCategory

pCategoryOpTable :: OperatorTable Char st Category
pCategoryOpTable = [ [op "/"  (:/) AssocLeft], 
                     [op "\\" (:\) AssocLeft] ]
                   where op s f assoc = Infix ( do string s; return f ) assoc

pCategory :: Parser Category
pCategory =     pParens pCategoryExpr
            <|>     (pCategory' "S"     S)
            <|> try (pCategory' "NP"    NP)
            <|>     (pCategory' "N"     N)
            <|>     (pCategory' "PP"    PP)
            <|>     (pCategory' "conj"  CONJ)
            <|>     (pCategory' "."     Punctuation)
            <|>     (pCategory' ","     Comma)
            -- <|> try (pCategory' "CONJ" CONJ
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
           <|> try (string "inv"   >> return SInv )
           <|> try (string "pss"   >> return SPss )
           <|> try (string "b"     >> return SB   )
           <|> try (string "to"    >> return To   )
           <|> ( do { v <- many1 upper; return $ Var v } )
           <?> "feature"

parseLexicon :: String -> Lexicon
parseLexicon str = 
  case parse pLexicon "Parse error:" str of
    Left e  -> error $ show e
    Right r -> r

parseTree :: Lexicon -> String -> PTree
parseTree l str = 
  case parse (pTree l) "Parse error:" str of
    Left e  -> error $ show e
    Right r -> r

getSection :: Handle -> String -> IO String
getSection h s = 
    do inpStr <- hGetLine h
       if inpStr == ""
          then return s
          else do -- putStr ("Got line: " ++ inpStr ++ "\n")
                  getSection h (s ++ inpStr ++ "\n")

runCC :: (Word -> Word) -> String -> IO PTree 
runCC a s = do 
  -- Create C & C process pipeline
  -- (Just posIn, Just posOut, _, _) <- 
  --   createProcess (proc "bin/pos" [
  --       "--model", "models/pos"]) { 
  --     std_in = CreatePipe, 
  --     std_out = CreatePipe 
  --   }
  -- (Nothing, Just parserOut, _, _) <- 
  --   createProcess  (proc "bin/parser" [
  --       "--parser", "models/parser", 
  --       "--super", "models/super",
  --       "--log", "log/parser.log",
  --       -- "--noisy_rules",
  --       "--printer", "prolog"]) {
  --     std_in = UseHandle posOut,
  --     std_out = CreatePipe
  --      , std_err = CreatePipe 
  --   }
  (Just posIn, Just parserOut, _, _) <- 
     createProcess  (proc "bin/candc" [
         "--models", "models",
         "--log", "log/candc.log",
         --"--candc-parser-extra_rules","false",
         "--candc-parser-noisy_rules","false",
         -- "--candc-parser-extra_rules", "false",
         "--candc-printer", "prolog"]) {
       std_in = CreatePipe,
       std_out = CreatePipe
     }
  hSetBuffering posIn LineBuffering
  hSetBuffering parserOut LineBuffering

  -- Discard initial outpus from C & C          
  --mainloop parserOut ""
  putStr "C & C processes successfully spawned.\n"
  putStr "\n" 
  putStr "Parsing:\n" 
  putStr s
  putStr "\n" 
  hPutStr posIn s
  hPutStr posIn "\n"      
  hWaitForInput parserOut (-1)          
  getSection parserOut ""            -- Discard comments
  getSection parserOut ""            -- Discard functor declarations
  tree    <- getSection parserOut "" -- Tree
  lexicon <- getSection parserOut "" -- Lexicon
  
  let l = map a $ parseLexicon lexicon
  putStr "\n"
  putStr "Lexicon:\n"
  putStr lexicon  
  putStr $ unlines $ map show (l)
  putStr "\n"

  putStr tree
  let tree' = filter (not . isSpace) tree
  let t = parseTree l tree'   
  -- putStr "Tree:\n"  
  -- putStr tree'
  -- putStr "\n"
  
  hClose posIn
  -- hClose posOut
  hClose parserOut
  return t