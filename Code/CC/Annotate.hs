{-# LANGUAGE ImplicitParams #-}

module Annotate where

import Control.Monad.ST
import Data.STRef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Graph.Inductive

import CCG
import WordNet hiding (Word)

data AnnotationEnv = AnnotationEnv { 
     wnEnv  :: WordNetEnv,
     adjFun :: [SearchResult] -> Float,  -- rename to adjChange?
     scaleFun :: [SearchResult] -> Float -- rename to adjScale?
}

-- Parameters for the annotation
omega = 100
n = 2

-- | Unfold the graph using the given relation and seeds.
unfoldG :: (Ord a) => (a -> [a]) -> [a] -> (Map a Node, Gr a Int)
unfoldG r seeds = runST $ unfoldST r seeds

-- | State trasformer for unfolding graphs.
unfoldST :: (Ord a) => (a -> [a]) -> [a] -> ST s (Map a Node, Gr a Int)
unfoldST r seeds =
  do mapRef    <- newSTRef Map.empty    -- Map from Item to Node
     nodesRef  <- newSTRef []           -- List of Node/[Edge] pairs
     idRef     <- newSTRef 0            -- Counter for indexing nodes
     -- Recursively visits n
     let visit n = 
           do -- Test if n has already been visited
              test <- (return . Map.lookup n =<< readSTRef mapRef)
              case test of
                Just v  -> return v
                Nothing -> 
                  do -- Get next id for this item
                     i <- readSTRef idRef
                     modifySTRef idRef (+1)
                     -- Update item/node map
                     modifySTRef mapRef (Map.insert n i)
                     -- Recursively visit related items
                     ks <- mapM visit $ r n 
                     let ns = ((i,n), [(i,k,1) | k <- ks])
                     modifySTRef nodesRef (ns:)
                     return i
     -- Visit seeds
     mapM visit seeds
     -- Read resuls and return map/graph-pair
     list <- readSTRef nodesRef         
     nodeMap <- readSTRef mapRef
     let nodes = [n | (n, _) <- list]
     let edges = concat [es | (_, es) <- list]
     return (nodeMap, mkGraph nodes edges)

-- | Polarity value for adjective graphs
pAdj :: Real b => Gr a b -> [Node] -> [Node] -> [Node] -> Float
pAdj gr pns nns qns = (sum $ distAdj nns qns) - (sum $ distAdj pns qns)
                      where
                        distAdj :: [Node] -> [Node] -> [Float]
                        distAdj sns qns = take n $ sort [normAdj (length (sp sn qn gr) - 1) | sn <- sns, qn <- qns]
                        
                        normAdj :: Int -> Float
                        normAdj x | x < 0 || x > 10 = omega / (fromIntegral n)
                                  | otherwise       = (fromIntegral x / 10) * (omega / (fromIntegral n))

-- | Polarity value for scale graphs
pScale :: Real b => Gr a b -> [Node] -> [Node] -> [Node] -> Float
pScale gr pns nns qns = 2**(sum $ distScale nns qns) - (sum $ distScale pns qns)
                        where
                          distScale :: [Node] -> [Node] -> [Float]
                          distScale sns qns = take n $ sort [normScale (length (sp sn qn gr) - 1) | sn <- sns, qn <- qns]
                          
                          normScale :: Int -> Float
                          normScale x | x < 0 || x > 10 = 1 / (fromIntegral n)
                                      | otherwise       = (fromIntegral x / 10) * (1 / (fromIntegral n))


concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 x = let nonEmpty = filter (not . null) x
            in (map head nonEmpty) ++ concat2 (map tail nonEmpty)

shiftTerm :: SExpr -> Int -> SExpr
shiftTerm (Abs x t) k'      = Abs x (shiftTerm t k')
shiftTerm (Fun f j k ts) k' = Fun f j k' ts

isDet pos    = pos == "DT"
isAdj pos    = (take 2 pos) == "JJ"
isVerb pos   = (take 2 pos) == "VB"
isAdverb pos = (take 2 pos) == "RB"
isNoun pos   = (take 2 pos) == "NN" || pos == "PRP"
isP pos      = pos == "IN" || pos == "TO" || pos == "WDT"

annotateDet env w@(Word { category = c, token = t, lemma = l })
  | c =? NP [] :/ N [] =
    w { expr = lid } 
  | otherwise = 
    annotateAny env w

annotateNoun env w@(Word { category = c, token = t, lemma = l })
  | c =? N [] :/ N [] = 
    w { expr = (Abs "x" $ Seq [Fun l 0 0 [], Var "x"]) } -- Part of multi lexical noun  
  | otherwise         = 
    annotateAny env w

annotateVerb env w@(Word { category = c, token = t, lemma = l })
  | c =? (S [SDcl] :\ NP []):/(S [SAdj] :\ NP []) =
    w { expr = lid } -- Linking verb
  | otherwise = annotateAny env w

annotateAdj env w@(Word { category = c, lemma = l }) 
  | (S [SAdj] :\ NP []) =? c || (NP [] :/ NP []) =? c || (N [] :/ N []) =? c = -- TODO: Do we need any check?
    let query = (let ?wne = (wnEnv env) in search l Adj AllSenses)
        value = (adjFun env) query
    in  w { expr = (Abs "x" $ Change (Var "x") value) }
  | otherwise =
    annotateAny env w

annotateAdverb env w@(Word { category = c, lemma = l })
  | arg c =? res c = 
    let -- Try to lookup adjective pertainyms 
        query = (let ?wne = (wnEnv env) in (concat2 $ map (relatedBy Pertainym) (search l Adv AllSenses)) ++ (search l Adj AllSenses))
        scaleValue  = (scaleFun env) $ filter ((==) Adj . srPOS) $ query
        changeValue = (adjFun env) $ filter ((==) Adj . srPOS) $ query
    in  if scaleValue /= 0 then
          w { expr = (Abs "x" $ Scale (Var "x") $ scaleValue) } 
        else
          w { expr = (Abs "x" $ Change (Var "x") $ changeValue) } 
  | otherwise =
    annotateAny env w

annotateP env w@(Word { category = c })
  | (NP [] :\ NP []) :/ (S [] :/ NP []) =? c =
    w { expr = Abs "x" $ Abs "y" $ ImpactChange (App (Var "x") (Var "y")) 1 }
  | (NP [] :\ NP []) :/ (S [] :\ NP []) =? c =
    w { expr = Abs "x" $ Abs "y" $ ImpactChange (App (Var "x") (Var "y")) 2 }
  | (NP [] :\ NP []) :/ NP [] =? c =
    w { expr = shiftTerm (expr w') 2 }
  | otherwise = 
    w'
  where w' = annotateAny env w

annotateAny _ w@(Word { token = t, category = c, lemma = l }) =
  w { expr = constructTerm xyzVars [] c }
  where 
    constructTerm :: [String] -> [(SExpr, Category)] -> Category -> SExpr
    constructTerm vs ts c = case c of
      r :\ a -> constructAbstraction vs ts a r
      r :/ a -> constructAbstraction vs ts a r
      _      -> Fun l 0 0 $ reverse $ [v | (v, _) <- ts]

    constructAbstraction :: [String] -> [(SExpr, Category)] -> Category -> Category -> SExpr
    constructAbstraction (v:vs) ts a r = -- a = tau_alpha, r = tau_beta
      let t = Var v -- NP
          cond1 = any (\(_, t) -> (Just a) =? arg t) ts -- types where a migth be used as argument
          cond2 = any (\(_, t) -> arg a =? (Just t)) ts -- types where a migth be used as function
          term = if cond1 then map (\(t', c') -> if Just a =? arg c' then (App t' t, fromJust $ res c') else (t', c')) ts else
                 if cond2 then map (\(t', c') -> if arg a =? Just c' then (App t t', fromJust $ res a) else (t', c')) ts else
                 (t,a):ts
                 
      in 
        Abs v (constructTerm vs (term) r) 

-- | Special annotation for C&C conj-rule
annotateConj :: Category -> Word -> Word
annotateConj cat w@(Word { lemma = l }) =
  w { expr = Abs "x" $ Abs "y" $ constructTerm [] cat }
  where 
    newVar used = head $ dropWhile (`elem` used) $ (iterate (++ "'") "z")
    constructTerm :: [String] -> Category -> SExpr
    constructTerm used c = case c of
      a :\ _ -> let v = newVar used in Abs v (constructTerm (v:used) a)
      a :/ _ -> let v = newVar used in Abs v (constructTerm (v:used) a)
      _      -> Seq $ map (\term -> foldr (flip App) term $ map Var used) [Var "x", Var "y"]

-- | Auxiliary function for annotation for C&C lp/rp-rule
flatten :: Category -> [Category]
flatten (a :\ b) = b:flatten a
flatten (a :/ b) = b:flatten a
flatten a        = [a]

-- | Special annotation for C&C lp/rp-rule
annotateConj' :: Word -> Word
annotateConj' w@(Word { lemma = l, category = c }) =
  let f = flatten c
      c1 = f !! 0
      c2 = f !! 1
      cr = f !! 2
  in
    case length f of
      1 -> error "Annotate Conj: We expect least t -> t."
      2 -> w { expr = lid }  -- Dummy conjection, just return identity, for instance , and . in "funny , and happy ."
      _ -> w { expr = Abs "x" $ Abs "y" $ constructTerm [] c2 }     
  where 
    newVar used = head $ dropWhile (`elem` used) $ zVars
    constructTerm :: [String] -> Category -> SExpr
    constructTerm used c = case c of
      a :\ _ -> let v = newVar used in Abs v (constructTerm (v:used) a)
      a :/ _ -> let v = newVar used in Abs v (constructTerm (v:used) a)
      _      -> Seq $ map (\term -> foldr (flip App) term $ reverse $ map Var used) [Var "x", Var "y"]

-- | Special annotation for C&C ltc-rule
annotateLtc :: Lexicon -> Word -> Word
annotateLtc _ w@(Word { token = t, category = c, lemma = l }) =
  w { expr = constructTerm xyzVars [] c }
  where 
    constructTerm :: [String] -> [(SExpr, Category)] -> Category -> SExpr
    constructTerm vs ts c = case c of
      r :\ a -> constructAbstraction vs ts a r
      r :/ a -> constructAbstraction vs ts a r
      _      -> Seq $ reverse $ [v | (v, _) <- ts]

    constructAbstraction :: [String] -> [(SExpr, Category)] -> Category -> Category -> SExpr
    constructAbstraction (v:vs) ts a r = -- a = tau_alpha, r = tau_beta
      let t = Var v -- NP
          cond = any (\(_, t) -> (Just a) =? arg t) ts -- types where a migth be used as argument
          term = if cond then map (\(t', c') -> if Just a =? arg c' then (App t' t, fromJust $ res c') else (t', c')) ts else
                 (t,a):ts                 
      in 
        Abs v (constructTerm vs (term) r) 


annotateWord :: AnnotationEnv -> Word -> Word
annotateWord env w@(Word { pos = pos })
  | isDet pos  = annotateDet env w
  | isAdj pos  = annotateAdj env w
  | isVerb pos = annotateVerb env w
  | isAdverb pos = annotateAdverb env w
  | isNoun pos = annotateNoun env w
  | isP pos    = annotateP env w
  | otherwise  = annotateAny env w