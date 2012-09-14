{-# LANGUAGE ImplicitParams #-}

module Annotate where

import Data.Graph.Inductive
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import CCG
import WordNet hiding (Word)

import Debug.Trace

data AnnotationEnv = AnnotationEnv { 
     wnEnv  :: WordNetEnv,
     adjFun :: [SearchResult] -> Float,  -- rename to adjChange?
     scaleFun :: [SearchResult] -> Float -- rename to adjScale?
}

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


annotateWord :: AnnotationEnv -> Word -> Word
annotateWord env w@(Word { pos = pos })
  | isDet pos  = annotateDet env w
  | isAdj pos  = annotateAdj env w
  | isVerb pos = annotateVerb env w
  | isAdverb pos = annotateAdverb env w
  | isNoun pos = annotateNoun env w
  | isP pos    = annotateP env w
  | otherwise  = annotateAny env w