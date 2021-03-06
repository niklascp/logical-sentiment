{-# LANGUAGE ImplicitParams #-}

module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Control.Monad

import Parser
import CCG
import Annotate

import WordNet hiding (Word)

matchE :: String -> String -> Bool
matchE s s' = (map toLower s') == s      ||
              (map toLower s') == "it"   ||
              (map toLower s') == "they" ||
              (map toLower s') == "them"     

extract :: String -> SExpr -> [Float]
extract s (Fun s' j _ es) | matchE s s' = j:(concat $ map (extract s) es)
                          | otherwise   = (concat $ map (extract s) es)
extract s (Seq es)                      = (concat $ map (extract s) es)
extract _ _                             = []

analyse :: (Word -> Word) -> String -> (String, Int) -> IO ((Int, Maybe Float))
analyse annotationAlgorithm subject (sentence, index) = do
  tree <- runCc annotationAlgorithm sentence
  if (isJust tree) then do
    let sexpr = nodeExpr $ fromJust tree
    let r = extract subject sexpr
    let m = (maximum r) + (minimum r)
    if (null r) then return (index, Nothing)
    else if (m > 0) then
      return $ (index, Just $ maximum r)
    else if (m < 0) then
      return $ (index, Just $ minimum r)
    else
      return (index, Nothing)
  else
    return (index, Nothing)



main :: IO ()
main = do wne <- initializeWordNetWithOptions Nothing Nothing          

          -- Define positive concepts
          let adj_pos_list = [("good", 1), ("beautiful", 1), ("pleasant", 1),   ("clean", 1), ("quiet", 1), ("friendly", 1),   ("cheap", 1), ("fast", 1), ("large", 1),("nice",1)]
          let adj_neg_list = [("bad", 1),  ("hideous", 1),   ("unpleasant", 1), ("dirty", 1), ("noisy", 1), ("unfriendly", 1), ("expensive", 1), ("slow", 1), ("small", 1),("nasty",1)]

          -- Define intensifiers concepts
          let intensifiers = [("extreme", 1), ("much", 1), ("more", 1)]
          let qualifier = [("moderate", 1), ("little", 1), ("less", 1)]

          -- Load the synsets that corresponds to the words in the above lists.
          let s_pos = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) adj_pos_list)
          let s_neg = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) adj_neg_list)

          -- Load the synsets that corresponds to the words in the above lists.
          let intensifiers_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) intensifiers)
          let qualifier_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) qualifier)

          -- Print info about the seed concepts
          putStr "\n\n"

          putStr $ "Positive concepts:\n"
          putStr $ unlines $ map show s_pos
          putStr "\n"
          putStr $ "Negative concepts:\n"
          putStr $ unlines $ map show s_neg
          putStr "\n"
          putStr $ "Intensifying concepts:\n"
          putStr $ unlines $ map show intensifiers_ss
          putStr "\n"
          putStr $ "Qualifying concepts:\n"
          putStr $ unlines $ map show qualifier_ss

          putStr "\n\n"

          -- Build semantic networks and annotation environment
          putStr "Unfolding semantic networks...\n"
          let (adjMap, adjGraph) = (let ?wne = wne in unfoldG (\x -> (relatedBy Similar x ++ relatedBy SeeAlso x)) (s_pos ++ s_neg))
          let (scaleMap, scaleGraph) = (let ?wne = wne in unfoldG (relatedBy Similar) (intensifiers_ss ++ qualifier_ss))
          putStr $ "- Change network : " ++ (show $ Map.size adjMap) ++ " concepts.\n"
          putStr $ "- Scale network  : " ++ (show $ Map.size scaleMap) ++ " concepts.\n"

          let adjPosRoots = map (fromJust . flip Map.lookup adjMap) s_pos
          let adjNegRoots = map (fromJust . flip Map.lookup adjMap) s_neg
          let adj_fun = pAdj adjGraph adjPosRoots adjNegRoots . catMaybes . map (flip Map.lookup adjMap)

          let scaleIntensifiersRoots = map (fromJust . flip Map.lookup scaleMap) intensifiers_ss
          let scaleQualifierRoots = map (fromJust . flip Map.lookup scaleMap) qualifier_ss
          let scaleFun = pScale scaleGraph scaleIntensifiersRoots scaleQualifierRoots . catMaybes . map (flip Map.lookup scaleMap)

          let env = AnnotationEnv { 
            wnEnv = wne, 
            adjFun = adj_fun,
            scaleFun = scaleFun
          }

          -- Load review data
          reviewData <- liftM lines $ readFile "../Data/rooms_swissotel_chicago_a.txt"           
          
          -- Analyse
          result <- mapM (analyse (annotateWord env) "room") $ zip reviewData [1..]

          -- Print results
          putStr "\n\n"
          putStr "Results:\n"
          putStr $ unlines (map (\(i, r) -> (show i) ++ ": " ++ (show r)) result)
          putStr "\n\n"
          putStr "Finshed.\n"
          return ()
