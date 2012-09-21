{-# LANGUAGE ImplicitParams #-}

module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph.Inductive
import Data.Maybe
import Data.Char
import Control.Monad

import Parser
import CCG
import Annotate

import Pretty

import WordNet hiding (Word)
import qualified WordNet.Prims as P

import Debug.Trace

extract :: String -> SExpr -> [Float]
extract s (Fun s' j _ es) | (map toLower s') == s      ||
                            (map toLower s') == "it"   ||
                            (map toLower s') == "they"    = j:(concat $ map (extract s) es)
                          | otherwise             = (concat $ map (extract s) es)
extract s (Seq es)                                = (concat $ map (extract s) es)
extract _ _                                       = []

analyse :: (Word -> Word) -> CcEnv -> String -> (String, Int) -> IO (Maybe Float)
analyse annotationAlgorithm ccEnv subject (sentence, index) = do
  tree <- runCcEnv ccEnv annotationAlgorithm sentence
  if (isJust tree) then do
    let sexpr = nodeExpr $ fromJust tree
    putStr $ show sexpr
    return (Just $ sum $ extract subject sexpr)
  else
    return Nothing

main :: IO ()
main = do wne <- initializeWordNetWithOptions Nothing Nothing          
          ccEnv <- createCcEnv

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
          result <- mapM (analyse (annotateWord env) ccEnv "room") $ zip reviewData [1..]
          -- Just tree <- runCcEnv ccEnv (annotateWord env) "The rooms were sleek and cool , great views ."
          -- latexify tree
          putStr "\n\n"
          putStr "Results:\n"
          putStr $ unlines (map show result)
          putStr "\n\n"
          putStr "Finshed.\n"
          return ()

