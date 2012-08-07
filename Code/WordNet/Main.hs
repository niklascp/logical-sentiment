{-# LANGUAGE ImplicitParams #-}

module Main where
import Control.Monad.ST
import Data.STRef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete

import WordNet

unfoldGMany :: (Ord a) => (a -> [a]) -> [a] -> (Map a Node, Gr a Int)
unfoldGMany f roots = runST ( unfoldGManyST f roots )

unfoldGManyST :: (Ord a) => (a -> [a]) -> [a] -> ST s (Map a Node, Gr a Int)
unfoldGManyST adj seeds =
    let 
        firstId = 0::Node
    in  
      do mtab      <- newSTRef (Map.empty)
         allNodes  <- newSTRef []           -- [(LNode, [LEdge])]
         vertexRef <- newSTRef firstId
         let visit n = do probe <- memTabFind mtab n
                          case probe of
                            Just v  -> return v
                            Nothing -> do v <- allocVertex vertexRef   -- Get next vetex
                                          memTabBind n v mtab          -- Update map
                                          let ns' = adj n              -- Get adjancent nodes 
                                          ws <- mapM visit ns'         -- Visit adjancent nodes 
                                          let res = ((v, n), [(v,w,1) | w <- ws])
                                          modifySTRef allNodes (res:)
                                          return v
         mapM_ visit seeds
         list <- readSTRef allNodes         
         nodeMap <- readSTRef mtab -- (return . map fromJust) =<< mapM (memTabFind mtab) seeds
         let nodes = [n | (n, _) <- list]
         let edges = concat [es | (_, es) <- list]
         return (nodeMap, mkGraph nodes edges)
    where 
        allocVertex ref = do vertex <- readSTRef ref
                             writeSTRef ref (vertex + 1)
                             return vertex
        memTabFind mt key = return . Map.lookup key =<< readSTRef mt
        memTabBind key val mt = modifySTRef mt (Map.insert key val)



-- defaultVis = setDirectedness graphToDot nonClusteredParams { 
--                                           globalAttributes = [GraphAttrs [Overlap ScaleOverlaps]], 
--                                           fmtNode = \(n,l) -> [toLabel (l)] 
--                                         }


instance Eq SearchResult where
  a == b = (srToKey a) == (srToKey b)

instance Ord SearchResult where
  compare a b = compare (srToKey a) (srToKey b) 

dist :: (Real b) => Gr a b -> [Node] -> [Node] -> [Node] -> Int
dist gr pns nns qns = let pos_dists = take 3 $ sort $ filter (>0) $ [length $ sp pn qn gr | pn <- pns, qn <- qns]
                          neg_dists = take 3 $ sort $ filter (>0) $ [length $ sp nn qn gr | nn <- nns, qn <- qns]
                      in (sum neg_dists) - (sum pos_dists)

main :: IO ()
main = do wne <- initializeWordNetWithOptions Nothing Nothing

          -- Define positive list for adjectives.
          let pos_list = [("good", 1), ("beautiful", 1), ("pleasant", 1),   ("clean", 1), ("quiet", 1), ("friendly", 1),   ("cheap", 1)]
          let neg_list = [("bad", 1),  ("hideous", 1),   ("unpleasant", 1), ("dirty", 1), ("noisy", 1), ("unfriendly", 1), ("expensive", 1)]

          -- Load the synsets that corresponds to the words in the above lists.
          let pos_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) pos_list)
          let neg_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) neg_list)

          putStr "\n\n"

          putStr $ "Positive synsets:\n"
          putStr $ unlines $ map show pos_ss
          putStr "\n"
          putStr $ "Negative synsets:\n"
          putStr $ unlines $ map show neg_ss

          putStr "\n\n"

          putStr "Constructing semantic networks...\n"
          let (nmap, g) = (let ?wne = wne in unfoldGMany (\x -> (relatedBy Similar x ++ relatedBy SeeAlso x)) (pos_ss ++ neg_ss))
          putStr $ "- Adj: " ++ (show $ Map.size nmap) ++ " concepts."

          let pos_roots = map (fromJust . flip Map.lookup nmap) pos_ss
          let neg_roots = map (fromJust . flip Map.lookup nmap) neg_ss   
          let d = dist g pos_roots neg_roots       
          
          putStr "\n\n"

          let query_ss1 = (let ?wne = wne in search "helpful" Adj AllSenses)
          let query_ss2 = (let ?wne = wne in search "gracious" Adj AllSenses)
          let query_ss3 = (let ?wne = wne in search "terrible" Adj AllSenses)
          print $ d $ catMaybes $ map (flip Map.lookup nmap) query_ss1
          print $ d $ catMaybes $ map (flip Map.lookup nmap) query_ss2
          print $ d $ catMaybes $ map (flip Map.lookup nmap) query_ss3

          -- runGraphviz (defaultVis g) Eps "test.eps"
          
          -- let lenghts = filter (>0) $ map ((\x -> x-1) . length) $ map (flip (sp v) g) vs
          -- print lenghts
          -- putStr $ show gr1
          putStr "\n\n"
          return ()