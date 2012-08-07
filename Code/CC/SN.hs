{-# LANGUAGE ImplicitParams #-}

module Main where
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete

import WordNet
import WordNet.PrimTypes
import GraphUtils

defaultVis = setDirectedness graphToDot nonClusteredParams { 
                                          globalAttributes = [GraphAttrs [Overlap ScaleOverlaps]], 
                                          fmtNode = \(n,l) -> [toLabel l] 
                                        }

instance Labellable SearchResult where
  toLabelValue (SearchResult { srSynset = ss }) = toLabelValue (unwords $ take 3 $ map (\(w,_, i) -> case i of { SenseNumber i' -> w ++ "." ++ show i'; AllSenses -> w }) $ ssWords ss)

demoNetwork :: IO ()
demoNetwork = do wne <- initializeWordNetWithOptions Nothing Nothing
          
                 -- Load a synset that corresponds to the given word and sense.
                 let adj_ss = (let ?wne = wne in head $ search "exceptional" Adj 1)
                 let (adj_map, adj_g) = (let ?wne = wne in unfoldGMany (\x -> (relatedBy Similar x ++ relatedBy SeeAlso x)) [adj_ss])

                 -- Id the relation is reflexive, drop the double edges
                 let adj_g' = undir adj_g

                 runGraphviz (defaultVis adj_g') DotOutput "exceptional.dot" 
                 return ()

instance Eq SearchResult where
  a == b = (srToKey a) == (srToKey b)

instance Ord SearchResult where
  compare a b = compare (srToKey a) (srToKey b) 
