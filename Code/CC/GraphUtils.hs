module GraphUtils where
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

unfoldG :: (Ord a) => (a -> [a]) -> [a] -> (Map a Node, Gr a Int)
unfoldG r seeds = runST ( unfoldST r seeds )

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
                  Nothing -> do -- Get next id for this item
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

fix :: Int -> Int
fix x | x == 0    = 10
      | x > 10    = 10 
      | x <= 10    = x
      | otherwise = error "should not happend"

dist :: (Real b) => Gr a b -> [Node] -> [Node] -> [Node] -> Int
dist gr pns nns qns = let pos_dists = take 3 $ sort $ map fix $ [length $ sp pn qn gr | pn <- pns, qn <- qns]
                          neg_dists = take 3 $ sort $ map fix $ [length $ sp nn qn gr | nn <- nns, qn <- qns]
                      in (sum neg_dists) - (sum pos_dists)

