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
         let visit d n = do probe <- memTabFind mtab n
                            case probe of
                              Just v  -> return v
                              Nothing -> do v <- allocVertex vertexRef   -- Get next vetex
                                            memTabBind n v mtab          -- Update map
                                            let ns' = if d < 3 then adj n else []              -- Get adjancent nodes 
                                            ws <- mapM (visit $ d + 1) ns' -- Visit adjancent nodes 
                                            let res = ((v, n), [(v,w,1) | w <- ws])
                                            modifySTRef allNodes (res:)
                                            return v
         mapM_ (visit 0) seeds
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

fix :: Int -> Int
fix x | x == 0    = 10
      | x > 10    = 10 
      | x <= 10    = x
      | otherwise = error "should not happend"

dist :: (Real b) => Gr a b -> [Node] -> [Node] -> [Node] -> Int
dist gr pns nns qns = let pos_dists = take 3 $ sort $ map fix $ [length $ sp pn qn gr | pn <- pns, qn <- qns]
                          neg_dists = take 3 $ sort $ map fix $ [length $ sp nn qn gr | nn <- nns, qn <- qns]
                      in (sum neg_dists) - (sum pos_dists)

