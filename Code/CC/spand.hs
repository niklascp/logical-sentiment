import Control.Monad.ST
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map

canUnify :: Category -> Category -> Bool
canUnify c1 c2 = runST ( canUnifyST c1 c2 )

canUnifyST :: Category -> Category -> ST s Bool
canUnifyST c1 c2 =
    do boundRef  <- newSTRef (Map.empty) -- do probe <- memTabFind mtab n
       let unify c1' c2'  = do bindings <- readSTRef boundRef
                               case c2' of
                                 c2a :/ c2b -> case c1' of 
                                                 c1a :/ c1b -> do t1 <- unify c1a c2a
                                                                  t2 <- unify c1b c2b  
                                                                  return $ t1 && t2 
                                                 _ -> return False
                                 c2a :\ c2b -> case c1' of 
                                                 c1a :\ c1b -> do t1 <- unify c1a c2a
                                                                  t2 <- unify c1b c2b
                                                                  return $ t1 && t2 
                                                 _ -> return False
                                 CVar x     -> case Map.lookup x bindings of
                                                 Just c  -> do t <- unify c c1'
                                                               return t
                                                 Nothing -> do modifySTRef boundRef (Map.insert x c1')
                                                               return True
                                 _          -> return $ c1' =? c2'      
       r <- unify c1 c2
       return r 