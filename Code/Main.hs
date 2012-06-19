module Main where
import Control.Monad
import Data.Char
import Data.Map ((!))
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Text.Regex.Base;
import Text.Regex.TDFA

import Lambda;
import CCG;
import LexiconParser
import Pretty;

instance (Pretty a) => Pretty (State a b) where
  render (State (ps,[])) = foldr (++) "" (map render $ reverse ps)

---- TODO: Use stochatic propabilities to tag queries. 
tag :: Lexicon -> String -> [Tag]
tag lex s = [Tag { word = w, score = 1.0 } | w <- (case Map.lookup s lex of { Just a -> a; Nothing -> error $ "Unknown word " ++ s })]

-- |Converts the string to lower case, strips non-letters, and splits it into words.
tokenize :: String -> [String]
tokenize a = getAllTextMatches ((map toLower a) =~ "[a-z']+" :: AllTextMatches [] String)

main :: IO ()--"The hotel buffet had fabulous food." --
main = do putStr $ "Parsing lexicon ...\n"
          lexiconData <- readFile "Preprocess/lex.data" 
          let lexicon = parseLexicon lexiconData
          putStr $ "Loaded lexicon (" ++ (show $ Map.size lexicon) ++ " tokens)\n"
          let tagging = map (tag lexicon) $ tokenize "The price is moderate for the service and the location."  --"The hotel had outstanding food."
          putStr $ unlines (map show tagging)
          let a = parse2 (State ([], tagging)) 0  
          --putStr $ "Found " ++ (show $ length a) ++ " solutions.\n"
          --print $ head a
          --latexify $ (sort a) !! 0
          latexify $ head a
          return ()


--- BEGIN ON PARSER
-- TODO Should we simply use the stack for the parse tree?
data State a b = State ([a],[b]) -- Parse tree stack, and unprocessed words.
                 deriving (Eq,Show)

instance (Eq a, Eq b) => Ord (State a b) where
  compare (State (ps,_)) (State (ps',_)) = compare (length ps) (length ps')

--instance (Eq a, Eq b) => Eq (State a b) where
--  State (a, b) == State (a', b') = a == a' && b == b'

type Rule a b = State a b -> [State a b]

acceptS :: (State PTree [Tag]) -> Bool -- Accerps a single sentence
acceptS (State (stack,[])) = map (isS . category) stack == [True]
acceptS _                  = False

acceptSs :: (State PTree [Tag]) -> Bool-- Accerps a list of sentences
acceptSs (State (stack,[])) = all (isS . category) stack
acceptSs _                  = False

parse2 :: (State PTree [Tag]) -> Int -> [State PTree [Tag]]
parse2 state d | acceptS state   = do -- Accepting state
                                      return state

               | d < 40          = do -- Calculate new states by applying rules.
                                      state' <- conjugation state ++
                                                forwardTypeRaise state ++
                                                forwardApp state ++
                                                backwardApp state ++
                                             --   forwardComp state ++
                                             --   nounRaise state ++
                                                shift state
                                      -- Ensure that we actually got a new state.
                                      guard (state' /= state) 
                                      --trace (show d ++ ": " ++ show state') [1]
                                      -- Recursively apply rules to this new state
                                      state'' <- (parse2 (state') (d + 1))
                                      return state''

               | otherwise      = trace "-: Max depth reached" []



m :: [PTree] -> ([Category] -> [LTerm] -> [State a b]) -> [State a b]
m ptrees expr = expr (map category ptrees) (map term ptrees)


shift :: Rule PTree [Tag]
shift (State (stack, tags:rest)) = [State (PWord (word tag):stack, rest) | tag <- tags]
shift (State (stack, []))        = mzero -- No more to shift

conjugation :: Rule PTree [Tag]
conjugation (State (a:b:c:stack, rest)) = m [a,b,c] conjugation'
                                        where conjugation' :: [Category] -> [LTerm] -> [State PTree [Tag]]
                                              conjugation' [x,y@(CONJ _),x'] [t1,t,t2] | x =? x'   = [State (PConj x (reduce $ LApp (LApp t t1) t2) c b a : stack, rest)]
                                                                                       | otherwise = []
                                              conjugation' _ _                                     = []
conjugation _ = []

nounRaise :: Rule PTree [Tag]
nounRaise (State (a:stack, rest)) = m [a] nounRaise'
                                  where nounRaise' :: [Category] -> [LTerm] -> [State PTree [Tag]]
                                        nounRaise' [x] [t] | x =? N [] = [State (PNounRaise (NP $ agreement x) t a : stack, rest)]
                                                           | otherwise = []
                                        nounRaise' _ _                 = []
nounRaise _ = []

forwardTypeRaise :: Rule PTree [Tag]
forwardTypeRaise (State (a:stack, rest)) = m [a] forwardTypeRaise'
                                         where forwardTypeRaise' :: [Category] -> [LTerm] -> [State PTree [Tag]]
                                               forwardTypeRaise' [x] [t] | x =? NP [] = [State (PFwdTR (S [] :/ (S [] :\ x)) (reduce $ LAbs "f" (LApp (LVar "f") t)) a : stack, rest)]
                                                                         | otherwise  = []
                                               forwardTypeRaise' _ _                  = []
forwardTypeRaise _ = []


forwardApp :: Rule PTree [Tag]
forwardApp (State (a:b:stack, rest)) = m [a,b] forwardApp'
                                     where forwardApp' :: [Category] -> [LTerm] -> [State PTree [Tag]]
                                           forwardApp' [y,x:/y'] [arg,f] | y =? y'   = [State (PFwdApp x (reduce $ LApp f arg) b a : stack, rest)]
                                                                         | otherwise = []
                                           forwardApp' _ _                           = []
forwardApp _ = []

backwardApp :: Rule PTree [Tag]
backwardApp (State (a:b:stack, rest)) = m [a,b] backwardApp'
                                      where backwardApp' :: [Category] -> [LTerm] -> [State PTree [Tag]]
                                            backwardApp' [x:\y,y'] [f,arg] | y =? y'   = [State (PBwdApp x (reduce $ LApp f arg) b a : stack, rest)]
                                                                           | otherwise = []
                                            backwardApp' _ _                           = []
backwardApp _ = []


forwardComp :: Rule PTree [Tag]
forwardComp (State (a:b:stack, rest)) = m [a,b] forwardComp'
                                      where forwardComp' :: [Category] -> [LTerm] -> [State PTree [Tag]]
                                            forwardComp' [y:/z,x:/y'] [g,f] | y =? y'   = [State (PFwdComp (x:/z) (reduce $ LAbs "z" (LApp f (LApp g (LVar "z")))) b a : stack, rest)]
                                                                            | otherwise = []
                                            forwardComp' _ _                            = []
forwardComp _ = []