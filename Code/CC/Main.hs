{-# LANGUAGE ImplicitParams #-}

module Main where
-- import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph.Inductive
import Data.Maybe

import Parser
import CCG

import Pretty

import WordNet hiding (Word)
import qualified WordNet.Prims as P
import GraphUtils

import Debug.Trace

data AnnotationEnv = AnnotationEnv { 
     wnEnv  :: WordNetEnv,
     adjMap :: Map SearchResult Node,
     adjFun :: [Node] -> Int
}

isDet pos    = pos == "DT"
isConj pos    = pos == "CC"
isAdj pos    = (take 2 pos) == "JJ"
isVerb pos   = (take 2 pos) == "VB"
isAdverb pos = (take 2 pos) == "RB"
isNoun pos   = (take 2 pos) == "NN" || pos == "PRP"

-- TODO: It is not succicient to check args
annotateDet env w@(Word { category = c, token = t, lemma = l })
  | args c == 0 = w { lTerm = LFun l 0 [] } -- Simple determiner 
  | args c == 1 = w { lTerm = lid } -- Simple determiner 
  | args c == 2 = w { lTerm = lid } -- Simple determiner 
  | args c == 3 = w { lTerm = LAbs "x" $ LAbs "f" $ LApp (LVar "f") (LVar "x") } -- Complex determiner 

annotateNoun env w@(Word { category = c, token = t, lemma = l })
  | args c == 0 = w { lTerm = (LFun l 0 []) } -- Simple noun
  | args c == 1 = w { lTerm = (LAbs "x" $ LSeq [LFun l 0 [], LVar "x"]) } -- Part of multi lexical noun  
  | otherwise         = trace ("Could not annotate noun: " ++ t) (annotateAny env w)


annotateVerb env w@(Word { category = c, token = t, lemma = l })
  | args (arg c) == 1 = w { lTerm = (LAbs "x" $ LVar "x") }
  -- | args c == 3       = w { lTerm = (LAbs "x" $ LAbs "y" $ LAbs "f" $ LFun l 0 [LApp (LVar "f") (LVar "x"), LApp (LVar "f") (LVar "y")]) } -- Ditransitive verb
  | otherwise         = annotateAny env w
  -- | args c == 1       = w { lTerm = (LAbs "x" $ LFun l 0 [LVar "x"]) }
  -- | args c == 2       = w { lTerm = (LAbs "x" $ LAbs "y" $ LFun t 0 [LVar "x", LVar "y"]) }
  -- | args c == 3       = w { lTerm = (LAbs "x" $ LAbs "y" $ LAbs "z" $ LFun t 0 [LVar "x", LVar "y", LVar "z"]) }
  -- | otherwise         = error "No verb should yield more than binary expressions." 


annotateAdj env w@(Word { token = t, category = c, lemma = l })
  | args c   == 1 = let query = (let ?wne = (wnEnv env) in search l Adj AllSenses)
                        value = (adjFun env) $ catMaybes $ map (flip Map.lookup $ adjMap env) query
                    in  w { lTerm = (LAbs "x" $ LAdd (LVar "x") value) }
  | otherwise     = w { lTerm = (LAbs "x" $ LVar "x") } -- error "No adjective should yield more than unary expressions."


annotateAdverb env w@(Word { token = t, category = c, lemma = l })
  | args c == 2 = let -- Try to lookup adjective pertainyms
                      query = (let ?wne = (wnEnv env) in filter ((==) Adj . srPOS) $ concat $ map (relatedBy Pertainym) (search l Adv AllSenses))                      
                      value = (adjFun env) $ catMaybes $ map (flip Map.lookup $ adjMap env) query
                  in case value of
                      0 -> w { lTerm = (LAbs "x" $ LVar "x") } -- TODO: fallback to simpy adverb reduction to adj?
                      x -> w { lTerm = (LAbs "x" $ LAdd (LVar "x") $ x) }
                      
  | otherwise   = w { lTerm = (LAbs "x" $ LVar "x") } -- error "No adverb should yield more than binary expressions."

annotateAny env w@(Word { token = t, category = c, lemma = l }) =
  w { lTerm = constructTerm [] c }
  where 
    newVar used = head $ dropWhile (`elem` used) $ xyzVars
    constructTerm :: [String] -> Category -> LTerm
    constructTerm used c = case c of
      a :\ _ -> let v = newVar used in LAbs v (constructTerm (v:used) a)
      a :/ _ -> let v = newVar used in LAbs v (constructTerm (v:used) a)
      _      -> LFun l 0 $ map LVar $ reverse used

-- annotateConj env (Word token pos cat expr) 

annotateWord :: AnnotationEnv -> Word -> Word
annotateWord env w@(Word { pos = pos })
  | isDet pos  = annotateDet env w
  | isAdj pos  = annotateAdj env w
  | isVerb pos = annotateVerb env w
  | isAdverb pos = annotateAdverb env w
  | isNoun pos = annotateNoun env w
  | isConj pos = annotateAny env w
  | otherwise  = annotateAny env w

mai2 :: IO ()
mai2 = do let term = LAbs "x" $ LAbs "y" $ LFun "tes" 0 [LVar "x", LVar "y"]
          print $ foldr (flip LApp) term (reverse [LVar "k", LVar "j"])
          return ()

mai3 :: IO ()
mai3 = do wne <- initializeWordNetWithOptions Nothing Nothing
          
          baseForm <- P.excLookup wne "harder" Adv

          print $ fromJust baseForm

          return ()

main :: IO ()
main = do wne <- initializeWordNetWithOptions Nothing Nothing
          
          -- Define positive list for adjectives.
          let adj_pos_list = [("good", 1), ("beautiful", 1), ("pleasant", 1),   ("clean", 1), ("quiet", 1), ("friendly", 1),   ("cheap", 1), ("fast", 1), ("large", 1)]
          let adj_neg_list = [("bad", 1),  ("hideous", 1),   ("unpleasant", 1), ("dirty", 1), ("noisy", 1), ("unfriendly", 1), ("expensive", 1), ("slow", 1), ("small", 1)]

          -- Load the synsets that corresponds to the words in the above lists.
          let adj_pos_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) adj_pos_list)
          let adj_neg_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) adj_neg_list)

          putStr "\n\n"

          putStr $ "Positive synsets:\n"
          putStr $ unlines $ map ((++) "- Adj: " . show) adj_pos_ss
          putStr "\n"
          putStr $ "Negative synsets:\n"
          putStr $ unlines $ map ((++) "- Adj: " . show) adj_neg_ss

          putStr "\n\n"

          putStr "Constructing semantic networks...\n"
          let (adj_map, adj_g) = (let ?wne = wne in unfoldGMany (\x -> (relatedBy Similar x ++ relatedBy SeeAlso x)) (adj_pos_ss ++ adj_neg_ss))
          putStr $ "- Adj: " ++ (show $ Map.size adj_map) ++ " concepts.\n"

          let adj_pos_roots = map (fromJust . flip Map.lookup adj_map) adj_pos_ss
          let adj_neg_roots = map (fromJust . flip Map.lookup adj_map) adj_neg_ss
          let adj_fun = dist adj_g adj_pos_roots adj_neg_roots    

          let env = AnnotationEnv { 
            wnEnv = wne, 
            adjMap = adj_map, 
            adjFun = adj_fun
          }

          -- Syntactic Examples
          let sentence = "Harry gave Louise a flower"
          let sentence = "The service that the hotel provides" -- relative clause
          let sentence = "the hotel had daily a large buffet with delicious food" -- <Bx rule
          let sentence = "I Ed think that saw Ann"

          -- putStr $ "Loading reviews...\n"
          -- reviewData <- readFile "../Data/rooms_swissotel_chicago.txt.data" 
          -- let reviews = map (Text.unpack . Text.strip . Text.pack) (lines reviewData)
          -- putStr $ "Loaded reviews (" ++ (show $ length reviews) ++ " sentences)\n"

          -- SIMPLE:
          -- let sentence = "the accomodations and support services are phenomenal ."
          -- let sentence = "excellent service and awesome view ."
          -- let sentence = "Great stay , excellent housekeeping service ." -- NP
          -- let sentence = "The rooms were spotlessly clean and serviced each day ."

          -- GOOD EXAMPLES:          
          -- let sentence = "Service was excellent and the suites were large and comfortable ."
          -- let sentence = "they had a great wine list , appetizers and friendly service ."
          -- let sentence = "the rooms are very clean , comfortable and spacious ."
          -- let sentence = "The concierge and service line were both just okay ." -- long term dependency
          -- let sentence = "The ideal location coupled with great service and beautiful rooms makes for a wonder stay ."

          -- COUNTER EXAMPLES: Eg. negative sentences, are rated positive vice-verse
          

          -- EXAMPLES OF ABUIGITY:
          -- let sentence = "mediocre room and service for a very extravagant price ." -- service is however not infected by mediocre

          -- ONES WE ALMOST CRACK
          -- let sentence = "some of the concierge staff were a bit chilly at times , but the housekeeping service staff were extremely friendly and cheerful ."

          -- ONES WE MIGHT NEVER CRACK:
          -- let sentence = "too bad on several days , they were not able to service the room until late afternoon ."
          -- let sentence = "no robes in the rooms , ice machines are not on every floor , no turn down service at night , and when housekeeping did come , they did not replace the shower gel we had used which was totally empty ."
          -- let sentence = "even with the amount we saved on the room , we could not justify spending the amount they listed for room service ."
          -- let sentence = "I will not be staying here again until I hear that they have upgraded this service ."
          
          

          -- DATA SET SUCKS
          -- let sentence = "The service truly was above and beyond ." -- wrong word order.

          

          -- PROBLEMS:
          -- IN
          -- let sentence = "The hotel was clean and the room was decent in size"  
          -- let sentence = "building is shaped like a triangle"          
          -- let sentence = "the internet service is reliable and fast and the desk in the room is large" -- large modifies "in" instead of "desk"
          -- let sentence = "Room service was expensive in the hotel ."
          -- let sentence = "Service was excellent and housing came as soon as asked ."
          -- let sentence = "They have demonstrated a wonderful ability to deliver consistent service and go above and beyond ."
          -- let sentence = "You can get better customer service elsewhere for much less ."

          -- WHEN
          -- let sentence = "Concierge service was fine when we needed our boarding pass printed ."
          
          -- TO
          -- let sentence = "We found the service to be fine for our needs ."
          
          -- NEGATION
          -- We ordered room service for breakfast and were not disappointed .
          -- We were disappointed that the internet service was not free .

          -- VERBS
          -- let sentence = "rooms , service , location were top notch ." -- "top notch" describes rooms, service ...

          -- PARSING
          -- let sentence = "Fantastic view excellent service , top notch ." ltc rule !?!?
          -- VALUES
          -- let sentence = "free internet in the lobby but none of any sort in the rooms" - Free is 0 ?
          -- let sentence = "very impressed with rooms and view" - impressed is 0 ?

          -- Determiners
          -- let sentence = "the rooms were cleaned spic and span every day" -- every, also funny spic

          -- let sentence = "beautiful hotel , service slightly lacking ."


          -- IN PROGRESS:
          --
          -- 1. CONJUGATION, COMMAS AND PUNCTATION
          -- let sentence = "we appreciated the elegant service and manner of the staff" -- simple (N\N)/N
          -- let sentence = "the internet service is reliable and fast and the desk in the room is large" -- simple: (Sdcl\Sdcl)/Sdcl
          -- let sentence = "The rooms are very clean , comfortable and spacious and up-to-date ." -- Notice that comfortable and spacious and up-to-date is "type changed"
          -- let sentence = "the rooms are very clean , comfortable and spacious ."
          -- let sentence = "they had a great wine list , appetizers , and friendly service ."
          -- let sentence = "they had a great wine list , appetizers and friendly service ."
          --
          -- 2. VALANCE (itencifiers)
          -- let sentence = "the rooms were small and much more expensive" 
          -- let sentence = "building is shaped like a triangle and only has about 17 rooms on a floor so very quiet and no hall noise"
          --
          -- 3. CONNECTED LEXICAL UNITS 
          -- let sentence = "The buffet in the hotel lobby is very expensive ." -- "hotel lobby"
          -- let sentence = "They had a great wine list , appetizers and friendly service ." -- "wine list"
          -- let sentence = "maid service was prompt and outstanding" -- "maid service"
          -- let sentence = "The main issue I had with Kindle was the navigation button design and placement" -- "navigation button design"

          
          
          -- let sentence = "One thing that really impressed us was the excellent housekeeping service ."
          t <- runCC (annotateWord env) sentence
          latexify t

          

          putStr "Closed:\n"
          return ()

-- TODO: Move?
instance Eq SearchResult where
  a == b = (srToKey a) == (srToKey b)

instance Ord SearchResult where
  compare a b = compare (srToKey a) (srToKey b) 