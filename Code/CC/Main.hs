{-# LANGUAGE ImplicitParams #-}

module Main where
-- import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Graph.Inductive
import Data.Maybe

import Parser
import CCG
import Annotate

import Pretty

import WordNet hiding (Word)
import qualified WordNet.Prims as P
import GraphUtils

import Debug.Trace

main :: IO ()
main = do wne <- initializeWordNetWithOptions Nothing Nothing
          
          -- Define positive concepts
          let adj_pos_list = [("good", 1), ("beautiful", 1), ("pleasant", 1),   ("clean", 1), ("quiet", 1), ("friendly", 1),   ("cheap", 1), ("fast", 1), ("large", 1)]
          let adj_neg_list = [("bad", 1),  ("hideous", 1),   ("unpleasant", 1), ("dirty", 1), ("noisy", 1), ("unfriendly", 1), ("expensive", 1), ("slow", 1), ("small", 1)]

          -- Define intensifiers concepts
          let intensifiers = [("extreme", 1), ("much", 1), ("more", 1)]
          let qualifier = [("moderate", 1), ("little", 1), ("less", 1)]

          -- Load the synsets that corresponds to the words in the above lists.
          let adj_pos_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) adj_pos_list)
          let adj_neg_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) adj_neg_list)

          -- Load the synsets that corresponds to the words in the above lists.
          let intensifiers_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) intensifiers)
          let qualifier_ss = (let ?wne = wne in map (\(w, i) -> head $ search w Adj i) qualifier)

          putStr "\n\n"

          putStr $ "Positive synsets:\n"
          putStr $ unlines $ map ((++) "- Adj: " . show) adj_pos_ss
          putStr "\n"
          putStr $ "Negative synsets:\n"
          putStr $ unlines $ map ((++) "- Adj: " . show) adj_neg_ss

          putStr "\n\n"

          putStr "Constructing semantic networks...\n"
          let (adj_map, adj_g) = (let ?wne = wne in unfoldG (\x -> (relatedBy Similar x ++ relatedBy SeeAlso x)) (adj_pos_ss ++ adj_neg_ss))
          let (scaleMap, scaleGraph) = (let ?wne = wne in unfoldG (relatedBy Similar) (intensifiers_ss ++ qualifier_ss))
          putStr $ "- Change : " ++ (show $ Map.size adj_map) ++ " concepts.\n"
          putStr $ "- Scale  : " ++ (show $ Map.size scaleMap) ++ " concepts.\n"

          let adj_pos_roots = map (fromJust . flip Map.lookup adj_map) adj_pos_ss
          let adj_neg_roots = map (fromJust . flip Map.lookup adj_map) adj_neg_ss
          let adj_fun = fromIntegral . dist adj_g adj_pos_roots adj_neg_roots . catMaybes . map (flip Map.lookup adj_map)

          let scaleIntensifiersRoots = map (fromJust . flip Map.lookup scaleMap) intensifiers_ss
          let scaleQualifierRoots = map (fromJust . flip Map.lookup scaleMap) qualifier_ss
          let scaleFun = exp . (0.0231049*) . fromIntegral . dist scaleGraph scaleIntensifiersRoots scaleQualifierRoots . catMaybes . map (flip Map.lookup scaleMap)

          let env = AnnotationEnv { 
            wnEnv = wne, 
            adjFun = adj_fun,
            scaleFun = scaleFun
          }

          -- Syntactic Examples
          let sentence = "Harry gave Louise a flower"
          let sentence = "The service that the hotel provides" -- relative clause
          let sentence = "the hotel had daily a large buffet with delicious food" -- <Bx rule
          -- let sentence = "The hotel was fairly expensive ."
          -- let sentence = "The hotel was fairly clean ."


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

          -- - IN
          -- let sentence = "The hotel was clean and the room was decent in size"  
          -- let sentence = "The hotel is located beautifully in the mountains"  
          let sentence = "the internet service is reliable and fast and the desk in the room is large" -- large modifies "desk" correctly
          -- let sentence = "room service was expensive in the hotel ."
          -- let sentence = "Service was excellent and housing came as soon as asked ."
          -- let sentence = "We found the service to be fine for our needs" -- LONG DISTANVE - GOOD!

          -- COUNTER EXAMPLES: Eg. negative sentences, are rated positive vice-verse
          -- let sentence = "You can get better customer service elsewhere for much less ."

          -- EXAMPLES OF ABUIGITY:
          -- let sentence = "mediocre room and service for a very extravagant price ." -- service is however not infected by mediocre

          -- ONES WE ALMOST CRACK
          -- let sentence = "some of the concierge staff were a bit chilly at times , but the housekeeping service staff were extremely friendly and cheerful ."

          -- ONES WE MIGHT NEVER CRACK:
          -- let sentence = "too bad on several days , they were not able to service the room until late afternoon ."
          -- let sentence = "no robes in the rooms , ice machines are not on every floor , no turn down service at night , and when housekeeping did come , they did not replace the shower gel we had used which was totally empty ."
          -- let sentence = "even with the amount we saved on the room , we could not justify spending the amount they listed for room service ."
          -- let sentence = "I will not be staying here again until I hear that they have upgraded this service ."
          
          -- let sentence = "The breakfast that the resturant served daily was excellent" 
          -- let sentence = "The resturant served daily an excellent breakfast ." 
          -- let sentence = "The resturant that employed the waiter was excellent ." 
          
          -- DATA SET SUCKS
          -- let sentence = "The service truly was above and beyond ." -- wrong word order.

          

          -- FUNNY:
          -- - IN:          
          -- let sentence = "They have demonstrated a wonderful ability to deliver consistent service and go above and beyond ."
          
          -- WHEN w-XX
          -- let sentence = "One thing that really impressed us was the excellent housekeeping service ."
          -- let sentence = "Concierge service was fine when we needed our boarding pass printed ."
          
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
          -- let sentence = "beautiful hotel , service slightly lacking ."
          
          -- Determiners : GOOD GOOD GOOD !!!
          -- let sentence = "the rooms were cleaned every day" -- every, also funny spic
          -- let sentence = "the rooms were cleaned every day" 
          -- let sentence = "the rooms were cleaned daily" 
          -- let sentence = "the maid cleaned every room"


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
          --let sentence = "the rooms were small and much more expensive" 
          -- let sentence = "building is shaped like a triangle and only has about 17 rooms on a floor so very quiet and no hall noise"
          --
          -- 3. CONNECTED LEXICAL UNITS 
          -- let sentence = "The buffet in the hotel lobby is very expensive ." -- "hotel lobby"
          -- let sentence = "They had a great wine list , appetizers and friendly service ." -- "wine list"
          -- let sentence = "maid service was prompt and outstanding" -- "maid service"
          -- let sentence = "The main issue I had with Kindle was the navigation button design and placement" -- "navigation button design"

          
          let sentence = "the service was great"
          
          t <- runCC (annotateWord env) sentence
          latexify t

          

          putStr "Fnished.\n"
          return ()

