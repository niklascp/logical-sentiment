{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NLP.WordNet
-- Copyright   :  (c) Hal Daume III 2003-2004
-- License     :  BSD-style
--
-- Maintainer  :  hdaume@isi.edu
-- Stability   :  experimental
-- Portability :  non-portable (H98 + implicit parameters)
--
-- This is the top level module to the Haskell WordNet interface.
--
-- This module is maintained at:
--    <http://www.isi.edu/~hdaume/HWordNet/>.
--
-- This is the only module in the WordNet package you need to import.
-- The others provide utility functions and primitives that this
-- module is based on.
--
-- More information about WordNet is available at:
--    <http://http://www.cogsci.princeton.edu/~wn/>.
-----------------------------------------------------------------------------
module WordNet
    (
     -- * The basic type system
     module WordNet.Types,

     -- * Top level execution functions
     runWordNet,
     runWordNetQuiet,
     runWordNetWithOptions,

     -- * Functions to manually initialize the WordNet system; these are not
     --   needed if you use one of the "runWordNet" functions above.
     initializeWordNet,
     initializeWordNetWithOptions,
     closeWordNet,
     runs,

     -- * The basic database access functions.
     getOverview,
     searchByOverview,
     search,
     lookupKey,

     -- * The agglomeration functions
     relatedBy,
     -- closure,
     -- closureOn,

     -- * Computing lowest-common ancestor functions; the implementation
     --   of these can be tuned by providing a different "Bag" implementation.
     --   use "emptyQueue" for breadth-first-search (recommended) or "emptyStack"
     --   for depth-first-search, or write your own.
     -- meet,
     -- meetPaths,
     -- meetSearchPaths,
     -- Bag(..),
     -- emptyQueue,
     -- emptyStack,
    )
    where

import Prelude hiding (catch)
import Data.Array
import GHC.Arr (unsafeIndex)
import GHC.IO.Handle
import Data.Tree
import Data.IORef
import Data.Dynamic
import qualified Data.Set as Set
import Numeric (readHex, readDec)
import System.IO.Unsafe

import WordNet.Common
import WordNet.Consts
import WordNet.Util
import WordNet.Types

import qualified WordNet.PrimTypes as T
import qualified WordNet.Prims     as P

-- | Takes a WordNet command, initializes the environment
-- and returns the results in the 'IO' monad.  WordNet
-- warnings are printed to stderr.
runWordNet ::WN a -> IO a
runWordNet = runWordNetWithOptions Nothing Nothing

-- | Takes a WordNet command, initializes the environment
-- and returns the results in the 'IO' monad.  WordNet
-- warnings are ignored.
runWordNetQuiet :: WN a -> IO a
runWordNetQuiet = runWordNetWithOptions Nothing (Just (\_ _ -> return ()))

-- | Takes a FilePath to the directory holding WordNet and
-- a function to do with warnings and a WordNet command, initializes 
-- the environment and returns the results in the 'IO' monad.
runWordNetWithOptions :: 
    Maybe FilePath ->                          -- word net data directory
    Maybe (String -> SomeException -> IO ()) ->    -- warning function (by default, warnings go to stderr)
    WN a ->                                    -- what to run
      IO a
runWordNetWithOptions dd warn wn = do
  wne <- P.initializeWordNetWithOptions dd warn
  -- let a = (let ?wne = wne in wn)
  -- P.closeWordNet wne
  return (let ?wne = wne in wn)

-- | Gives you a 'WordNetEnv' which can be passed to 'runs' or used
-- as the implicit parameter to the other WordNet functions.
initializeWordNet :: IO WordNetEnv
initializeWordNet = P.initializeWordNet

-- | Takes a FilePath to the directory holding WordNet and
-- a function to do with warnings, initializes 
-- the environment and returns a 'WordNetEnv' as in 'initializeWordNet'.
initializeWordNetWithOptions :: Maybe FilePath -> Maybe (String -> SomeException -> IO ()) -> IO WordNetEnv
initializeWordNetWithOptions = P.initializeWordNetWithOptions

-- | Closes all the handles associated with the 'WordNetEnv'.  Since
-- the functions provided in the "NLP.WordNet.WordNet" module
-- are /lazy/, you shouldn't do this until you're really done.
-- Or perhaps not at all (GC will eventually kick in).
closeWordNet :: WordNetEnv -> IO ()
closeWordNet = P.closeWordNet

-- | This simply takes a 'WordNetEnv' and provides it as the
-- implicit parameter to the WordNet command.
runs :: WordNetEnv -> WN a -> a
runs wne x = let ?wne = wne in x


-- | This takes a word and returns an 'Overview' of all its senses
-- for all parts of speech.
getOverview :: (?wne :: WordNetEnv) => WN (Word -> Overview)
getOverview word = unsafePerformIO $ do
  idxN <- unsafeInterleaveIO $ getOverview' Noun
  idxV <- unsafeInterleaveIO $ getOverview' Verb
  idxA <- unsafeInterleaveIO $ getOverview' Adj
  idxR <- unsafeInterleaveIO $ getOverview' Adv
  return (T.Overview idxN idxV idxA idxR)
  where
    getOverview' pos = do
      strM <- P.getIndexString ?wne word pos
      case strM of
        Nothing -> return Nothing
        Just  s -> unsafeInterleaveIO $ P.indexLookup ?wne word pos

-- | This takes an 'Overview' (see 'getOverview'), a 'POS' and a 'SenseType' and returns
-- a list of search results.  If 'SenseType' is 'AllSenses', there will be one
-- 'SearchResult' in the results for each valid sense.  If 'SenseType' is
-- a single sense number, there will be at most one element in the result list.
searchByOverview :: (?wne :: WordNetEnv) => WN (Overview -> POS -> SenseType -> [SearchResult])
searchByOverview overview pos sense = unsafePerformIO $ 
  case (case pos of { Noun -> T.nounIndex ; Verb -> T.verbIndex ; Adj -> T.adjIndex ; Adv -> T.advIndex })
          overview of
    Nothing  -> do
      return []
    Just idx -> do
      let numSenses = T.indexSenseCount idx
      synsets <- mapM (\sense -> do let cpos = T.fromEPOS $ T.indexPOS idx
                                    synset <- P.readSynset ?wne cpos (T.indexOffsets idx !! (sense-1)) ""
                                    return (((,) sense) synset)) (sensesOf numSenses sense)
      return $ map (\(snum, synset) -> T.SearchResult (Nothing) (Just overview) (Just idx) (Just (SenseNumber snum)) synset) synsets


-- | This takes a 'Word', a 'POS' and a 'SenseType' and returns
-- the equivalent of first running 'getOverview' and then 'searchByOverview'.
search :: (?wne :: WordNetEnv) => WN (Word -> POS -> SenseType -> [SearchResult])
search word pos sense = searchByOverview (getOverview word) pos sense

-- | This takes a 'Key' (see 'srToKey' and 'srFormKeys') and looks it
-- up in the databse.
lookupKey :: (?wne :: WordNetEnv) => WN (Key -> SearchResult)
lookupKey (T.Key (o,p)) = unsafePerformIO $ do
  ss <- unsafeInterleaveIO $ P.readSynset ?wne p o ""
  return $ T.SearchResult Nothing Nothing Nothing Nothing ss

-- morphWord :: (?wne :: WordNetEnv) => WN (Word -> POS -> Word)
-- morphWord w p = do -- Check exception-list
--                    excResult <- P.excLookup ?wne w p
--                    case excResult of
--                      Just w  -> return w
--                      Nothing -> 

-- | This takes a 'Form' and a 'SearchResult' and returns all
-- 'SearchResult' related to the given one by the given 'Form'.
--
-- For example:
--
-- > relatedBy Antonym (head (search "happy" Adj 1))
-- > [<unhappy>]
-- >
-- > relatedBy Hypernym (head (search "dog" Noun 1))
-- > [<canine canid>]
relatedBy :: (?wne :: WordNetEnv) => WN (Form -> SearchResult -> [SearchResult])
relatedBy form sr = map lookupKey $ srFormKeys sr form

