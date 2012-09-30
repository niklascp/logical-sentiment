{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CCG (
  module Unification,
  module Lambda,
  module CCG
) where
import Unification
import Lambda

type Token = String   -- Lexical entry
type Lemma = String   -- Lemma of lexical entry
type Pos   = String   -- Part of spearch

-- | Data structure for lexical units.
data Word = Word { 
              token :: Token,
              lemma :: Lemma,
              pos :: Pos,
              category :: Category, 
              expr :: SExpr 
            }
            deriving (Eq)

type Lexicon = [Word]

infix 9 :/  -- Forward slash operator
infix 9 :\  -- Backward slash operator

type Agreement = [Feature]

data Category = S           { agreement :: Agreement } -- Sentence
              | N           { agreement :: Agreement } -- Noun
              | NP          { agreement :: Agreement } -- Noun Phrase
              | PP          { agreement :: Agreement } -- Preposision Phrase
              | CONJ        { agreement :: Agreement } -- Conjugation (temperary category)
              | Punctuation { agreement :: Agreement } -- Punctation  (temperary category)
              | Comma       { agreement :: Agreement } -- Comma  (temperary category)
              | Category :/ Category                   -- Forward slash
              | Category :\ Category                   -- Backward slash
              deriving (Eq)

data Feature = FDcl  | FAdj | FEm  | FInv  -- Sentence
             | FTo   | FB   | FPt  | FPss  -- Verbs
             | FNg   | FNb  | FFor
             | FThr  | FFrg                -- Fragments
             | FQ    | FWq  | FQem         -- Questions
             | FVar String                 -- Variables
             | FUnknown String             -- Unknowns
             deriving (Eq,Show)

data PTree = PWord Word
           | PFwdApp   { nCategory :: Category, nExpr :: SExpr, n1 :: PTree, n2 :: PTree }
           | PBwdApp   { nCategory :: Category, nExpr :: SExpr, n1 :: PTree, n2 :: PTree }
           | PFwdComp  { nCategory :: Category, nExpr :: SExpr, n1 :: PTree, n2 :: PTree }
           | PBwdComp  { nCategory :: Category, nExpr :: SExpr, n1 :: PTree, n2 :: PTree }
           | PBwdXComp { nCategory :: Category, nExpr :: SExpr, n1 :: PTree, n2 :: PTree }
           | PFwdTR    { nCategory :: Category, nExpr :: SExpr, n1 :: PTree }
           | PLexRaise { nCategory :: Category, nExpr :: SExpr, n1 :: PTree }
           deriving (Eq,Show)

-- | Gets the category of a node in the deduction tree.
nodeCategory :: PTree -> Category
nodeCategory (PWord w) = category w
nodeCategory x         = nCategory x

-- | Gets the semantic expression of a node in the deduction tree.
nodeExpr :: PTree -> SExpr
nodeExpr (PWord w) = expr w
nodeExpr x         = nExpr x

instance Unifiable Category where
  S _           =? S _            = True
  N _           =? N _            = True
  NP _          =? NP _           = True
  PP _          =? PP _           = True
  CONJ _        =? CONJ _         = True
  Punctuation _ =? Punctuation _  = True
  Comma _       =? Comma _        = True
  (a :/ b)      =? (a' :/ b')     = a =? a' && b =? b'
  (a :\ b)      =? (a' :\ b')     = a =? a' && b =? b'
  _             =? _              = False

-- | Return if a category is compound.
isComplex :: Category -> Bool
isComplex (_ :/ _) = True
isComplex (_ :\ _) = True
isComplex _        = False

-- | Return the argument of the type inflicted by a compound category.
arg :: Category -> Maybe Category
arg (_:\x) = Just x
arg (_:/x) = Just x
arg x      = Nothing 

-- | Return the result of the type inflicted by a compound category.
res :: Category -> Maybe Category
res (x:\_) = Just x
res (x:/_) = Just x
res _      = Nothing 

-- | Pretty printing of Word
instance Show Word where
  showsPrec d (Word { token = t, lemma = lemma, pos = p, category = c, expr = e }) = 
    (showString t) . (showString "~") . (showString lemma) . (showString "/") . 
    (showString p) . (showString " ⊣ ") . (shows c) . (showString " : ") .
    (shows e)

-- | Pretty printing of Category
instance Show Category where
  showsPrec d (S a)            = showString "S"    . (showString " ") . (shows a)
  showsPrec d (N a)            = showString "N"    . (showString " ") . (shows a)
  showsPrec d (NP a)           = showString "NP"   . (showString " ") . (shows a)
  showsPrec d (PP a)           = showString "PP"   . (showString " ") . (shows a)
  showsPrec d (CONJ a)         = showString "CONJ" . (showString " ") . (shows a)
  showsPrec d (Punctuation a)  = showString "."    . (showString " ") . (shows a)
  showsPrec d (Comma a)        = showString ","    . (showString " ") . (shows a)
  showsPrec d (a :/ b)  = 
    ((showParen (isComplex a) (shows a)) . 
    (showString "/") . 
    (showParen (isComplex b) (shows b)))
  showsPrec d (a :\ b)  = 
    ((showParen (isComplex a) (shows a)) . 
    (showString "\\") . 
    (showParen (isComplex b) (shows b)))
