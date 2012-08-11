{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CCG (
  module Unification,
  module Lambda,
  module CCG
) where

import Unification
import Lambda
import Pretty

type Token = String   -- Lexical entry
type Lemma = String   -- Lemma of lexical entry
type Pos = String     -- Part of spearch

data Word = Word { 
              token :: Token,
              lemma :: Lemma,
              pos :: Pos,
              category :: Category, 
              lTerm :: LTerm 
            }
            deriving (Eq)

type Agreement = [Feature]

data Category = S Agreement             -- Sentence
              | N Agreement             -- Noun
              | NP Agreement            -- Noun Phrase
              | PP Agreement            -- Preposision Phrase
              | CONJ Agreement          -- Conjugation
              | Punctuation Agreement
              | Comma Agreement
              | Category :/ Category    -- Forward slash
              | Category :\ Category    -- Backward slash
              deriving (Eq)

data Feature = Masc  | Fem  | Neutr | MascOrFem   -- Gender
             | Sg    | Pl                         -- Number
             | Fst   | Snd  | Thrd                -- Person
             | Nom   | AccOrDat  | Gen            -- Case
             | Pers  | Refl | Wh
             | Tense | Infl
             | On    | With | By | To | From      -- Preposition
             | SDcl  | SAdj | SNb  | SNg  | SPt | SInv | SPss | SB    -- Sentence Type
             | Var String
             deriving (Eq,Show)

data PTree = PWord Word
           | PFwdApp Category LTerm PTree PTree 
           | PBwdApp Category LTerm PTree PTree 
           | PFwdComp Category LTerm PTree PTree 
           | PBwdComp Category LTerm PTree PTree 
           | PBwdXComp Category LTerm PTree PTree 
           | PFwdTR Category LTerm PTree 
           | PNounRaise Category LTerm PTree 
           deriving (Eq,Show)

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

isComplex :: Category -> Bool
isComplex (S _)           = False
isComplex (N _)           = False
isComplex (NP _)          = False
isComplex (PP _)          = False
isComplex (CONJ _)        = False
isComplex (Punctuation _) = False
isComplex (Comma _)       = False
isComplex _               = True

agreement :: Category -> Agreement
agreement (S a)           = a
agreement (N a)           = a
agreement (NP a)          = a
agreement (PP a)          = a
agreement (CONJ a)        = a
agreement (Punctuation a) = a
agreement (Comma a)       = a
agreement _      = error "Cannot extract agreement from non-atomic category."

arg :: Category -> Maybe Category
arg (_:\x) = Just x
arg (_:/x) = Just x
arg x      = Nothing 

res :: Category -> Maybe Category
res (x:\_) = Just x
res (x:/_) = Just x
res _      = Nothing 

args :: Category -> Int
args (x:\_) = 1 + (args x)
args (x:/_) = 1 + (args x)
args _      = 0;

term :: PTree -> LTerm
term (PWord w) = lTerm w
term (PFwdApp _ t _ _) = t
term (PBwdApp _ t _ _) = t
term (PFwdComp _ t _ _) = t
term (PBwdComp _ t _ _) = t
term (PBwdXComp _ t _ _) = t
term (PFwdTR _ t _) = t
term (PNounRaise _ t _) = t



-- Pretty printing of data structures

instance Show Word where
  showsPrec d (Word { token = t, lemma = lemma, pos = p, category = c, lTerm = l }) = 
    (showString t) . (showString "~") . (showString lemma) . (showString "/") . (showString p) .
    (showString " ⊣ ") . (shows c) . (showString " : ") . (shows l)

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



-- LaTeX "printing" of data structures

includePOS :: Bool
includePOS = False

includeLambda :: Bool
includeLambda = True

includeAgreement :: Bool
includeAgreement = False

instance Pretty PTree where
  render (PWord (Word { token = t, pos = p, category = c, lTerm = (LVar "?") })) = "\\inference{\\token{" ++ t ++ "}" ++ (if includePOS then "\\\\\\pos{" ++ (render p) ++ "}" else "") ++ "}{" ++ (render c) ++ "}"
  render (PWord (Word { token = t, pos = p, category = c, lTerm = l })) = "\\inference{\\token{" ++ t ++ "}" ++ (if includePOS then "\\\\\\pos{" ++ (render p) ++ "}" else "") ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render l) else "") ++ "}"
  render (PFwdApp c t t1 t2) = "\\inference[>]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PBwdApp c t t1 t2) = "\\inference[<]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PFwdComp c t t1 t2) = "\\inference[>B]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PBwdComp c t t1 t2) = "\\inference[<B]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PBwdXComp c t t1 t2) = "\\inference[<B_\\times]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PFwdTR c t t1) = "\\inference[>T]{" ++ (render t1) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PNounRaise c t t1) = "\\inference{" ++ (render t1) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"

instance Pretty Category where
  render (S a)    = "\\cat{S}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (N a)    = "\\cat{N}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (NP a)   = "\\cat{NP}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (PP a)   = "\\cat{PP}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (CONJ a) = "\\cat{CONJ}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (Punctuation a) = "\\cat{.}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (Comma a) = "\\cat{,}" ++ (if includeAgreement then "_{" ++ (render a) ++ "}" else "")
  render (a :/ b) = (if (isComplex a) then "(" ++ (render a) ++ ")" else (render a)) ++
                    "/" ++
                    (if (isComplex b) then "(" ++ (render b) ++ ")" else (render b))
  render (a :\ b) = (if (isComplex a) then "(" ++ (render a) ++ ")" else (render a)) ++
                    " \\bsl " ++
                    (if (isComplex b) then "(" ++ (render b) ++ ")" else (render b))

instance Pretty Agreement where
  render (f:fs) | length fs > 0 = (render f) ++ "," ++ (render fs)  
                | otherwise     = render f
  render _                      = []

instance Pretty Feature where
  render SDcl    = "dcl" 
  render SAdj    = "adj"
  render SNb     = "nb"
  render SNg     = "ng"
  render SPt     = "pt"
  render SInv    = "inv"
  render SPss    = "pss"
  render SB      = "b"
  render (Var x) = x
  render f      = show f