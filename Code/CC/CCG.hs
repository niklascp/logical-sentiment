{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CCG (
  module Unification,
  module Lambda,
  module CCG
) where
import Data.Char

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
              expr :: SExpr 
            }
            deriving (Eq)

infix 9 :/  -- Forward slash operator
infix 9 :\  -- Backward slash operator

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
             | SDcl  | SAdj | SNb  | SNg  | SPt | SInv | SPss | SB | SEm   -- Sentence Type
             | FThr
             | FWq
             | FQ
             | FFor
             | FQem
             | FVar String
             deriving (Eq,Show)

data PTree = PWord Word
           | PFwdApp Category SExpr PTree PTree 
           | PBwdApp Category SExpr PTree PTree 
           | PFwdComp Category SExpr PTree PTree 
           | PBwdComp Category SExpr PTree PTree 
           | PBwdXComp Category SExpr PTree PTree 
           | PFwdTR Category SExpr PTree 
           | PNounRaise Category SExpr PTree 
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

-- args :: Category -> Int
-- args (x:\_) = 1 + (args x)
-- args (x:/_) = 1 + (args x)
-- args _      = 0;

nodeExpr :: PTree -> SExpr
nodeExpr (PWord w) = expr w
nodeExpr (PFwdApp _ t _ _) = t
nodeExpr (PBwdApp _ t _ _) = t
nodeExpr (PFwdComp _ t _ _) = t
nodeExpr (PBwdComp _ t _ _) = t
nodeExpr (PBwdXComp _ t _ _) = t
nodeExpr (PFwdTR _ t _) = t
nodeExpr (PNounRaise _ t _) = t

nodeCategory :: PTree -> Category
nodeCategory (PWord w)           = category w
nodeCategory (PFwdApp c _ _ _)   = c
nodeCategory (PBwdApp c _ _ _)   = c
nodeCategory (PFwdComp c _ _ _)  = c
nodeCategory (PBwdComp c _ _ _)  = c
nodeCategory (PBwdXComp c _ _ _) = c
nodeCategory (PFwdTR c _ _)      = c
nodeCategory (PNounRaise c _ _)  = c


-- Pretty printing of data structures

instance Show Word where
  showsPrec d (Word { token = t, lemma = lemma, pos = p, category = c, expr = e }) = 
    (showString t) . (showString "~") . (showString lemma) . (showString "/") . (showString p) .
    (showString " ⊣ ") . (shows c) . (showString " : ") . (shows e)

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
includePOS = True

includeLambda :: Bool
includeLambda = True

includeAgreement :: Bool
includeAgreement = True

instance Pretty PTree where
  render (PWord (Word { token = t, pos = p, category = c, expr = (Var "?") })) = "\\inference{\\token{" ++ t ++ "}" ++ (if includePOS then "\\\\\\pos{" ++ (render p) ++ "}" else "") ++ "}{" ++ (render c) ++ "}"
  render (PWord (Word { token = t, pos = p, category = c, expr = e })) = "\\inference{\\token{" ++ t ++ "}" ++ (if includePOS then "\\\\\\pos{" ++ (render p) ++ "}" else "") ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render e) else "") ++ "}"
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
  render (FVar x) = x
  render f      = map toLower $ drop 1 $ show f