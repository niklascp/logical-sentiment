{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CCG where
import Lambda
import Pretty

--infix 5 :/
--infix 5 :\
infix 9 =?

data Feature = Masc  | Fem  | Neutr | MascOrFem   -- Gender
             | Sg    | Pl                         -- Number
             | Fst   | Snd  | Thrd                -- Person
             | Nom   | AccOrDat  | Gen            -- Case
             | Pers  | Refl | Wh
             | Tense | Infl
             | On    | With | By | To | From      -- Preposition
             | Dcl   | Adj  | Ng   | Pt           -- Sentence Type
             deriving (Eq,Show)

type Agreement = [Feature]

data Category = S Agreement             -- Sentence
              | N Agreement             -- Noun
              | NP Agreement            -- Noun Phrase
              | CONJ Agreement          -- Conjugation
              | Category :/ Category    -- Forward slash
              | Category :\ Category    -- Backward slash
              deriving (Eq)

data Word = Word String Category LTerm 
            deriving (Eq)

data PTree = PWord Word
           | PFwdApp Category LTerm PTree PTree 
           | PBwdApp Category LTerm PTree PTree 
           | PFwdComp Category LTerm PTree PTree 
           | PFwdTR Category LTerm PTree 
           | PConj Category LTerm PTree PTree PTree
           | PNounRaise Category LTerm PTree 
           deriving (Eq,Show)

data Tag = Tag { word :: Word,
                 score :: Float
               }
               deriving (Eq)

(=?) :: Category -> Category -> Bool
S    _ =? S    _ = True
NP   _ =? NP   _ = True
N    _ =? N    _ = True
CONJ _ =? CONJ _ = True
(a :/ b) =? (a' :/ b') = a =? a' && b =? b'
(a :\ b) =? (a' :\ b') = a =? a' && b =? b'
_      =? _      = False

isN :: Category -> Bool
isN (N _) = True
isN _     = False

isNP :: Category -> Bool
isNP (NP _) = True
isNP _      = False

isS :: Category -> Bool
isS (S _) = True
isS _     = False

isComplex :: Category -> Bool
isComplex (S _)  = False
isComplex (N _)  = False
isComplex (NP _) = False
isComplex _      = True

agreement :: Category -> Agreement
agreement (S a)  = a
agreement (N a)  = a
agreement (NP a) = a
agreement _      = error "Cannot extract agreement from non-atomic category."

category :: PTree -> Category
category (PWord (Word _ c _)) = c
category (PFwdApp c _ _ _) = c
category (PBwdApp c _ _ _) = c
category (PFwdComp c _ _ _) = c
category (PFwdTR c _ _) = c
category (PConj c _ _ _ _) = c
category (PNounRaise c _ _) = c

term :: PTree -> LTerm
term (PWord (Word _ _ t)) = t
term (PFwdApp _ t _ _) = t
term (PBwdApp _ t _ _) = t
term (PFwdComp _ t _ _) = t
term (PFwdTR _ t _) = t
term (PConj _ t _ _ _) = t
term (PNounRaise _ t _) = t


-- Pretty printing of data structures

instance Show Tag where
  showsPrec d (Tag w s) = (shows w) -- . (showString "@") . (shows s)

instance Show Word where
  showsPrec d (Word s c l) = (showString s) . (showString " ⊣ ") . (shows c) . (showString " : ") . (shows l)

instance Show Category where
  showsPrec d (S a)     = showString "S" . (showString " ") . (shows a)
  showsPrec d (N a)     = showString "N" . (showString " ") . (shows a)
  showsPrec d (NP a)    = showString "NP" . (showString " ") . (shows a)
  showsPrec d (CONJ a)    = showString "CONJ" . (showString " ") . (shows a)
  showsPrec d (a :/ b)  = 
    ((showParen (isComplex a) (shows a)) . 
    (showString "/") . 
    (showParen (isComplex b) (shows b)))
  showsPrec d (a :\ b)  = 
    ((showParen (isComplex a) (shows a)) . 
    (showString "\\") . 
    (showParen (isComplex b) (shows b)))



-- LaTeXify printing of data structures

instance Pretty PTree where
  render (PWord (Word s c t)) = "\\inference{\\token{" ++ s ++ "}}{" ++ (render c) ++ ":" ++ (render t) ++ "}"
  render (PFwdApp c t t1 t2) = "\\inference[>]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ ":"  ++ (render t) ++ "}"
  render (PFwdComp c t t1 t2) = "\\inference[B_>]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ ":" ++ (render t) ++ "}"
  render (PBwdApp c t t1 t2) = "\\inference[<]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ ":" ++ (render t) ++ "}"
  render (PFwdTR c t t1) = "\\inference[T_>]{" ++ (render t1) ++ "}{" ++ (render c) ++ ":" ++ (render t) ++ "}"
  render (PConj c t t1 t2 t3) = "\\inference[<\\&>]{" ++ (render t1) ++ (render t2) ++ (render t3) ++ "}{" ++ (render c) ++ ":" ++ (render t) ++ "}"
  render (PNounRaise c t t1) = "\\inference{" ++ (render t1) ++ "}{" ++ (render c) ++ ":" ++ (render t) ++ "}"

instance Pretty Category where
  render (S a)    = "\\cat{S}_{" ++ (render a) ++ "}"
  render (N a)    = "\\cat{N}_{" ++ (render a) ++ "}"
  render (NP a)   = "\\cat{NP}_{" ++ (render a) ++ "}"
  render (CONJ a) = "\\cat{CONJ}_{" ++ (render a) ++ "}"
  render (a :/ b) = (if (isComplex a) then "(" ++ (render a) ++ ")" else (render a)) ++
                    "/" ++
                    (if (isComplex b) then "(" ++ (render b) ++ ")" else (render b))
  render (a :\ b) = (if (isComplex a) then "(" ++ (render a) ++ ")" else (render a)) ++
                    " \\bsl " ++
                    (if (isComplex b) then "(" ++ (render b) ++ ")" else (render b))

instance Pretty Agreement where
  render (f:fs) | length fs > 0 = (show f) ++ "," ++ (render fs)  
                | otherwise     = show f
  render _                      = []