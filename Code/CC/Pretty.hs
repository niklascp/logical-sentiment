{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Pretty (
  Pretty,
  latexify,
  render)
where

import Data.Char

import System.Directory (setCurrentDirectory)
import System.Cmd (rawSystem)

import Lambda
import CCG

-- | Class for LaTeX "printing" of data structures
class Pretty a where
  render :: a -> String

instance Pretty String where
  render s = concat $ map f s
             where f s = case s of
                           '$'  -> "\\$"
                           '\'' -> "\\'"
                           x    -> [x]

instance Pretty PTree where
  render (PWord (Word { token = t, pos = p, category = c, expr = (Var "?") })) = "\\inference{\\token{" ++ t ++ "}" ++ (if includePOS then "\\\\\\pos{" ++ (render p) ++ "}" else "") ++ "}{" ++ (render c) ++ "}"
  render (PWord (Word { token = t, pos = p, category = c, expr = e })) = "\\inference{\\token{" ++ t ++ "}" ++ (if includePOS then "\\\\\\pos{" ++ (render p) ++ "}" else "") ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render e) else "") ++ "}"
  render (PFwdApp c t t1 t2) = "\\inference[>]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PBwdApp c t t1 t2) = "\\inference[<]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PFwdComp c t t1 t2) = "\\inference[>B]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PBwdComp c t t1 t2) = "\\inference[<B]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PBwdXComp c t t1 t2) = "\\inference[<B_\\times]{" ++ (render t1) ++ (render t2) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PFwdTR c t t1) = "\\inference[>T]{" ++ (render t1) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"
  render (PLexRaise c t t1) = "\\inference{" ++ (render t1) ++ "}{" ++ (render c) ++ (if includeLambda then ":" ++ (render t) else "") ++ "}"

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

instance Pretty SExpr where
  render (Var x) = x
  render (Abs x t) = "\\lambda " ++ x ++ "." ++ 
                     (if (isComplexExpr t) then "(" ++ (render t) ++ ")" else (render t))
  render (App t1 t2) = (if (isComplexExpr t1) then "(" ++ (render t1) ++ ")" else (render t1)) ++
                       "\\;" ++ 
                       (if (isComplexExpr t2) then "(" ++ (render t2) ++ ")" else (render t2))
  render (Seq [t]) = render t
  render (Seq (t1:t2:ts)) = (render t1) ++ ", " ++ (render (Seq (t2:ts)))
  render (Fun f j 0 []) = "\\mathrm{" ++ f ++ "}_{" ++ (show j) ++ "}"
  render (Fun f j k ts) = "\\mathrm{" ++ f ++ "}_{" ++ (show j) ++ "}^{" ++ (show k) ++ "}(" ++ (showList' ts) ++ ")"
                        where showList' :: Pretty a => [a] -> String
                              showList' [] = ""
                              showList' [a] = render a
                              showList' (a1:a2:as) = (render a1) ++ ", " ++ (showList' (a2:as))
  render (ImpactChange t k') = "(" ++ (render t) ++ ")^{\\leadsto " ++ (show k') ++ "}"
  render (Change t1 v) = (render t1) ++ "_{\\circ " ++ (show v) ++ "}"
  render (Scale t1 v) = (render t1) ++ "_{\\bullet " ++ (show v) ++ "}"

latexify :: Pretty a => a -> Int -> IO ()
latexify a i = do writeFile ("Pretty/Content" ++ (show i) ++ ".tex") $ "\\newcommand{\\content}{$$\n" ++ (render a) ++ "\n$$}\n"
                  --setCurrentDirectory "Pretty"
                  --rawSystem "/usr/texbin/pdflatex" ["-jobname=" ++ (show i),"-interaction=batchmode", "Maker.tex"]
                  --rawSystem "open" ["Maker.pdf"]
                  return ()

includePOS :: Bool
includePOS = False

includeLambda :: Bool
includeLambda = True

includeAgreement :: Bool
includeAgreement = True
