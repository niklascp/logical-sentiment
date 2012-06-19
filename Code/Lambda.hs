module Lambda where
import Pretty;

data LTerm = LVar String
           | LAbs String LTerm
           | LApp LTerm LTerm
           | LFun String [LTerm]
           | LAdd LTerm LTerm
           deriving (Eq)

isComplexTerm :: LTerm -> Bool
isComplexTerm (LVar _)   = False
isComplexTerm (LApp _ _) = True
isComplexTerm (LAbs _ _) = False
isComplexTerm (LFun _ _) = False
isComplexTerm (LAdd _ _) = False

atom :: String -> LTerm
atom x = LFun x []

fun1 :: String -> LTerm
fun1 x = LAbs "x" (LFun x [LVar "x"])

fun2 :: String -> LTerm
fun2 x = LAbs "x" (LAbs "y" (LFun x [LVar "y", LVar "x"]))

instance Show LTerm where
  showsPrec d (LVar x)     = (showString x)
  showsPrec d (LAbs x t)   = (showString $ "λ" ++ x ++ ".") . (shows t)
  showsPrec d (LApp t1 t2) = (showParen (isComplexTerm t1) (shows t1)) .
                             (showString " ") . 
                             (showParen (isComplexTerm t2) (shows t2))
  showsPrec d (LFun f [])  = (showString $ f ++ "'")
  showsPrec d (LFun f ts)  = (showString $ f ++ "'(") . (showList' ts) . (showString ")")
                             where showList' :: Show a => [a] -> ShowS
                                   showList' [] = showString ""
                                   showList' [a] = shows a
                                   showList' (a1:a2:as) = (shows a1) . (showString ", ") . (showList' (a2:as))
  showsPrec d (LAdd t1 t2) = (shows t1) . (shows t2)

-- α-conversion
free :: LTerm -> String -> Bool
free (LVar x) x'     | x == x'   = True
                     | otherwise = False
free (LApp t1 t2) x'             = (free t1 x') && (free t2 x')
free (LAbs x t) x'   | x == x'   = False
                     | otherwise = (free t x')
free (LFun f ts) x'              = any (flip free x') ts
free (LAdd t1 t2) x'             = (free t1 x') && (free t2 x')

substitute :: LTerm -> String -> LTerm -> LTerm
substitute t@(LVar x) x' t'     | x == x'   = t'
                                | otherwise = t
substitute t@(LApp t1 t2) x' t'             = LApp (substitute t1 x' t') (substitute t2 x' t')
substitute t@(LAbs x t1) x' t'  | x == x'   = t -- x is bound in t, so do not continue
                                | free t' x = error $ "TODO: α-conversion of " ++ x ++ " in " ++ (show t) 
                                | otherwise = LAbs x (substitute t1 x' t')
substitute (LFun f ts) x' t' = LFun f $ (map (\t -> substitute t x' t' )) ts
substitute (LAdd t1 t2) x' t'               = LAdd (substitute t1 x' t') (substitute t2 x' t')

-- β-reduction
reduce :: LTerm -> LTerm 
reduce (LApp (LAbs x t) t') = reduce $ substitute (reduce t) x (reduce t')
reduce (LApp t1 t2) = if (t1 /= t1') then (reduce $ LApp t1' t2) else (LApp t1' t2)
                      where t1' = reduce t1
reduce (LAbs x t)   = LAbs x $ reduce t
reduce (LAdd t1 t2) = LAdd (reduce t1) (reduce t2)
reduce x            = x

instance Pretty LTerm where
  render _ = "" -- Dont render for now...
  render (LVar x) = x
  render (LAbs x t) = "\\lambda " ++ x ++ "." ++ 
                      (if (isComplexTerm t) then "(" ++ (render t) ++ ")" else (render t))
  render (LApp t1 t2) = (if (isComplexTerm t1) then "(" ++ (render t1) ++ ")" else (render t1)) ++
                        "\\;" ++ 
                        (if (isComplexTerm t2) then "(" ++ (render t2) ++ ")" else (render t2))
  render (LFun f []) = "\\mathrm{" ++ f ++ "}"
  render (LFun f ts) = "\\mathrm{" ++ f ++ "}" ++ "(" ++ (showList' ts) ++ ")"
                       where showList' :: Pretty a => [a] -> String
                             showList' [] = ""
                             showList' [a] = render a
                             showList' (a1:a2:as) = (render a1) ++ ", " ++ (showList' (a2:as))
  render (LAdd t1 t2) = (render t1) ++ "\\text{-}" ++ (render t2)