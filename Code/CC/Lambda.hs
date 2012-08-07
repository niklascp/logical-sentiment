module Lambda where
import Pretty;

data LTerm = LVar String
           | LAbs String LTerm
           | LApp LTerm LTerm
           | LFun String Int [LTerm]
           | LSeq [LTerm]
           | LAdd LTerm Int
           deriving (Eq)

lid = LAbs "x" $ LVar "x"

isComplexTerm :: LTerm -> Bool
isComplexTerm (LVar _)   = False
isComplexTerm (LApp _ _) = True
isComplexTerm (LAbs _ _) = False
isComplexTerm (LFun _ _ _) = False
isComplexTerm (LSeq _) = True
isComplexTerm (LAdd _ _) = True

instance Show LTerm where
  showsPrec d (LVar x)     = (showString x)
  showsPrec d (LAbs x t)   = (showString $ "λ" ++ x ++ ".") . (shows t)
  showsPrec d (LApp t1 t2) = (showParen (isComplexTerm t1) (shows t1)) .
                             (showString " ") . 
                             (showParen (isComplexTerm t2) (shows t2))
  showsPrec d (LFun f v [])  = showString $ f ++ "'" ++ (show v)
  showsPrec d (LFun f v ts)  = (showString $ f ++ "'" ++ (show v) ++ "(") . (showList' ts) . (showString ")")
                               where showList' :: Show a => [a] -> ShowS
                                     showList' [] = showString ""
                                     showList' [a] = shows a
                                     showList' (a1:a2:as) = (shows a1) . (showString ", ") . (showList' (a2:as))
  showsPrec d (LSeq ts) = (shows ts)
  showsPrec d (LAdd t1 v) = (shows t1) . (showString "'") . (shows v)

-- α-conversion
free :: LTerm -> String -> Bool
free (LVar x) x'     | x == x'   = True
                     | otherwise = False
free (LApp t1 t2) x'             = (free t1 x') && (free t2 x')
free (LAbs x t) x'   | x == x'   = False
                     | otherwise = (free t x')
free (LFun f _ ts) x'            = any (flip free x') ts
free (LSeq ts) x'                = any (flip free x') ts
free (LAdd t1 _) x'              = (free t1 x')

substitute :: LTerm -> String -> LTerm -> LTerm
substitute t@(LVar x) x' t'     | x == x'   = t'
                                | otherwise = t
substitute t@(LApp t1 t2) x' t'             = LApp (substitute t1 x' t') (substitute t2 x' t')
substitute t@(LAbs x t1) x' t'  | x == x'   = t -- x is bound in t, so do not continue
                                | free t' x = error $ "TODO: α-conversion of " ++ x ++ " in " ++ (show t) 
                                | otherwise = LAbs x (substitute t1 x' t')
substitute (LFun f v ts) x' t'              = LFun f v $ (map (\t -> substitute t x' t' )) ts
substitute (LSeq ts) x' t'                  = LSeq $ (map (\t -> substitute t x' t' )) ts
substitute (LAdd t1 v) x' t'                = LAdd (substitute t1 x' t') v

-- β-reduction
reduce :: LTerm -> LTerm 
reduce (LApp (LAbs x t) t') = reduce $ substitute (reduce t) x (reduce t')
reduce (LApp t1 t2) = if (t1 /= t1') then (reduce $ LApp t1' t2) else (LApp t1' t2)
                      where t1' = reduce t1
reduce (LAbs x t)   = LAbs x $ reduce t
reduce (LFun f v ts) = LFun f v $ map reduce ts
reduce (LSeq ts) = LSeq $ map reduce ts
reduce (LAdd (LAbs x t) v) = LAbs x $ reduce $ LAdd t v
reduce (LAdd (LFun f v' ts) v) = LFun f (v + v') $ map reduce ts
reduce (LAdd (LSeq ts) v) = LSeq $ map (reduce . flip LAdd v) ts
reduce (LAdd (LAdd t v') v) = LAdd (reduce t) (v + v')
reduce (LAdd t v)   = if (t /= t') then (reduce $ LAdd t' v) else (LAdd t' v)
                      where t' = reduce t
reduce x            = x

-- | Creates an infinite list of variables [x, x', x'', ...]
xVars :: [String]
xVars = iterate (++ "'") "x"

zVars :: [String]
zVars = iterate (++ "'") "z"

fVars :: [String]
fVars = iterate (++ "'") "f"

-- | Creates an infinite list of variables [x, y, z, x', y' z', x'', y'', z'', ...]
xyzVars :: [String]
xyzVars = [v ++ v' | v' <- (iterate (++ "'") ""), v <- ["x","y","z"]]

instance Pretty LTerm where
  render (LVar x) = x
  render (LAbs x t) = "\\lambda " ++ x ++ "." ++ 
                      (if (isComplexTerm t) then "(" ++ (render t) ++ ")" else (render t))
  render (LApp t1 t2) = (if (isComplexTerm t1) then "(" ++ (render t1) ++ ")" else (render t1)) ++
                        "\\;" ++ 
                        (if (isComplexTerm t2) then "(" ++ (render t2) ++ ")" else (render t2))
  render (LSeq [t]) = render t
  render (LSeq (t1:t2:ts)) = (render t1) ++ ", " ++ (render (LSeq (t2:ts)))
  render (LFun f v []) = "\\mathrm{" ++ f ++ "}_{" ++ (render v) ++ "}"
  render (LFun f v ts) = "\\mathrm{" ++ f ++ "}_{" ++ (render v) ++ "}(" ++ (showList' ts) ++ ")"
                         where showList' :: Pretty a => [a] -> String
                               showList' [] = ""
                               showList' [a] = render a
                               showList' (a1:a2:as) = (render a1) ++ ", " ++ (showList' (a2:as))
  render (LAdd t1 v) = (render t1) ++ "_{\\sim " ++ (render v) ++ "}"

instance Pretty Int where
  render i = if i < 0 then "\\overline{" ++ (show $ -i) ++ "}" else (show i)