module Lambda where
import Pretty;
import Data.List (nub,union,(\\));

data LTerm = LVar String
           | LAbs String LTerm
           | LApp LTerm LTerm
           | LFun String Int Int [LTerm]
           | LSeq [LTerm]
           | LAdd LTerm Int
           deriving (Eq)

lid = LAbs "x" $ LVar "x"

isComplexTerm :: LTerm -> Bool
isComplexTerm (LVar _)        = False
isComplexTerm (LApp _ _)      = True
isComplexTerm (LAbs _ _)      = False
isComplexTerm (LFun _ _ _ _)  = False
isComplexTerm (LSeq _)        = True
isComplexTerm (LAdd _ _)      = True

-- α-conversion
free :: LTerm -> [String]
free (LVar x)         = [x]
free (LApp t1 t2)     = (free t1) `union` (free t2)
free (LAbs x t)       = (free t) \\ [x]
free (LFun _ _ _ ts)  = nub $ concat $ map free ts
free (LSeq ts)        = nub $ concat $ map free ts
free (LAdd t _)       = (free t)

substitute :: LTerm -> String -> LTerm -> LTerm
substitute t@(LVar x) x' t'     | x == x'   = t'
                                | otherwise = t
substitute t@(LApp t1 t2) x' t'             = LApp (substitute t1 x' t') (substitute t2 x' t')
substitute t@(LAbs x t1) x' t'  | x == x'   = t -- x is bound in t, so do not continue
                                | x `elem` free t' = error $ "TODO: α-conversion of " ++ x ++ " in " ++ (show t) 
                                | otherwise = LAbs x (substitute t1 x' t')
substitute (LFun f j k ts) x' t'            = LFun f j k $ (map (\t -> substitute t x' t' )) ts
substitute (LSeq ts) x' t'                  = LSeq $ (map (\t -> substitute t x' t' )) ts
substitute (LAdd t1 j) x' t'                = LAdd (substitute t1 x' t') j

-- β-reduction
reduce :: LTerm -> LTerm 
reduce (LApp (LAbs x t) t')       = reduce $ substitute (reduce t) x (reduce t')
reduce (LApp t1 t2)               = if (t1 /= t1') then (reduce $ LApp t1' t2) else (LApp t1' t2)
                                    where t1' = reduce t1
reduce (LAbs x t)                 = LAbs x $ reduce t
reduce (LFun f j k ts)            = LFun f j k $ map reduce ts
reduce (LSeq ts)                  = LSeq $ map reduce ts
reduce (LAdd (LAbs x t) v)        = LAbs x $ reduce $ LAdd t v
reduce (LAdd (LFun f j 0 ts) j')  = LFun f (j + j') 0 $ map reduce ts
reduce (LAdd (LFun f j k ts) j')  = LFun f j k $ map reduce $ (take (k - 1) ts) ++ [LAdd (ts !! (k - 1)) j'] ++ (drop (k + 1) ts)
reduce (LAdd (LSeq ts) v)         = LSeq $ map (reduce . flip LAdd v) ts
reduce (LAdd t j)                 = if (t /= t') then (reduce $ LAdd t' j) else (LAdd t' j)
                                    where t' = reduce t
reduce x                          = x

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


instance Show LTerm where
  showsPrec d (LVar x)          = (showString x)
  showsPrec d (LAbs x t)        = (showString $ "λ" ++ x ++ ".") . (shows t)
  showsPrec d (LApp t1 t2)      = (showParen (isComplexTerm t1) (shows t1)) .
                                  (showString " ") . 
                                  (showParen (isComplexTerm t2) (shows t2))
  showsPrec d (LFun f j _ [])   = (showString $ f ++ "'" ++ (show j))
  showsPrec d (LFun f j k ts)   = (showString $ f ++ "'" ++ (show j) ++ "(") . (showList' ts) . (showString ")")
                                    where showList' :: Show a => [a] -> ShowS
                                          showList' [] = showString ""
                                          showList' [a] = shows a
                                          showList' (a1:a2:as) = (shows a1) . (showString ", ") . (showList' (a2:as))
  showsPrec d (LSeq ts)         = (shows ts)
  showsPrec d (LAdd t1 v)       = (shows t1) . (showString "'") . (shows v)


instance Pretty LTerm where
  render (LVar x) = x
  render (LAbs x t) = "\\lambda " ++ x ++ "." ++ 
                      (if (isComplexTerm t) then "(" ++ (render t) ++ ")" else (render t))
  render (LApp t1 t2) = (if (isComplexTerm t1) then "(" ++ (render t1) ++ ")" else (render t1)) ++
                        "\\;" ++ 
                        (if (isComplexTerm t2) then "(" ++ (render t2) ++ ")" else (render t2))
  render (LSeq [t]) = render t
  render (LSeq (t1:t2:ts)) = (render t1) ++ ", " ++ (render (LSeq (t2:ts)))
  render (LFun f j 0 []) = "\\mathrm{" ++ f ++ "}_{" ++ (render j) ++ "}"
  render (LFun f j k ts) = "\\mathrm{" ++ f ++ "}_{" ++ (render j) ++ "}^{" ++ (render k) ++ "}(" ++ (showList' ts) ++ ")"
                         where showList' :: Pretty a => [a] -> String
                               showList' [] = ""
                               showList' [a] = render a
                               showList' (a1:a2:as) = (render a1) ++ ", " ++ (showList' (a2:as))
  render (LAdd t1 v) = (render t1) ++ "_{\\circ " ++ (render v) ++ "}"

instance Pretty Int where
  render i = if i < 0 then "\\overline{" ++ (show $ -i) ++ "}" else (show i)