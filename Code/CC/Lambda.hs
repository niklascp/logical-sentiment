module Lambda where
import Data.List (nub,union,(\\));


-- | Data structure for semantic expressions
data SExpr = Var String                    -- Variable
           | Abs String SExpr              -- Lambda abstraction
           | App SExpr SExpr               -- Lambda application
           | Fun String Float Int [SExpr]  -- Functor
           | Seq [SExpr]                   -- Sequence
           | ImpactChange SExpr Int        -- Impact change
           | Change SExpr Float            -- Change
           | Scale SExpr Float             -- Scale
           deriving (Eq)


-- | Returns the set of free variables in the given expression
free :: SExpr -> [String]
free (Var x)            = [x]
free (App e1 e2)        = (free e1) `union` (free e2)
free (Abs x e)          = (free e) \\ [x]
free (Fun _ _ _ es)     = nub $ concat $ map free es
free (ImpactChange e _) = (free e)
free (Seq es)           = nub $ concat $ map free es
free (Change e _)       = (free e)
free (Scale e _)        = (free e)


-- | Safe substitution of variables x' with e' in e
subst :: SExpr -> String -> SExpr -> SExpr
subst e@(Var x) x' e'     | x == x'   = e'
                          | otherwise = e
subst e@(App e1 e2) x' e'             = App (subst e1 x' e') (subst e2 x' e')
subst e@(Abs x e1) x' e'  | x == x'   = 
                              -- x is bound in e, so do not continue
                              e
                          | x `elem` free e' = 
                              -- x is in FV(e'), need alpha-conversion of x:
                              let x'' = head $ xVars \\ (free e1 `union` free e')
                              in  subst (Abs x'' $ subst e1 x (Var x'')) x' e'
                          | otherwise =
                              -- otherwise just continue 
                              Abs x (subst e1 x' e')
subst e@(Fun f j k es) x' e'          = Fun f j k $ (map (\e1 -> subst e1 x' e' )) es
subst e@(Seq es) x' e'                = Seq $ (map (\e1 -> subst e1 x' e' )) es
subst e@(ImpactChange e1 k') x' e'    = ImpactChange (subst e1 x' e') k'
subst e@(Change e1 j) x' e'           = Change (subst e1 x' e') j
subst e@(Scale e1 j) x' e'            = Scale (subst e1 x' e') j


-- | Reduces a semantic expression
reduce :: SExpr -> SExpr 
-- β-reduction
reduce (App (Abs x t) t')         = reduce $ subst (reduce t) x (reduce t')
reduce (App t1 t2)                = if (t1 /= t1') then (reduce $ App t1' t2) else (App t1' t2)
                                    where t1' = reduce t1
reduce (Abs x t)                  = Abs x $ reduce t
reduce (Fun f j k ts)             = Fun f j k $ map reduce ts
reduce (Seq ts)                   = Seq $ map reduce ts
-- FC1, FC2, SC and PC rules:
reduce (Change (Fun f j 0 ts) j') = Fun f (j + j') 0 $ map reduce ts
reduce (Change (Fun f j k ts) j') = Fun f j k $ map reduce $ (take (k - 1) ts) ++ 
                                        [Change (ts !! (k - 1)) j'] ++ (drop k ts)
reduce (Change (Seq ts) j')       = Seq $ map (reduce . flip Change j') ts
reduce (Change (Abs x t) j')      = Abs x $ reduce $ Change t j'
reduce (Change t j)               = if (t /= t') then (reduce $ Change t' j) else (Change t' j)
                                    where t' = reduce t
-- FS1, FC2, SS and PS rules:
reduce (Scale (Fun f j 0 ts) j')  = Fun f (if j == 0 then j' else j * j') 0 $ map reduce ts
reduce (Scale (Fun f j k ts) j')  = Fun f j k $ map reduce $ (take (k - 1) ts) ++  
                                        [Scale (ts !! (k - 1)) j'] ++ (drop k ts)
reduce (Scale (Seq ts) v)         = Seq $ map (reduce . flip Scale v) ts
reduce (Scale (Abs x t) v)        = Abs x $ reduce $ Scale t v
reduce (Scale t j)                = if (t /= t') then (reduce $ Scale t' j) else (Scale t' j)
                                    where t' = reduce t
-- IC rule:
reduce (ImpactChange (Fun f j k ts) k') = Fun f j k' ts
reduce (ImpactChange t k')              = if (t /= t') then 
                                            (reduce $ ImpactChange t' k') 
                                          else 
                                            (ImpactChange t k')
                                          where t' = reduce t
-- Otherwise
reduce x                            = x

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

-- | Identity semantic expression
lid = Abs "x" $ Var "x"

-- | Determines if the expression complex, i.e. needs parenthesis
isComplexExpr :: SExpr -> Bool
isComplexExpr (App _ _)          = True
isComplexExpr (Seq _)            = True
isComplexExpr (ImpactChange _ _) = True
isComplexExpr (Change _ _)       = True
isComplexExpr (Scale _ _)        = True
isComplexExpr _                  = False

-- | Pretty printing of data structures
instance Show SExpr where
  showsPrec d (Var x)          = (showString x)
  showsPrec d (Abs x t)        = (showString $ "\\" ++ x ++ ".") . (shows t)
  showsPrec d (App t1 t2)      = (showParen (isComplexExpr t1) (shows t1)) .
                                 (showString " ") . 
                                 (showParen (isComplexExpr t2) (shows t2))
  showsPrec d (Fun f j _ [])   = (showString $ f ++ "'" ++ (show j))
  showsPrec d (Fun f j k ts)   = (showString $ f ++ "'" ++ (show j) ++ "(") . 
                                 (showList' ts) . (showString ")")
                                 where showList' :: Show a => [a] -> ShowS
                                       showList' [] = showString ""
                                       showList' [a] = shows a
                                       showList' (a1:a2:as) = (shows a1) . 
                                                              (showString ", ") . 
                                                              (showList' (a2:as))  
  showsPrec d (ImpactChange t k') = (shows t) . (showString "->") . (shows k')
  showsPrec d (Seq ts)         = (shows ts)
  showsPrec d (Change t1 v)    = (shows t1) . (showString "⚪") . (shows v)
  showsPrec d (Scale t1 v)     = (shows t1) . (showString "⚫") . (shows v)
