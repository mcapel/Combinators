module Unify where
import Birds

type SUB = [(CB,CB)]

subb :: SUB -> CB -> CB
subb [] = id
subb ((x,t):fs) = sub t x . subb fs

-- substitute t for x in v
sub :: CB -> CB -> CB -> CB
sub t (A x) (A y) | x==y = t
                  | otherwise = A y
sub t x@(A _) (l :> r) = (sub t x l) :> (sub t x r)
sub _ _ t@(C _) = t

-- unify Pattern Term
unify :: CB -> CB -> Maybe SUB
unify t1 t2 = unify' [(t1,t2)] []

unify' [] f = Just f
unify' ((x@(C _),y@(C _)):ts) f 
   | x==y = unify' ts f 
   | otherwise = Nothing
unify' ((l1:>r1,l2:>r2):ts) f =
   unify' ((l1,l2):(r1,r2):ts) f
unify' ((y@(A _),k@(A 0)):ts) f 
   | k==y = unify' ts f 
   | otherwise = do 
                 x <- lookup y f
                 unify' ts ((k,x):f)
unify' (p@(A _,t):ts) f 
   | free p f = unify' ts (p:f)
   | otherwise = Nothing
unify' _ _ = Nothing

free _ [] = True
free (x,t) ((y,t'):ys) = (x/=y || t==t') && free (x,t) ys

initialize :: CB -> [(CB, CB)] -> [(CB, CB)]
initialize l cs = 
   init' (ls++vs) [] cs
    where
     ls = vars l
     vs = concatMap (vars.fst) cs

init' :: [Int] -> [(CB, CB)] -> [(CB, CB)] -> [(CB, CB)]
init' _ cs [] = cs
init' vx cb ((cl,cr):cs) =
   init' vx' ((f cl,f cr):cb) cs 
   where
    vc = vars cl
    (f,vx') = refresh id vx vc

fresh :: [Int] -> Int
fresh xs = (maximum xs) + 1

refresh :: (CB -> CB) -> [Int] -> [Int] -> (CB -> CB, [Int])
refresh f bs [] = (f,bs)
refresh f bs (0:xs) = refresh f bs xs
refresh f bs (x:xs) = 
   refresh f' (x':bs) xs
   where
    x' = fresh bs
    f' = sub (A x') (A x) . f


