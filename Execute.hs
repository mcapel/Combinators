module Execute
where
import Birds
import Unify
import Debug.Trace

convert' :: [Int] -> CB -> CB
convert' [] r = r
convert' (i:is) r = 
   convert' is r' 
   where 
    r' = eliminate (A i) r

convert :: (CB, CB) -> CB
convert (C _,r) = r
convert (l :> x@(A _),r) = 
   convert (l,r') 
   where 
    r' = eliminate x r

eliminate _ y@(C _) = C "K" :> y
eliminate x y@(A _) | x==y = C "S" :> C "K" :> C "K" 
                    | otherwise = C "K" :> y
eliminate x (l :> r) = 
   C "S" :> l' :> r' 
   where 
     l' = eliminate x l
     r' = eliminate x r

find' (C x) (C y) = x==y
find' x (l :> r) = find' x l 
find' _ _ = False

find x [] = Nothing
find x ((l,r):cs) = 
   if find' x l
   then Just (l,r)
   else find x cs

execute :: CB -> CB
execute x = 
   foldl (:>) y ys 
     where 
      (y:ys) = execute' x [] allcb'
      allcb' = initialize x allcb

execute' (l :> r) ts cs = execute' l (execute r:ts) cs
execute' t@(C _) ts cs = 
   case find t cs of
   Nothing -> error ("CB not found" ++ show t)
   Just (l,r) -> 
        let
         i = length (vars l)
        in if i > length ts
           then t:ts
           else let 
                 (ls,rs) = splitAt i ts
                 t' = foldl (:>) t ls
                 w = trace (show (l,t'))
                in case unify l t' of
                     Nothing -> error "Unification failed"
                     Just f -> execute' (subb f r) rs cs
execute' t ts cs = t:ts

--execute' (C "B") (x:y:z:cs) = execute' (x:>(y:>z)) cs
--execute' (C "S") (x:y:z:cs) = execute' (x:>z:>(y:>z)) cs
--execute' (C "K") (x:y:cs) = execute' x cs
--execute' (C "I") (x:cs) = execute' x cs
--execute' (l :> r) cs = execute' l (execute r:cs)
--execute' x cs = x:cs

