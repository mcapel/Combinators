module Proofs where
import Birds
import Data.List
import Data.Tree
import Debug.Trace
import Unify
import Execute
import ReadCB

terminal i (l,r) t = 
   i == j && normal t && r==t'   
   where 
      t' = execute t
      j = length (vars t)

prove :: (CB, CB) -> [(CB, CB)] -> [CB]
prove x@(xl,xr) cs = 
   prove' x (vars xl) [xr] cs'
    where cs' = initialize xl cs

prove' x vs [] _ = []
prove' x vs (t:ts) cs = 
   if not (null ns) 
   then ns
   else prove' x vs ts1 cs
--   then trace (show q) ns
--   else trace (show q) prove' x vs ts1 cs
   where 
--     q = (t,dist t vs,[ (t', dist t' vs) | t' <- ts' ])
--     q = (t,[ (t', execute t') | t' <- ts', execute t' /= snd x])
     ts0 = scan t cs
     ts' = (nub ts0)\\[t]
     ns = filter (terminal (length vs) x) ts' 
     ts1 = sortBy (cmpCB vs) (nub (ts++ts'))

scan :: CB -> [(CB, CB)] -> [CB]
scan t@(l :> r) cs =
   iter t cs 
   ++
   [ l' :> r' | l' <- scan l cs, r' <- scan r cs ] 
scan t cs = [t] ++ iter t cs

iter :: CB -> [(CB, CB)] -> [CB]
iter _ [] = []
iter t ((cl,cr):cs) =
   case unify cr t of
      Nothing -> iter t cs
      Just f -> subb f cl:iter t cs


