module Main where
import Text.Printf
import Control.Exception
import System.CPUTime
import Birds
import Proofs
import Execute

test1 = 
   [ (t,p,execute p) | (t,cs) <- pairs, 
                       p <- prove t cs {-, snd t /= execute p-} ] 

test = 
   [ (t,prove t cs) | (t,cs) <- pairs ] 

showCB (l,r) = show l ++ " = " ++ show r

proveIO (t,ts) = 
   do
   putStrLn ("Prove: \n" ++ showCB t)
   putStrLn ("Assumptions: " ++ concatMap (("\n"++).showCB) ts)
   putStrLn ("Proof: " ++ concatMap (("\n"++).showCB) ps)
   where
      ps = [ (p,execute p) | p <- prove t ts ]
 
runTest p = 
   do
    start <- getCPUTime
    v <- proveIO p
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

runTests [] = return ()
runTests (p:ps) = 
   runTest p 
   >> putStrLn "============================"
   >> runTests ps

main = 
   runTests pairs 

pairs = 
   [
   (d,[b]),
   (dx,[b]),   
   (dxx,[b]),   
   (bx,[b]),   
   (bxx,[b]),   
   (bxxx,[b]),   
   (e,[b]), 
-- (ex,[b]),   
   (m,[w,k]),
   (m,[w,i]),
   (i,[w,k]),
-- (i,[c,k]), -- really?
   (t,[c,i]),
   (r,[b,t]),
   (c,[r]),
   (r,[c]),
   (f,[b,r,c]),
   (f,[b,r]),
   (f,[b,c]),
   (f,[c,v]),
-- (f,[b,t]),
   (v,[f,c]),
   (v,[f,r]),
   (cx,[b,c]),
   (rx,[b,c]),
   (fx,[b,c]),
   (vx,[b,c]),
   (v,[cx,t]),
   (q,[b,c]),
   (q,[b,r]),
   (qx,[b,c]),
   (qx,[b,r]),
   (qxx,[b,c]),
   (qxx,[c,qxx]),
   (qxx,[c,qx]),
   (qxxx,[b,c]),
   (qxxxx,[b,c]),
   (qxxx,[c,qxxxx]),
   (qxxxx,[c,qxxx]),
   (qxxxx,[qx,t]),
   (b,[q,t]),
   (c,[q,t]),
   (g,[b,c]),
   (mx,[m,b]),
   (l,[b,t,m]),
   (l,[b,c,m]),
   (l,[b,r,m]),
   (l,[b,w]),
   (l,[m,q]),
   (w1,[m,b,t]),
   (w1,[m,b,r]),
--   (w,[m,b,r,c]),
--   (w,[m,b,t]),
   (m,[w,t]),
   (wx,[w,b]),
   (wxx,[w,b]),
   (h,[w,b,c]),
--   (h,[b,m,t]),
   (w,[h,b,c]),
   (w,[h,c,r]),
   (w,[h,r]),
--   (s,[b,t,m]),
   (s,[b,c,w]),
   (s,[b,g,w]),
   (s1,[c,s]),
   (s1,[b,t,s]),
   (s,[c,q,w]),
--   (w,[b,c,s]),
   (w,[c,s]),
   (w,[r,s]),
   (w,[t,s]),
   (h,[r,s]),
   (h,[c,s]),
   (m,[t,s]),
   (phi,[b,s]),
   (hx,[b,h]),
--   (psi,[b,s]),
   (psi,[hx,dxx]),
   (psi,[h,d,b]),
   (gx,[b,c]),
   (gxx,[b,c,s]),
   (gxx,[b,c,s,gx]),
   (u,[q,l,w,b]),
   (o,[c,w,b]),
   (o,[q,w]),
   (u,[o,l]),
   (m,[o,i]),
   (o,[s,i])
   ]


