module Birds where
import Debug.Trace
import Data.Tree

toDataTree (L a) = Node (show a) []
toDataTree (Uni a t) = Node (show a) [toDataTree t]
toDataTree (Bin a l r) = Node (show a) [toDataTree l, toDataTree r]

infixl :>

data Proof a = L a | Bin a (Proof a) (Proof a) | Uni a (Proof a)
   deriving Eq

root (L a) = a
root (Bin a _ _) = a
root (Uni a _) = a

instance Show a => Show (Proof a) where
 show (L a) = show a
 show (Uni a t) = show a ++ " | " ++ show t ++ ""
 show (Bin a l r) = show a ++ " [" ++ show l ++ "] & [" ++ show r ++ "]"
                  
data CB = C String | A Int | CB :> CB
   deriving Eq

subterm :: CB -> [CB]
subterm x@(A _) = [x]
subterm x@(l :> r) = x:subterm l ++ subterm r

instance Show CB where
 show (A x) = show x
 show (C x) = x
 show (x :> y) = show x ++ " " ++ show' y

show' (x :> y) = "(" ++ show x ++ " " ++ show' y ++ ")"
show' a = show a

x = A 1
y = A 2
z = A 3
w' = A 4
v' = A 5

-- BASIC
k = (C "K" :> x :> A 0,x)
s = (C "S" :> x :> y :> z,x :> z :> (y :> z))
i = (C "I" :> x,x)
s1 = (C "S1" :> x :> y :> z,y :> z :> (x :> z))
-- MULT
m = (C "M" :> x,x :> x)
mx = (C "M*" :> x :> y,x :> y :> (x :> y))
l = (C "L" :> x :> y,x :> (y :> y))
h = (C "H" :> x :> y :> z,x :> y :> z :> y)
hx = (C "H*" :> x :> y :> z :> w',x :> y :> z :> w' :> z)
w = (C "W" :> x :> y,x :> y :> y)
w1 = (C "W1" :> x :> y,y :> x :> x)
wx = (C "W*" :> x :> y :> z,x :> y :> z :> z)
wxx = (C "W**" :> x :> y :> z :> w',x :> y :> z :> w' :> w')
phi = (C "Phi" :> x :> y :> z :> w',x :> (y :> w') :> (z :> w'))
psi = (C "Psi" :> x :> y :> z :> w',x :> (y :> z) :> (y :> w'))
-- COMM
t = (C "T" :> x :> y,y :> x)
v = (C "V" :> x :> y :> z,z :> x :> y)
vx = (C "V*" :> x :> y :> z :> w',x :> w' :> y :> z)
c = (C "C" :> x :> y :> z,x :> z :> y)
cx = (C "C*" :> x :> y :> z :> w',x :> y :> w' :> z)
cxx = (C "C*" :> x :> y :> z :> w' :> v',x :> y :> z :> v' :> w')
f = (C "F" :> x :> y :> z,z :> y :> x)
fx = (C "F*" :> x :> y :> z :> w',x :> w' :> z :> y)
r = (C "R" :> x :> y :> z,y :> z :> x)
rx = (C "R*" :> x :> y :> z :> w',x :> z :> w' :> y)
g = (C "G" :> x :> y :> z :> w',x :> w' :> (y :> z))
gx = (C "G*" :> x :> y :> z :> w' :> v',x :> y :> v' :> (z :> w'))
gxx = (C "G**" :> x :> y :> z :> w',x :> w' :> (x :> w') :>(y :> z))
-- ASSOC
b = (C "B" :> x :> y :> z,x :> (y :> z))
d = (C "D" :> x :> y :> z :> w',x :> y :> (z :> w'))
bx = (C "B*" :> x :> y :> z :> w',x :> (y :> z :> w'))
bxx = (C "B**" :> x :> y :> z :> w' :> v',x :> (y :> z :> w' :> v'))
bxxx = (C "B***" :> x :> y :> z :> w',x :> (y :> (z :> w')))
dx = (C "D*" :> x :> y :> z :> w' :> v',x :> y :> z :> (w' :> v'))
dxx = (C "D**" :> x :> y :> z :> w' :> v',x :> (y :> z) :> (w' :> v'))
e = (C "E" :> x :> y :> z :> w' :> v',x :> y :> (z :> w' :> v'))
ex = (C "E**" :> x :> y :> z :> w' :> v' :> A 6 :> A 7,x :> (y :> z :> w') :> (v' :> A 6 :> A 7))
-- ASSOC & COMM
q = (C "Q" :> x :> y :> z,y :> (x :> z))
qx = (C "Q*" :> x :> y :> z,x :> (z :> y))
qxx = (C "Q**" :> x :> y :> z,y :> (z :> x))
qxxx = (C "Q***" :> x :> y :> z,z :> (x :> y))
qxxxx = (C "Q****" :> x :> y :> z,z :> (y :> x))
-- ITER
th = (x :> y,y :> (x :> y))
u = (C "U" :> x :> y,y :> (x :> x :> y))
o = (C "O" :> x :> y,y :> (x :> y))

allcb = [k,s,i,s1,
      m,mx,l,h,hx,w,w1,wx,wxx,phi,psi,
      t,v,vx,c,cx,cxx,f,fx,r,rx,g,gx,gxx,
      b,d,bx,bxx,dx,bxxx,dxx,e,ex,
      q,qx,qxx,qxxx,qxxxx,
      u,o]
assoc = [b,bx,bxx,bxxx,d,dx,dxx,e]
combinators = [s,k,i]

len (l :> r) = len l + len r
len _ = 1

toList :: CB -> [CB]
toList (l :> r) = toList l ++ toList r
toList x = [x]

vars :: CB -> [Int]
vars (C _) = []
vars (A x) = [x]
vars (l :> r) = vars l ++ vars r

normal :: CB -> Bool
normal (C x) = True
normal (x :> A _) = normal x
normal (x :> y) = normal' x && normal' y
normal _ = False

normal' :: CB -> Bool
normal' (C x) = True
normal' (x :> y) = normal' x && normal' y
normal' _ = False

order :: CB -> Int
order t = maximum (order' 0 t)
order' i (l :> r) = order' i l ++ order' (i+1) r
order' i t = [i]

pos :: CB -> [(CB,Int)]
pos t = 
   zip ts is
   where
    ts = toList t   
    is = reverse [0..length ts -1]

dist :: CB -> [Int] -> Int
dist t vs = 
   i + 
   j + 
   dist' ts vs'   
   where
      ts = pos t
      j = order t
      i = len t
      vs' = zip (reverse vs) [0..]

dist' :: [(CB, Int)] -> [(Int, Int)] -> Int
dist' _ [] = 0
dist' ts ((v,i):vs) = 
   sum [ abs (j-i) | j <- myLookup v ts ]
   + dist' ts vs

myLookup :: Int -> [(CB, t)] -> [t]
myLookup v [] = []
myLookup v ((A v',j):vs) | v==v' = j:myLookup v vs
                         | otherwise = myLookup v vs
myLookup v (_:vs) = myLookup v vs

cmpCB vs x y = compare (dist x vs) (dist y vs)


