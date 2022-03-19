module ReadCB where
import Birds
import Text.ParserCombinators.ReadP
import Data.Char (digitToInt,isSpace,isAlpha)

readTerm str = 
     case [ x | (x,"") <- readP_to_S expr str ] of
        [] -> error "Parse error"
        (x:_) -> return x

mainOp = munch1 isSpace >> return (:>)

expr = term `chainl1` mainOp

term =
    ground
    <++
    app

ground = 
    cons
    +++
    var

app = 
   do 
     char '('
     many space
     (t:ts) <- sepBy1 term (many1 space)
     many space
     char ')'
     return (foldl (:>) t ts)

var =
    do
     is <- munch1 isDigit
     return (A (conv (map digitToInt is)))

conv xs = convert' (reverse (zip (reverse xs) [0..]))
    where
     convert' [] = 0
     convert' ((i,j):is) = i*(10^j) + convert' is

cons = 
   do 
    m <- satisfy isUppercase
    ms <- munch isAlpha
    return (C (m:ms))

isUppercase x = any (x==) ['A'..'Z']      
isDigit x = any (x==) ['0'..'9']
space :: ReadP Char
space = char ' '


