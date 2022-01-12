{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Text.Show.Functions
data BinTree a b =
            Empty
            |Leaf b
            |Node (BinTree a b) a (BinTree a b)
            deriving Show


example :: BinTree (Int -> Bool) Char
example = Node(Node(Leaf 'g') (\x->x*x==x) (Node(Leaf 'u') (\x->x==0) (Leaf 'l'))) (\x->x>4) (Node(Leaf 'f') (\x->x>=7) (Leaf 'i'))

example2 :: BinTree Int Int
example2 = Node (Leaf 124) 12 (Leaf 123)

countInnerNodes :: BinTree a b -> Int
countInnerNodes (Leaf e) = 0
countInnerNodes (Node c f d)= 1 + countInnerNodes c + countInnerNodes d
countInnerNodes Empty = 0

decodeInt :: BinTree (Int -> Bool) b -> Int -> b
decodeInt (Node x y z) f    | y(f)==True  =decodeInt z f
                            | otherwise    =decodeInt x f
decodeInt (Leaf d) f=d
--fehlt noch was der Empty Fall



main :: IO()
main = print (decodeInt example 10)


