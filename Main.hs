data BinTree a b = Bool (BinTree a b) (BinTree a b) | Char deriving Show

example :: BinTree (Int -> Bool) Char
example = BinTree (\x -> (x == 2)) "test"

main = do
    print("-------- a) --------")
    print(example)