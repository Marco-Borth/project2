data Cell = Open
            | Red
            | Black
      deriving(Show, Ord, Eq)

-- data Col a = []
--      deriving(Show, Ord, Eq)

data Set a = Empty
          | Entry a
          | Branch (Set a) (Set a)
    deriving(Show, Ord, Eq)

membership :: Ord a => a -> Set a -> Bool
membership x Empty = False
membership x (Entry y) = x == y
membership x (Branch (lhs) (rhs)) = membership x (lhs) || membership x (rhs)

place :: Ord a => a -> Set a -> Set a
place x Empty = Entry x
place x (Entry y) = Branch (Entry x) (Entry y)
place x (Branch (lhs) (rhs)) | lhs == Empty = Branch (Entry x) (rhs)
                              | rhs == Empty = Branch (lhs) (Entry x)
                              | otherwise = Branch (Entry x) (Branch (lhs) (rhs))

remove :: Ord a => a -> Set a -> Set a
remove x Empty = Empty
remove x (Entry y) | x == y = Empty
                    | otherwise = Entry y
remove x (Branch (lhs) (rhs)) | lhs == Entry x = Branch (Empty) (rhs)
                              | rhs == Entry x = Branch (lhs) (Empty)
                              | lhs == Empty && rhs == Empty = Empty
                              | otherwise = Branch (lhs) (rhs)

-- place :: Ord a => a -> Set a -> Set a
-- place x Empty = insert x Empty -- ( Branch ( Empty ) ( Branch ( Empty ) ( Branch ( Empty ) ( Branch ( Empty ) ( Branch ( Empty ) ( Branch ( Empty ) ( Empty ) ) ) ) ) ) )
