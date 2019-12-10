-- project2.hs file

-- https://wiki.haskell.org/Getting_the_current_date

import Data.Time.Clock
import Data.Time.Calendar

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

-- https://wiki.haskell.org/Getting_the_current_date

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving(Show, Ord, Eq)

-- whatDay :: Int -> Data.Time.Calendar.Day
-- whatDay xs | xs > 0 = Data.Time.Calendar.Day

data Month a = January | February | March | April | May | June | July | August
             | September | October | November | December
      deriving(Show, Ord, Eq)

data Set a = Empty
         | Entry a
         | Branch (Set a) (Set a)
   deriving(Show, Ord, Eq)

membership :: Ord a => a -> Set a -> Bool
membership x Empty = False
membership x (Entry y) = x == y
membership x (Branch (lhs) (rhs)) = membership x (lhs) || membership x (rhs)

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Entry x
insert x (Entry y) = Branch (Entry x) (Entry y)
insert x (Branch (lhs) (rhs)) | lhs == Empty = Branch (Entry x) (rhs)
                             | rhs == Empty = Branch (lhs) (Entry x)
                             | otherwise = Branch (Entry x) (Branch (lhs) (rhs))

delete :: Ord a => a -> Set a -> Set a
delete x Empty = Empty
delete x (Entry y) | x == y = Empty
                   | otherwise = Entry y
delete x (Branch (lhs) (rhs)) | lhs == Entry x = Branch (Empty) (rhs)
                             | rhs == Entry x = Branch (lhs) (Empty)
                             | lhs == Empty && rhs == Empty = Empty
                             | otherwise = Branch (lhs) (rhs)

-- Calendar :: IO (Integer, Int, Int) -> Month
-- data Calendar = Month
