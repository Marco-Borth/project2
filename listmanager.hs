-- connect4.hs file

import System.Random

-- commands found under zvon list commands http://zvon.org/other/haskell/Outputlist/index.html
nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\y -> not (eq x y)) xs)

delete                  :: Eq a => a -> [a] -> [a]
delete                  =  deleteBy (==)

deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

union                   :: Eq a => [a] -> [a] -> [a]
union                   =  unionBy (==)

unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs
-- commands found under zvon list commands http://zvon.org/other/haskell/Outputlist/index.html

second xs = head (tail xs)

sortAscending xs | length xs < 2 = xs
    | otherwise = [minimum xs] ++ sortAscending (delete (minimum xs) xs)

sortedAscending xs | length xs == 0 = True
                   | length xs == 1 = True
                   | head xs <= second xs = sortedAscending (tail xs)
                   | otherwise = False

sortDescending xs | length xs < 2 = xs
    | otherwise = [maximum xs] ++ sortDescending (delete (maximum xs) xs)

sortedDescending xs | length xs == 0 = True
                    | length xs == 1 = True
                    | head xs >= second xs = sortedDescending (tail xs)
                    | otherwise = False

push a xs = [a] ++ xs
pushback a xs = xs ++ [a]

rev xs | length xs > 0 = [last xs] ++ reverse (pop (last xs) xs)
       | otherwise = xs

-- insert a b xs | b <= 0 = [a] ++ xs
--              | b >= length xs = xs + [a]
--              | otherwise = xs

linkLists xs ys = xs ++ ys

insertList xs ys a | a <= 0 = linkLists xs ys
                   | a >= length ys = linkLists ys xs
                   | otherwise = take a xs ++ ys ++ drop a xs

pop a xs | length xs > 0 = delete a xs
         | otherwise = xs

mult a xs | length xs > 0 = [head xs * a] ++ mult a (tail xs)
          | otherwise = xs

-- head xs / a
-- round ( (head xs) / ( a ) )
quotient a xs | length xs > 0 = [ head xs / a ] ++ quotient a (tail xs)
              | otherwise = xs

shuffle xs | length xs <= 0 = xs
           | length xs `mod` 4 == 1 = [last xs] ++ shuffle (pop (last xs) xs)
           | length xs `mod` 4 == 3 = [head xs] ++ shuffle (tail xs)
           | length xs `mod` 4 == 0 = [second (tail xs)] ++ shuffle (pop (second (tail xs) ) xs)
           | otherwise = [second xs] ++ shuffle ( pop (second xs) xs )
