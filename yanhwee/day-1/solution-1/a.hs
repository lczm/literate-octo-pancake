import Data.List (sortBy)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = xs

iterate' :: (a -> a) -> (a -> Bool) -> (a -> b) -> a -> [b]
iterate' f p g = map g . takeWhile p . iterate f

split :: Eq a => a -> [a] -> [[a]]
split x = 
    let p = (/= x)
    in iterate' (dropWhile' p) (not . null) (takeWhile p)

solution1 :: String -> Integer
solution1 = maximum . map (sum . (map read)) . split "" . split '\n'

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

solution2 :: String -> Integer
solution2 = sum . take 3 . sortDesc . sln1
    where sln1 = map (sum . (map read)) . split "" . split '\n'

main :: IO ()
main = getContents >>= (print . solution2)