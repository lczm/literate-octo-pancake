import Data.Set (fromList, intersection, toList)
import Data.Char (ord, isLower, isUpper)

commons :: (Ord a) => [[a]] -> [a]
commons = toList . foldl1 intersection . map fromList

priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27
    | otherwise = undefined

iterate' :: (a -> a) -> (a -> Bool) -> (a -> b) -> a -> [b]
iterate' f p g = map g . takeWhile p . iterate f

group :: Int -> [a] -> [[a]]
group n = iterate' (drop n) (not . null) (take n)

solve :: [String] -> Integer
solve = sum . map (priority' . common') . group 3
    where common' = head . commons
          priority' = toInteger . priority

parse :: String -> [String]
parse = lines

solution :: String -> Integer
solution = solve . parse

main :: IO ()
main = getContents >>= (print . solution)