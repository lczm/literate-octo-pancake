import Data.Array (Ix, Array, accumArray, (//), (!))
import Data.List (elemIndices)

makeArray :: (Ix i) => (i,i) -> e -> Array i e
makeArray ii e = accumArray undefined e ii []

(!=) :: (Ix i) => Array i e -> (i,e) -> Array i e
(!=) a (i,e) = a // [(i,e)]

solve :: Int -> [Char] -> [Int]
solve n xs = 
    let a = makeArray ('a','z') 0
        as = scanl (!=) a (zip xs [1..])
        is = zipWith (!) as xs
        ds = zipWith (-) [1..] is
        ss = map (n -) ds
        max' x = max (x - 1)
        ss' = scanl1 max' ss
        js = elemIndices 0 ss'
        ks = map (1 +) js
    in ks

solution :: String -> Int
solution = head . (solve 14)

main :: IO ()
main = getContents >>= (print . solution)