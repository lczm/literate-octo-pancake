import Data.Maybe (mapMaybe)
import Control.Applicative (Applicative(liftA2))
import Control.Monad ((<=<))

-- utils
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = xs

iterate' :: (a -> a) -> (a -> Bool) -> (a -> b) -> a -> [b]
iterate' f p g = map g . takeWhile p . iterate f

split :: Eq a => a -> [a] -> [[a]]
split x = iterate' (dropWhile' p) (not . null) (takeWhile p)
    where p = (/= x)

-- main
data Shape = Rock | Paper | Scissors
data Outcome = Win | Draw | Lose

match :: Shape -> Shape -> Outcome
match Rock Rock = Draw
match Rock Paper = Win
match Rock Scissors = Lose
match Paper Rock = Lose
match Paper Paper = Draw
match Paper Scissors = Win
match Scissors Rock = Win
match Scissors Paper = Lose
match Scissors Scissors = Draw

outcomeScore :: Outcome -> Integer
outcomeScore Win = 6
outcomeScore Draw = 3
outcomeScore Lose = 0

shapeScore :: Shape -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

roundScore :: (Shape,Shape) -> Integer
roundScore (s1,s2) = outcomeScore (match s1 s2) + shapeScore s2

solve1 :: [(Shape,Shape)] -> Integer
solve1 = sum . map roundScore

deduce :: Shape -> Outcome -> Shape
deduce Rock Win = Paper
deduce Rock Draw = Rock
deduce Rock Lose = Scissors
deduce Paper Win = Scissors
deduce Paper Draw = Paper
deduce Paper Lose = Rock
deduce Scissors Win = Rock
deduce Scissors Draw = Scissors
deduce Scissors Lose = Paper

roundScore' :: (Shape,Outcome) -> Integer
roundScore' (s1,o) = shapeScore (deduce s1 o) + outcomeScore o

solve2 :: [(Shape,Outcome)] -> Integer
solve2 = sum . map roundScore'

parseShape1 :: Char -> Maybe Shape
parseShape1 'A' = Just Rock
parseShape1 'B' = Just Paper
parseShape1 'C' = Just Scissors
parseShape1 _   = Nothing

parseShape2 :: Char -> Maybe Shape
parseShape2 'X' = Just Rock
parseShape2 'Y' = Just Paper
parseShape2 'Z' = Just Scissors
parseShape2 _   = Nothing

parseOutcome :: Char -> Maybe Outcome
parseOutcome 'X' = Just Lose
parseOutcome 'Y' = Just Draw
parseOutcome 'Z' = Just Win
parseOutcome _   = Nothing

parseLine :: String -> Maybe (Char,Char)
parseLine [x,' ',y] = Just (x,y)
parseLine _         = Nothing

parse1 :: String -> [(Shape,Shape)]
parse1 = mapMaybe (parseShape <=< parseLine) . split '\n'
    where parseShape = \(a,b) -> liftA2 (,) (parseShape1 a) (parseShape2 b)

parse2 :: String -> [(Shape,Outcome)]
parse2 = mapMaybe (parse' <=< parseLine) . split '\n'
    where parse' = \(a,b) -> liftA2 (,) (parseShape1 a) (parseOutcome b)

solution1 :: String -> Integer
solution1 = solve1 . parse1

solution2 :: String -> Integer
solution2 = solve2 . parse2

main :: IO ()
main = getContents >>= (print . solution2)
