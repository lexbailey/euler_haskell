-- A solution to euler problem 2 in haskell
import System.Environment
import System.Exit
import Text.Read

isMultipleOf p f = p `mod` f == 0

isEven n = n `isMultipleOf` 2

fibs a b = [a+b] ++ (fibs b (a+b))

smallFibs limit = takeWhile (<= limit) (fibs 0 1)

onlyEven l = filter isEven l

solve :: Int -> Int
solve limit = sum (onlyEven (smallFibs limit))

parseArgs args = let l = length args in
    if l == 1 then readMaybe (head args)
    else Nothing

invalidArgs = error "Please specify exactly one integer argument. (The limit)"

main = getArgs >>= maybe invalidArgs (print . solve) . parseArgs

