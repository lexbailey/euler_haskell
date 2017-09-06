-- A solution to euler problem 3 in haskell
import System.Environment
import System.Exit
import Text.Read

isMultipleOf p f = p `mod` f == 0

maxSearch = ceiling . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime p = and [not (p `isMultipleOf` x) | x <- [2..maxSearch p]]

isPrimeFactorOf :: Int -> Int -> Bool
isPrimeFactorOf p n = and [isPrime p, n `isMultipleOf` p]

solve :: Int -> Int
solve n = head [x | x <- reverse [2..maxSearch n], x `isPrimeFactorOf` n]

parseArgs :: [String] -> Maybe Int
parseArgs args = let l = length args in
    if l == 1 then readMaybe (head args)
    else Nothing

invalidArgs = error "Please specify one number."

main = getArgs >>= maybe invalidArgs (print . solve) . parseArgs
