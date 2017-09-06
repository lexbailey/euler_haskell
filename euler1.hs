-- A solution to euler problem 1 in haskell
-- Run with: euler1 limit f1, f2, ... fn
-- where limit is the max value (999 for the euler task)
--       f1 etc is a list of multiples that should be 
--       included (3 and 5 for the euler task)
import System.Environment
import System.Exit
import Text.Read

isMultipleOf p f = p `mod` f == 0

isMultipleOfAny p = any (isMultipleOf p)

solve (limit:multiples) = sum ([x | x <- [1..limit], x `isMultipleOfAny` multiples])

parseArgs args = let l = length args in
    if l >= 2 then sequence (map readMaybe args)
    else Nothing

invalidArgs = error "Please specify two or more integer arguments (limit, f1, f2, ... fn)"

main = getArgs >>= maybe invalidArgs (print . solve) . parseArgs
