import Data.List
decprod :: Int -> Int -> [Int]
decprod a b = reverse $ nub $ sort $ concat [[a' * b' | a' <- [1..a] ] | b' <- [1..b] ]

isPalindrome :: Int -> Bool
isPalindrome s = (show s) == reverse (show s)

largestPalindrome :: Int -> Int -> Maybe Int
largestPalindrome a b = find isPalindrome (decprod a b)

main = maybe (error "No palindrome found") print (largestPalindrome 999 999)
