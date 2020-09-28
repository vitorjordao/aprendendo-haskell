isStringEven :: String -> Bool
isStringEven x = even (length x)

reverseList :: [String] -> [String]
reverseList (x:xs) = reverseList xs ++ [x]
reverseList x = x

evenWordsAndItsLength :: [String] -> [Int]
evenWordsAndItsLength xs =
    [length x | x<-xs, (isStringEven x) /= True]

newHead :: [a] -> a
newHead (x:list) = x

isPalindrome :: String -> Bool
isPalindrome str = str == (reverse str)

multiplayValue :: Num d => d -> (d, d, d, d)
multiplayValue val = (val * 2, val * 3, val * 4, val * 5)

main :: IO ()
main = do
    let stringEven = isStringEven "ABCDEF"

    print("String even ", stringEven)

    let stringList = ["ABCDEF", "b", "CIF"]

    let reversedList = reverseList stringList

    print("reversed list ", reversedList)

    let stringSize = evenWordsAndItsLength stringList

    print("string size ", stringSize)

    print("head ", newHead stringList)

    print("is palindrome ", isPalindrome (newHead stringList))

    let multiplayedValue = multiplayValue 3

    print("multiply value ", multiplayedValue)
    
