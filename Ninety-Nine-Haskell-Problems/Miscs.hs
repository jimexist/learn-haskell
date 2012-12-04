dict = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

fullWords n | n < 10 = dict !! n
fullWords n = fullWords (n `div` 10) ++ "-" ++ fullWords (n `mod` 10)