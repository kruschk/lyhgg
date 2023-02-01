main = interact respondPalindromes

respondPalindromes = unlines . map answer . lines
  where answer line = if line == reverse line
                      then "palindrome"
                      else "not a palindrome"
