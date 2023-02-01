evaluateRPN :: (Num a, Read a) => String -> a
evaluateRPN = head . foldl f [] . words
    where f (first:second:rest) "*" = first * second : rest
          f (first:second:rest) "+" = first + second : rest
          f (first:second:rest) "-" = second - first : rest
          f (first:second:rest) "/" = second - first : rest
          f stack number = read number : stack
