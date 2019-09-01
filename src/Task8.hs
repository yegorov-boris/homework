module Task8 where

import Text.Parsec hiding(digit)

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf $ ['0'..'9']

number :: (Floating a, Read a) => Parser a
number = do
  i <- many1 digit
  point <- optionMaybe $ char '.'
  case point of
    Nothing -> return $ read i
    Just _ -> do
      f <- many1 digit
      return $ read $ i ++ '.':f

negation :: (Floating a, Read a) => Parser a
negation = try negation' <|> addition
  where
    negation' = do
      char '-'
      x <- expr
      return $ negate x

byNumber :: Floating a =>
    Char
  -> (a -> a -> a)
  -> Parser a
  -> Parser (a -> a)
byNumber symbol func base =
                    do
                        char symbol
                        spaces
                        n <- base
                        spaces
                        return $ (`func` n)

powNumber :: (Floating a, Read a) => Parser (a -> a)
powNumber = byNumber '^' (**) expr

power :: (Floating a, Read a) => Parser a
power = do
           x <- expr
           spaces
           ys <- many powNumber
           return $ foldl (\ x f -> f x) x ys

multNumber :: (Floating a, Read a) => Parser (a -> a)
multNumber = byNumber '*' (*) power

divNumber :: (Floating a, Read a) => Parser (a -> a)
divNumber = byNumber '/' (/) power

multiplication :: (Floating a, Read a) => Parser a
multiplication = do
                    x <- power
                    spaces
                    ys <- many (multNumber <|> divNumber)
                    return $ foldl (\ x f -> f x) x ys

plusNumber :: (Floating a, Read a) => Parser (a -> a)
plusNumber = byNumber '+' (+) multiplication

minusNumber :: (Floating a, Read a) => Parser (a -> a)
minusNumber = byNumber '-' (-) multiplication

addition :: (Floating a, Read a) => Parser a
addition = do
                x <- multiplication
                spaces
                ys <- many (plusNumber <|> minusNumber)
                return $ foldl (\ x f -> f x) x ys

expr :: (Floating a, Read a) => Parser a
expr = number
        <|> do
                char '('
                spaces
                res <- negation
                char ')'
                spaces
                return $ res

root :: (Floating a, Read a) => Parser a
root = do
            spaces
            p <- negation
            eof
            return $ p

--main =
--        do
--            s <- getLine
--            putStrLn $ show $ parse root "<input>" s
--            main
