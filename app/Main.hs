module Main where
import System.Environment

data Movement = Up | Right | Left | Down | None deriving Eq

main :: IO ()
main = do
    fileName : _ <- getArgs
    fileContents <- parseFileContents <$> readFile fileName

    print $ parseAllTokens fileContents


parseAllTokens :: [[String]] -> String
parseAllTokens [] = error "empty token list"
parseAllTokens [_] = error "short token list"
parseAllTokens (levelT:nameT:bodyT) = 
    let
        level = parseLevel levelT
        name = parseName nameT
        body = map parseBodySection bodyT
    in 
        name ++ "ǇǇ" ++ show level ++ "Ǉ0Ǉ" ++ concatMap makePseudoFrameBitmap body

makePseudoFrameBitmap ::(Int, [Movement]) -> String
makePseudoFrameBitmap (frame, movements) = 
    show frame ++ "Ǉ" ++ movementBits ++ "Ǉ"
    where
        movementBits = map (head . show . fromEnum) [Up `elem` movements, Down `elem` movements, Main.Left `elem` movements, Main.Right `elem` movements]

parseFileContents :: String -> [[String]]
parseFileContents fileString = map words (filter (not . null) (lines fileString))

parseLevel :: [String] -> Int
parseLevel tokens = read (tokens !! 1) + 1

parseName :: [String] -> String
parseName tokens = tokens !! 1

parseBodySection :: [String] -> (Int, [Movement])
parseBodySection [] = error "unreachable"
parseBodySection (frame:movements) = (read frame, map toMovement movements)

toMovement :: String -> Movement
toMovement mstring =
    case mstring of
        "up" -> Up
        "right" -> Main.Right
        "left" -> Main.Left
        "down" -> Down 
        "none" -> None
        _ -> error "Unexpected movement token"

