import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Ord (comparing)
import Data.List ( minimumBy, delete, maximumBy )

getRandomWordWithLength :: Int -> IO String
getRandomWordWithLength targetLength = do
  wordsList <- readFile "words.txt" 
  let wordArray = lines wordsList
      filteredWords = filter (\word -> length word == targetLength) wordArray
  if null filteredWords
    then error "no words of the given length exist in the list"
    else do
      index <- randomRIO (0, length filteredWords - 1)
      return $ filteredWords !! index

getRandomWord :: [String] -> IO String
getRandomWord words = do
  index <- randomRIO(0, length words - 1)
  return $ words !! index

checkGuess :: String -> String -> String
checkGuess target guess =
  let correctChars = zipWithColor target guess target 
      feedback = if target == guess then "Congratulations! You guessed the word." else correctChars
  in feedback

zipWithColor :: String -> String -> String -> String
zipWithColor _ [] _ = []
zipWithColor [] _ _ = []
zipWithColor (currLetter:word) (g:guess) target
  | currLetter == g = "[green] " ++ zipWithColor word guess target
  | g `elem` target = "[yellow] " ++ zipWithColor word guess target
  | otherwise = "[grey] " ++ zipWithColor word guess target

zipWithColorSingle :: String -> String -> String -> String
zipWithColorSingle _ [] _ = []
zipWithColorSingle [] _ _ = []
zipWithColorSingle (currLetter:word) (g:guess) target
  | currLetter == g = "g" ++ zipWithColorSingle word guess target
  | g `elem` target = "y" ++ zipWithColorSingle word guess target
  | otherwise = "b" ++ zipWithColorSingle word guess target

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      larger  = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ larger

getYellowLetter :: String -> String -> String -> String
getYellowLetter [] _ _ = []
getYellowLetter (g:guess) (t:r) target 
  | g /= t && g `elem` target = g : getYellowLetter guess r target
  | otherwise =  getYellowLetter guess r target

getGreyLetters :: String -> String -> String -> String
getGreyLetters [] greyLetters _ = greyLetters
getGreyLetters (g:guess) greyLetters target
 | g `notElem` (greyLetters ++ target) = getGreyLetters guess (g:greyLetters) target
 | otherwise = getGreyLetters guess greyLetters target

wordContainsAlreadyKnownGreyLetter :: String -> String -> Bool
wordContainsAlreadyKnownGreyLetter word = any (`elem` word)

wordContainsAllGreen :: String -> String -> Bool
wordContainsAllGreen word [] = True
wordContainsAllGreen (w:word) (currLetter:r) = (currLetter == '_' || currLetter == w) && wordContainsAllGreen word r

getGreenLetters :: String -> String -> String
getGreenLetters [] _ = []
getGreenLetters _ [] = []
getGreenLetters (g:guess) (t:target)
 | g == t = g : rest
 | otherwise = '_' : rest
 where rest = getGreenLetters guess target

getWeight :: String -> String -> Int
getWeight word target =1 * length(getYellowLetter word target target) + 3 * greenLetters 
  where greenLetters = length (filter (/= '_') (getGreenLetters word target))

getWeightWords :: String -> [String] -> Int
getWeightWords guess words =  sum (map  (`getWeight` guess) words)

removeBadWords :: String -> String -> [String] -> [String]
removeBadWords potentialTarget givenGuess = 
  filter(\x -> 
    not (wordContainsAlreadyKnownGreyLetter x (getGreyLetters givenGuess [] potentialTarget ))
    && 
    wordContainsAllGreen x (getGreenLetters givenGuess potentialTarget))

optimalWords :: [String] -> String
optimalWords words =  fst (maximumBy (comparing snd) [(a,b) | a<-words,let b = getWeightWords a words])

gameLoopEasy :: String -> Integer -> [String] -> [String] -> String -> String -> IO ()
gameLoopEasy target attempts possibleWords triedWords greyLetters greenLetters = do
  putStrLn $ "Attempts left: " ++ show attempts 
  putStrLn "Enter your guess:"
  guess <- getLine
  handleGuess target attempts possibleWords triedWords greyLetters greenLetters guess

handleGuess :: String -> Integer -> [String] -> [String] -> String -> String -> String -> IO ()
handleGuess target attempts possibleWords triedWords greyLetters greenLetters guess
  | guess == "/info" = do
      putStrLn $ "Tried words: " ++ show triedWords
      putStrLn $ "Grey Letters: " ++ quicksort greyLetters
      putStrLn $ "Green Letters: " ++ greenLetters
      putStrLn ""
      loop
  | length target /= length guess = do
      putStrLn "Wrong input"
      putStrLn ""
      loop
  | guess `notElem` possibleWords = do
      putStrLn "Word not in the possible words"
      putStrLn ""
      loop
  | guess `elem` triedWords = do
      putStrLn "You have already tried that word"
      putStrLn ""
      loop
  | wordContainsAlreadyKnownGreyLetter guess greyLetters = do
      putStrLn "Word contains letters that are already known not to be in the answer"
      putStrLn ""
      loop
  | not (wordContainsAllGreen guess greenLetters) = do
      putStrLn "Word doesn't contain one or more letters whose position is known"
      putStrLn ""
      loop
  | otherwise = do
      putStrLn $ checkGuess target guess
      putStrLn ""
      if target == guess || attempts - 1 == 0 then
        putStrLn $ "The word was: " ++ target
      else 
        gameLoopEasy target (attempts - 1) possibleWords (guess : triedWords) (getGreyLetters guess greyLetters target) (getGreenLetters guess target)
  where loop =  gameLoopEasy target attempts possibleWords triedWords greyLetters greenLetters

gameLoopNormal :: String -> Int -> IO ()
gameLoopNormal target attempts = do
  putStrLn $ "Attempts left: " ++ show attempts 
  putStrLn "Enter your guess:"
  guess <- getLine
  let feedback = checkGuess target guess
  if length target /= length guess then do
    putStrLn "wrong input"
    putStrLn ""
    gameLoopNormal target attempts
    else do
    putStrLn feedback
    putStrLn ""
    if target == guess || attempts == 1
     then putStrLn $ "The word was: " ++ target
      else gameLoopNormal target (attempts - 1)

gameLoopExpert :: String -> Integer -> [String] -> Bool -> IO ()
gameLoopExpert target attempts potentialWords yetToLie = do 
  putStrLn $ "Attempts left: " ++ show attempts
  putStrLn "Enter your guess:"
  guess <- getLine
  randomNumber <- randomRIO (1, 3) :: IO Int
  if yetToLie && randomNumber == 2
    then do
      word <- getRandomWord potentialWords
      let feedback = checkGuess word guess
      putStrLn feedback
      if target == guess || attempts == 0
          then correctWord
      else continueLoop (removeBadWords target guess potentialWords) False
    else do
      let feedback = checkGuess target guess
      putStrLn feedback
      if target == guess || attempts == 0
          then correctWord
      else  continueLoop (removeBadWords target guess potentialWords) True
      where continueLoop = gameLoopExpert target (attempts - 1)
            correctWord = putStrLn $ "The word was: " ++ target

gameLoopProgram :: [String] -> String -> Bool -> IO ()
gameLoopProgram words target manualPlay
  | optimalWord == target  = putStrLn $ "The word " ++ optimalWord ++ " is the correct word"
  | manualPlay = do
    putStrLn ""
    putColors
    putStrLn "Type Colors"
    answer <- getLine
    if correctColors == answer then
     continueLoop
    else
     (do putStrLn "You have entered wrong colors"
         gameLoopProgram words target manualPlay)
  | otherwise = do
    putColors
    putStrLn (checkGuess target optimalWord)
    putStrLn ""
    continueLoop
  where
    optimalWord = optimalWords words
    correctColors = zipWithColorSingle target optimalWord target
    continueLoop = gameLoopProgram (delete optimalWord (removeBadWords target optimalWord words)) target manualPlay
    putColors = putStrLn ("The word \"" ++ optimalWord ++ "\" was played")

main :: IO ()
main = do
  wordList <- readFile "words.txt"
  putStrLn "Welcome to Wordle!"
  putStrLn "Choose game mode"
  putStrLn "1. Normal - No additional help"
  putStrLn "2. Assisted - additional help"
  putStrLn "3. Expert - The program can lie once"
  putStrLn "4. Helper - choose the secret word and let the program guess it"
  gamemode <- getLine
  putStrLn "Word length:"
  lengthWord <- getLine
  targetWord <- getRandomWordWithLength (read lengthWord)
  let onlyWordsWithSetLength = filter (\word -> length word == read lengthWord) (lines wordList)
  if gamemode == "1" then do
    gameLoopNormal targetWord 6
  else if gamemode == "2" then do
    gameLoopEasy targetWord 6 onlyWordsWithSetLength [] [] ['_' | _ <- [1..read lengthWord]]
  else if gamemode == "3" then do
    gameLoopExpert targetWord 6 (filter (\word -> length word == read lengthWord) (lines wordList)) True
  else if gamemode == "4" then do
    putStrLn "secret word:"
    secretWord <- getLine
    putStrLn "Automatic play? (y/n)"
    play<- getLine
    if play == "y" then gameLoopProgram onlyWordsWithSetLength secretWord False 
    else gameLoopProgram onlyWordsWithSetLength secretWord True 
  else
    error "Invalid game mode"