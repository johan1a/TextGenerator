import Data.Map (Map)
import qualified Data.Map as Map

import Data.List 
import qualified Data.List as List

import System.Random

type Bigram = (String, String)
type Trigram = (String, String, String)

type WordFrequencyMap = Map String Integer
type SuffixFrequencyMap = Map Bigram WordFrequencyMap

main = do
		wordList <- getWords "jeeves.txt"
		let suffixMap = suffixFrequencies wordList
		inputLoop suffixMap


inputLoop suffixMap = do
						let wordCount = 40
						--line <- getLine
						--unless (wordCount == "q") $ do
						text <- generateText suffixMap 40
						print(text)

generateText :: SuffixFrequencyMap -> Int -> IO String
generateText suffixMap wordCount = generateText_ suffixMap wordCount ("I", "have")

generateText_ :: SuffixFrequencyMap -> Int -> Bigram -> IO String
generateText_ suffixMap 0 bigram = return (fst bigram ++ snd bigram)
generateText_ suffixMap wordsLeft bigram = do
	nextWord <- randomSuffix (Map.findWithDefault Map.empty bigram suffixMap)
	text <- generateText_ suffixMap (wordsLeft - 1) (snd bigram, nextWord)
	return $ fst bigram ++ " " ++ text
	

randomSuffix :: WordFrequencyMap -> IO String
randomSuffix wordFrequencies = pick (Map.keys wordFrequencies)

mostProbableSuffix :: WordFrequencyMap -> String
mostProbableSuffix wordFrequencies = keyWithMaxValFromMap wordFrequencies

getWords :: FilePath -> IO [String]
getWords filePath = do 
					contents <- readFile filePath
					return (words contents)

suffixFrequencies :: [String] -> SuffixFrequencyMap
suffixFrequencies xs = suffixFrequencies_ xs Map.empty

suffixFrequencies_ :: [String] -> SuffixFrequencyMap -> SuffixFrequencyMap
suffixFrequencies_ [] m = m
suffixFrequencies_ [a, b] m = m
suffixFrequencies_ (x:xs) m = mergeSFM m (suffixFrequencies_ xs (singleSuffix (x:xs)))

singleSuffix :: [String] -> SuffixFrequencyMap
singleSuffix xs = Map.fromList [((headTuple xs), makeWFM (xs !! 2))]

makeWFM :: String -> WordFrequencyMap
makeWFM s = Map.fromList[(s, 1)]

mergeSFM :: SuffixFrequencyMap -> SuffixFrequencyMap -> SuffixFrequencyMap
mergeSFM a b = Map.unionWith mergeWFM a b

mergeWFM :: WordFrequencyMap -> WordFrequencyMap -> WordFrequencyMap
mergeWFM a b = Map.unionWith (+) a b

headTuple :: [String] -> Bigram
headTuple xs = (xs !! 0, xs !! 1) 



pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

keyWithMaxValFromMap :: WordFrequencyMap -> String
keyWithMaxValFromMap m = (map fst $ filter is_biggest sorted) !! 0
    where sorted = List.sortBy (\(k1, v1) (k2, v2) -> v2 `compare` v1) $ Map.toList m
          max_v = snd $ head sorted
          is_biggest (key, value) = value == max_v

normalizeMap :: Fractional a => Map Trigram Integer -> Integer -> Map Trigram Double
normalizeMap m k = Map.fromList (map (normalizeTrigram k) $ Map.toList m)

normalizeTrigram :: Integer -> (Trigram, Integer) -> (Trigram, Double)
normalizeTrigram k ((k1,k2,k3), v) = ((k1,k2,k3), (fromInteger v) / (fromInteger k ))
