import Data.Map (Map)
import qualified Data.Map as Map

import Data.List 
import qualified Data.List as List

type Bigram = (String, String)
type Trigram = (String, String, String)

type WordOccurence = Map String Integer
type NextWordWordOccurence = Map Bigram WordOccurence

main = do
		wordList <- getWords "jeeves.txt"
		let map_ = trigramOccurences wordList
		let nextWordOccurences = makeNextWordOccurences map_
		let maxOccurence = get_max_val_from_map map_
		let lst = Map.toList map_
		let bigram = ("this", "is")
		print (Map.lookup bigram nextWordOccurences)
		print maxOccurence

makeNextWordOccurences :: Map Trigram Integer -> NextWordWordOccurence
makeNextWordOccurences m = foldl (Map.unionWithKey nextWordOccurenceUnion) Map.empty (map makeNextWordOccurence (Map.toList m))

makeNextWordOccurence :: (Trigram, Integer) -> NextWordWordOccurence
makeNextWordOccurence (t, k) = Map.fromList [((firstTwo t), Map.fromList[(last_ t, k)])]

nextWordOccurenceUnion :: Bigram -> WordOccurence -> WordOccurence -> WordOccurence
nextWordOccurenceUnion bigram a b = Map.union a b


last_ (t1,t2,t3) = t3 

firstTwo :: Trigram -> Bigram
firstTwo (t1, t2, t3) = (t1, t2)  



normalizeMap :: Fractional a => Map Trigram Integer -> Integer -> Map Trigram Double
normalizeMap m k = Map.fromList (map (normalizeTrigram k) $ Map.toList m)

normalizeTrigram :: Integer -> (Trigram, Integer) -> (Trigram, Double)
normalizeTrigram k ((k1,k2,k3), v) = ((k1,k2,k3), (fromInteger v) / (fromInteger k ))

getWords :: FilePath -> IO [String]
getWords filePath = do 
					contents <- readFile filePath
					return (words contents)

bigramOccurences :: [String] -> Map Bigram Integer
bigramOccurences ss = getBigramOccurences ss Map.empty

trigramOccurences :: [String] -> Map Trigram Integer
trigramOccurences ss = getTrigramOccurences ss Map.empty

getBigramOccurences :: [String] -> Map Bigram Integer  -> Map Bigram Integer
getBigramOccurences [] m = m
getBigramOccurences [a, b] m = m
getBigramOccurences xs m = getBigramOccurences (drop 3 xs) (Map.insertWith (+) (headTuple xs) 1 m)

getTrigramOccurences :: [String] -> Map Trigram Integer  -> Map Trigram Integer
getTrigramOccurences [] m = m
getTrigramOccurences [a, b] m = m
getTrigramOccurences xs m = getTrigramOccurences (drop 3 xs) (Map.insertWith (+) (headTriple xs) 1 m)

headTuple :: [String] -> Bigram
headTuple xs = (xs !! 0, xs !! 1) 

headTriple :: [String] -> Trigram
headTriple xs = (xs !! 0, xs !! 1, xs !! 2) 

get_max_val_from_map m = (map snd $ filter is_biggest sorted) !! 0
    where sorted = List.sortBy (\(k1, v1) (k2, v2) -> v2 `compare` v1) $ Map.toList m
          max_v = snd $ head sorted
          is_biggest (key, value) = value == max_v