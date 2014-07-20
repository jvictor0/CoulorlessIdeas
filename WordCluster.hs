module WordCluster where

import ColourlessIdeas

import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Data.Maybe

readBaskets file = do
  f <- readFile file
  g <- readFile "quotes2.txt"
  i <- ideas
  putStrLn $ i`deepseq`"Gen'd Ideas"
  return $ filter (not . null) $
    map (lineToBasket i) $ (lines g) ++ (map unwords $ groupBy (\a b -> null b || head b /= '\"') $ lines f)

realChar '-' = True
realChar '\'' = True
realChar x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')

lineToBasket ideas str = nub $ concatMap (normalize ideas) $ words $ map (\x -> if realChar x then toLower x else ' ') str

normalize ideas str = case Map.lookup str ideas of
  Nothing -> []
  (Just ideas) -> case find (\(w,tp) -> "t" == tail tp) ideas of
    (Just (w,tp)) -> [w]
    Nothing -> [str]
  
pairs [] = []
pairs (a:as') = let as = filter (/=a) as' in (concatMap (\b -> [(a,b),(b,a)]) as) ++ (pairs as)

pairsHisto :: [[String]] -> Map.Map (String,String) Int
pairsHisto baskets = foldl' (\mp p -> p`deepseq`(Map.insertWith (+) p 1 mp)) Map.empty $ concatMap pairs baskets

itemsHisto :: [[String]] -> Map.Map String Int
itemsHisto baskets = foldl' (\mp p -> p`deepseq`(Map.insertWith (+) p 1 mp)) Map.empty $ concat baskets

x//y = (fromIntegral x)/(fromIntegral y)

interestMap :: Double -> Map.Map (String,String) Int -> Map.Map String Int -> Map.Map (String,String) Double
interestMap beta mp itemfreq = Map.mapWithKey (\(a,b) i -> (fromIntegral i)/
                                                           (beta + (1-beta)*(fromIntegral $fromJust $ Map.lookup a itemfreq))) 
                               mp

generateInterestMap = do
  bs <- readBaskets "tweets.csv"
  let (ps,is) = (pairsHisto bs,itemsHisto bs)
  print $ Map.size is
  return $ interestMap 0.0 ps is
  
generateCoOccuranceDict :: IO [(String,[(String,Int)])]
generateCoOccuranceDict = do
  mp <- generateInterestMap
  putStrLn $ mp`deepseq`"Interest Map found"
  let alpha = 5.0
  return 
    $ filter (not . null . snd)
    $ map (\l -> (fst $ fst $ head l, 
                  filter (\(_,a) -> a>0) $ map (\((_,a),r) -> (a,floor $ r*alpha)) l))
    $ groupBy (\((a,_),_) ((b,_),_) -> a == b) $ Map.toList mp
    
saveCoOcc = do
  f <- generateCoOccuranceDict
  writeFile "CoOcc.txt" $ "{" ++ (concat $ intersperse "," $ map 
                                  (\(a,b) -> (show a) ++ ":[" ++ (concat $ intersperse "," $ map 
                                                                  (\(a,b) -> "[" ++ (show a) ++"," ++ (show b) ++"]") b) ++ "]") f)
    ++ "}"