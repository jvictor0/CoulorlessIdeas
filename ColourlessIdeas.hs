module ColourlessIdeas where

import Data.List
import Data.Char
import qualified Data.Map as Map

data CapLet = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
            deriving (Show, Eq, Enum, Read)

a = 'a' 
b = 'b' 
c = 'c' 
d = 'd' 
e = 'e' 
f = 'f' 
g = 'g' 
h = 'h' 
i = 'i' 
j = 'j' 
k = 'k' 
l = 'l' 
m = 'm' 
n = 'n' 
o = 'o' 
p = 'p' 
q = 'q' 
r = 'r' 
s = 's' 
t = 't' 
u = 'u' 
v = 'v'  
w = 'w' 
x = 'x' 
y = 'y' 
z = 'z'

type DictEntry = (String,CapLet,Char,Char)

crop x = reverse $ dropWhile isSpace $ reverse x

splitComma (',':rst) = []:(splitComma rst)
splitComma (a:rst) = let (r:res) = (splitComma rst) in (a:r):res
splitComma [] = [[]]

readDE :: [String] -> String -> [DictEntry]
readDE sw str 
  | (map toLower str)`elem`sw = [] 
  | otherwise = map (\pos -> (crop $ take 23 str, 
                              read [head pos], 
                              pos!!1, pos!!2))
                poses
  where poses = splitComma $ take 23 $ drop 46 str
        
readDict :: IO [DictEntry]
readDict = do
  sw <- fmap lines $ readFile "stopwords"
  f <- fmap lines $ readFile "dict.txt"
  return 
    $ concatMap (\poses -> filter (\(_,_,_,r) -> r == (maximum $ map (\(_,_,_,r) -> r) poses)) poses)
    $ groupBy (\(a,_,_,_) (b,_,_,_) -> a == b)
    $ concatMap (readDE sw) f
  
char x = head $ show x
  
realAdverb x = "yl" == (take 2 $ reverse x)
         
entries (word,M,a,b) = (entries (word,K,a,b)) ++ (entries (word,L,a,b))
entries (word,J,a,b) = (entries (word,H,a,b)) ++ (entries (word,I,a,b))
entries (word,nn,'0',_) = [(word,[char nn,t]),(word++"s",[char nn,f,a]),(word++"ing",[char nn,f,b]),(word++"ed",[char nn,f,c])]
entries (word,nn,'1',_) = [(word,[char nn,t]),(word++"es",[char nn,f,a]),(word++"ing",[char nn,f,b]),(word++"ed",[char nn,f,c])]
entries (word,nn,'2',_) = [(word,[char nn,t]),(word++"s",[char nn,f,a]),((init word)++"ing",[char nn,f,b]),(word++"d",[char nn,f,c])]
entries (word,nn,'3',_) = [(word,[char nn,t]),((init word)++"ies",[char nn,f,a]),((init word)++"ying",[char nn,f,b]),((init word)++"ied",[char nn,f,c])]
entries (word,nn,'4',_) = [(word,[char nn,t]),(word++"s",[char nn,f,a]),(word++[last word]++"ing",[char nn,f,b]),(word++[last word]++"ed",[char nn,f,c])]
entries (word,nn,'5',_) = [(word,[char nn,t])]
entries (word,nn,'6',_) = [(word,[char nn,t]),(word++"s",[char nn,f])]
entries (word,nn,'7',_) = [(word,[char nn,t]),(word++"es",[char nn,f])]
entries (word,nn,'8',_) = [(word,[char nn,t]),((init word)++"ies",[char nn,f])]
entries (word,nn,'9',_) = [(word,[char nn,t]),(word,[char nn,f])]
entries (word,nn,'@',_) = [(word,[char nn,t])]
entries (word,nn,'A',_) = [(word,[char nn,t])]
entries (word,nn,'B',_) = [(word,[char nn,t]),(word++"r",[char nn,f,r]),(word++"st",[char nn,f,s])]
entries (word,nn,'C',_) = [(word,[char nn,t]),(word++"er",[char nn,f,r]),(word++"est",[char nn,f,s])]
entries (word,nn,'D',_) = [(word,[char nn,t]),((init word)++"ier",[char nn,f,r]),((init word)++"est",[char nn,f,s])]
entries (word,nn,'E',_) = [(word,[char nn,t])]
entries (word,nn,nc,_) 
  | nc`elem`"abcdrs"      = [(word,[char nn,f,nc])]
entries (word,nn,nc,_) 
  | nc`elem`"efghpqt"      = []
entries (word,nn,'i',_) = [(word,[char nn,t])]
entries (word,nn,'j',_) = [(word,[char nn,f])]
entries (word,nn,'k',_) = [(word,[char nn,t])]
entries (word,N,lmno,_) = [(word,[char N,lmno])]
entries (word,P,tp,_) 
  | realAdverb word = [(word,[char P,tp])]
entries _ = []


insertIdeas :: Map.Map String [(String,String)] -> DictEntry -> Map.Map String [(String,String)]
insertIdeas mp de = let es = entries de
                    in foldl' (\mp (w,_) -> Map.insertWith (++) w es mp) mp es

ideas = fmap (fmap nub . foldl' insertIdeas Map.empty) readDict

ideasString = do
  is <- ideas
  sw <- fmap lines $ readFile "stopwords"
  let stris = map (\(w,l) -> (show $ map toLower w) ++ " : " ++ (show $ map (\(x,y) -> [x,"(" ++ y ++ ")"]) 
                                                   $ filter ((`notElem`sw).(map toLower).fst) l)) 
              $ Map.toList is
  return $ "{" ++ (concat $ intersperse "," stris )++ "}" 
  
saveIdeas = do
  str <- ideasString
  writeFile "ideas_json" str