module Compiler (compile) where

import Data.IntMap.Strict as M
import Data.Maybe
import Data.List
import Data.Ord

import Parser (AbstractSyntaxList)

{-
Tree with two way relationships defined by gcd(a,b) != 1 (assuming a!=1 and b!=1)

For the use case of fizzbuzz it's only required to store the greatest parent
-}
data GcdTree = GcdNode { inner :: Int, children :: [GcdTree]}

instance Show GcdTree where
  show (GcdNode i c) = "{ inner " ++ show i ++ ", children" ++ show c ++ "}"

constructGcdTree :: [Int] -> M.IntMap GcdTree
constructGcdTree [] = mempty
constructGcdTree (n:ns) = let
  existing = constructGcdTree ns
  parent = find (\e -> mod n e == 0 && e /= n) ns
  in
  if isNothing parent then
    M.insert n (GcdNode n []) existing
  else let
    GcdNode pInner pChildren = (M.!) existing (fromJust parent)
    self = GcdNode n []
  in
    M.insert pInner (GcdNode pInner (self : pChildren)) existing

{-
Takes an abstract syntax list and returns a string of AT&T x86_64 Linux assembly

(Currently using C as it's easier to begin with)
-}
compile :: AbstractSyntaxList -> String
compile asl = let
  trees = constructGcdTree $ sortBy (comparing Down) $ fmap fst asl
  phrases = fromListWith (flip (++)) asl
  in treeToC phrases trees
  
treeToC :: M.IntMap String -> M.IntMap GcdTree -> String
treeToC phrases trees = let ifBlock = ifBlockC phrases 1 in
  "#include <stdio.h>\nvoid process(int n) { for (int i1=1; i1<=n; i1++){ int flag = 0;" ++
  ((concat $ fmap ifBlock $ elems trees) :: String)
  ++ "if (!flag) printf(\"%d\", i1);\nputchar('\\n');}}\n"
  ++ "int main() {\nint n = 0;\n"
  ++"scanf(\"%d\", &n); if (n == 0) return 1; process(n); return 0;}\n"

ifBlockC :: M.IntMap String -> Int -> GcdTree -> String
ifBlockC phrases parentI (GcdNode i c) = let
    showi = show i
    parentN = show parentI
    iOnParent = show $ i `div` parentI
  in
    "if (!(i" ++ parentN ++" % " ++ iOnParent ++ ")) {\nflag=1;\nint i" ++ showi
    ++ "= i" ++ parentN ++ "/ " ++ iOnParent ++ ";\nfputs("
    ++ (show . fromJust $ M.lookup i phrases) ++ ", stdout);\n"
    ++ ((concat $ fmap (ifBlockC phrases i) c) :: String)
    ++ "}\n"
