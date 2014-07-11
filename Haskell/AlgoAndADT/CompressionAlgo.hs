import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map

main = print output

output =  (==source) $map chr $lzwDecode $lzwEncode  $ map ord source 
    where source = "aaaaa"

------------------------------
-- binary stream encoding

bitsPerInt = 32

bitsEncode bits = length bits:encode bits
    where encode [] = []
          encode l = (foldl (\v b->v*2+b) 0 $reverse left):encode right
            where (left,right) = (take bitsPerInt l, drop bitsPerInt l)

bitsDecode (len:ints) = take len . concat . map (int2Bits 0) $ ints
    where int2Bits w 0 = replicate (bitsPerInt - w) 0
          int2Bits w x = (x `mod` 2):int2Bits (w + 1) (x `div` 2)

------------------------------
-- huffman encoding

data HuffNode a = HuffLeaf a | HuffBranch (HuffNode a) (HuffNode a) deriving(Show)

huffmanEncode l = (bitsEncode bits, huffTree)
    where newl = addHead l
          huffTree = buildHuffTree $ sortBy (compare `on` fst) $ map (\(x:xs)->(1+length xs,HuffLeaf x)) $ group $ sort newl
          charMap = Map.fromList $ huffTree2AssocList huffTree []
          bits = concat $map (\c-> charMap Map.! c) newl
          addHead l@(x:xs) | x == maxBound = pred x:l | otherwise = succ x:l
          buildHuffTree [(f1,n1)] = n1
          buildHuffTree ((f1,n1):(f2,n2):rest) = buildHuffTree (insertBy (compare `on` fst) (f1+f2,HuffBranch n1 n2) rest)
          huffTree2AssocList (HuffLeaf c) bits = [(c,reverse bits)]
          huffTree2AssocList (HuffBranch n1 n2) bits = huffTree2AssocList n1 (0:bits) ++ huffTree2AssocList n2 (1:bits)

huffmanDecode (encodedBits, huffTree) = drop 1 $ decode huffTree $ bitsDecode encodedBits
    where decode (HuffLeaf c) bits = c:decode huffTree bits
          decode node [] = []
          decode (HuffBranch n1 n2) (0:bits) = decode n1 bits
          decode (HuffBranch n1 n2) (1:bits) = decode n2 bits

------------------------------
-- lzw encoding

lzwEncode l = encode l [] $ Map.fromList $ zip [[x]|x<-[0..255]] [0..255]
    where encode [] [] table = []
          encode [] curr table = [table Map.! curr]
          encode (x:xs) curr table 
            | Map.member (x:curr) table = encode xs (x:curr) table
            | otherwise = (table Map.! curr):encode xs [x] (Map.insert (x:curr) (Map.size table) table)

lzwDecode tokens = concat $decode tokens $ Map.fromList $zip [0..255] [[x]|x<-[0..255]]
    where decode (id:[]) table = [table Map.! id]
          decode (id1:id2:rest) table = curr:decode (id2:rest) newTable
            where curr = table Map.! id1
                  newTable = Map.insert (Map.size table) (curr ++ [(Map.findWithDefault curr id2 table) !! 0]) table
