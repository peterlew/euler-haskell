
import qualified Data.Map as M

data Status = LOOKING | LEADING | LOOPING 
 deriving (Show)

upperN :: Integer
upperN = 10^6 - 1

facDig :: Char -> Integer
facDig '0' = 1
facDig '1' = 1
facDig '2' = 2
facDig '3' = 6
facDig '4' = 24
facDig '5' = 120
facDig '6' = 720
facDig '7' = 5040
facDig '8' = 40320
facDig '9' = 362880

chainStp :: Integer -> Integer
chainStp n = foldl (+) 0 (map facDig (show n))

markLoop :: Integer -> Int -> M.Map Integer (Status, Int) -> M.Map Integer (Status, Int)
markLoop k v m = case M.lookup k m of 
                  Just (LOOPING, _) -> m
                  otherwise -> markLoop (chainStp k) v (M.insert k (LOOPING, v) m)

lookToLead :: Integer -> M.Map Integer (Status, Int) -> M.Map Integer (Status, Int)
lookToLead n mp = case M.lookup n mp of 
                   Just (LOOKING, _) -> let nxtN = chainStp n
                                            nxtMp = lookToLead nxtN mp
                                        in M.insert n (LEADING, maybe 0 snd (M.lookup nxtN nxtMp) + 1) nxtMp 
                   _ -> mp

evalChainN :: Integer -> M.Map Integer (Status, Int) -> M.Map Integer (Status, Int)
evalChainN n' mp = 
    let ecn n c m = case M.lookup n m of
                     Just (LOOKING, k) -> markLoop n (c - k) m
                     Just _ -> m
                     Nothing -> ecn (chainStp n) (c + 1) (M.insert n (LOOKING, c) m)
    in lookToLead n' (ecn n' 0 mp)

allChains :: M.Map Integer (Status, Int)
allChains = foldr evalChainN M.empty [1..upperN]

p74 :: Int
p74 = M.size $ M.filter f allChains
 where f (_,v) = v == 60