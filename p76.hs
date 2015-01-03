
type Partition = [Int]

partitionsToUnder :: Int -> Int -> [Partition]
partitionsToUnder 0 _ = [[]] 
partitionsToUnder n 1 = [replicate n 1]
partitionsToUnder n cap | n < cap = partitionsToUnder n n
partitionsToUnder n cap = foldl (++) [] (map f [1..cap])
 where f x = map (x:) (partitionsToUnder (n - x) x)

partitions :: Int -> [Partition]
partitions n = partitionsToUnder n (n - 1)