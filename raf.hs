--
-- "Raf's Problem". See http://blog.tmorris.net/rafs-problem/
-- 

-- Matt Hellige's original broken solution
raf :: String -> Int
raf = foldr comb 0
  where
    comb x n = (fromEnum x - start) + (n * 26)
    start = fromEnum 'a' - 1

-- some quickcheck properties
prop_1 = (raf "a") == 1
prop_2 = (raf "b") == 2
prop_3 = (raf "z") == 26
prop_4 = (raf "aa") == 27
prop_5 = (raf "az") == 52
prop_6 = (raf "ba") == 53
prop_7 = (raf "bz") == 78
prop_8 = (raf "aaa") == 703
prop_9 = (raf "aaz") == 728
prop_10 = (raf "aza") == 1353
