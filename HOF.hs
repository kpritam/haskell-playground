
foldR               :: (a -> b -> b) -> b -> [a] -> b
foldR f v []        = v
foldR f v (x:xs)    = f x (foldR f v xs)

length'  :: [a] -> Int
length'  = foldR (\_ n -> n + 1) 0

reverse' :: [a] -> [a]
reverse' = foldR (\x xs -> xs ++ [x]) []