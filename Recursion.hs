
drop'            :: Int -> [a] -> [a]
drop' 0  xs      = xs
drop' n  []      = []
drop' n  (_:xs)  = drop (n-1) xs   

append'             :: [a] -> [a] -> [a]
append' []  ys      = ys
append' (x:xs) ys   = x : append' xs ys

(+++)             :: [a] -> [a] -> [a]
[]  +++ ys      = ys
(x:xs) +++ ys   = x : (xs +++ ys)
