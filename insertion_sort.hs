insertion_sort :: (Ord a) => [a] -> [a]
insertion_sort [] = []
insertion_sort (x:xs) =
	insert x (insertion_sort xs)
	
insert x [] = [x]
insert x (y:ys) 
	| x <= y = (x:y:ys)
	| otherwise = y:(insert x ys)
