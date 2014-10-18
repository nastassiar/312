 -- myremoveduplicates
 --
 -- This function takes a list and returns a new list containing the
 -- same elements as the original list, but only 1 of each element
 -- This algorithm has a time complexity of O(elem * n) where 'n' is the
 -- length of the list.
 --
 -- Arguments:
 -- -- lst: the input list
 --
 -- Returns: the new list
 --

myremoveduplicates :: Eq a => [a] -> [a]
myremoveduplicates lst
	| null lst 						= lst
	| elem (head lst) (tail lst)	= myremoveduplicates (tail lst)
	| otherwise						= (head lst) : myremoveduplicates (tail lst)


myremoveduplicates_pm :: Eq a => [a] -> [a]
myremoveduplicates_pm []	 		= []
myremoveduplicates_pm (x:xs) 		
	= case (elem x xs) of
		True -> myremoveduplicates_pm (xs)
		False -> x:(myremoveduplicates_pm xs)


-- Problem 2
-- myintersection
 --
 -- This function takes a list and returns a new list that only contain the
 -- elements that appear in both lists.
 -- This algorithm has a time complexity of O(elem*n) where 'n' is the
 -- length of the list.
 --
 -- Arguments:
 -- -- lst1: the first input list
 -- -- lst2: the second input list
 --
 -- Returns: the new list
 --

myintersection :: Eq a => [a] -> [a] -> [a]
myintersection lst1 lst2
	| null lst1 				    = []
	| elem (head lst1) lst2	        = (head lst1) : (myintersection (tail lst1) lst2)
	| otherwise						= myintersection (tail lst1) lst2


myintersection_pm :: Eq a => [a] -> [a] -> [a]
myintersection_pm [] lst2			= []
myintersection_pm (x:xs) lst2
	= case (elem x lst2) of 
		True -> x : (myintersection_pm xs lst2)
		False -> myintersection_pm xs lst2 

-- Problem 3
-- mynthtail
 --
 -- This function takes a list and a number.  It returns a list that is the tail of the 
 -- original list, starting at the position of the given number
 -- The algorithm has a time complexity of O(n) where n is the length of the list
 --
 -- Arguments:
 -- -- lst1: the first input list
 -- -- ln: position number
 --
 -- Returns: the new list
 --

mynthtail :: Int -> [a]-> [a]
mynthtail n lst
	| n == 0 			= lst
	| otherwise 		= mynthtail (n-1) (tail lst)

mynthtail_pm :: Int -> [a] -> [a]
mynthtail_pm 0 lst 		= lst
mynthtail_pm n (x:xs) 	= mynthtail_pm (n-1) xs

-- Problem 4

-- mylast
 --
 -- This function takes a list.  It returns a list containing only 
 -- the last element of the original list.
 -- The algorithm has a time complexity of O(n) where n is the length of the list
 --
 -- Arguments:
 -- -- lst: the list
 --
 -- Returns: the new list
 --
mylast :: [a] -> [a]
mylast lst 
	| null lst 			= lst
 	| null (tail lst)	= lst
 	| otherwise 		= mylast (tail lst)

mylast_pm :: [a] -> [a]
mylast_pm []			= []
mylast_pm (x:xs)
	= case (null xs) of 
		True -> (x:xs)
		False -> mylast_pm xs

-- Problem 5

-- myreverse
 --
 -- This function takes a list.  It returns a new list with the elements
 -- of the original list, reversed. 
 -- This function runs in O(n) 
 --
 -- Arguments:
 -- -- lst: the list
 --
 -- Returns: the new, reversed list
 --

myreverse :: [a] -> [a]
myreverse lst
	| null lst 		= lst
	| otherwise		= myreverse_helper lst []

myreverse_helper :: [a] -> [a] -> [a]
myreverse_helper lst1 lst2 
	| null lst1 	= lst2
	| otherwise 	= myreverse_helper (tail lst1) ((head lst1):lst2)

myreverse_pm :: [a] -> [a]
myreverse_pm [] 	= []
myreverse_pm lst 	= myreverse_helper_pm lst []

myreverse_helper_pm :: [a] -> [a] -> [a]
myreverse_helper_pm [] (y:ys) 			= (y:ys)
myreverse_helper_pm (x:xs) []			= myreverse_helper_pm xs (x:[])
myreverse_helper_pm (x:xs) (y:ys)		= myreverse_helper_pm xs (x:(y:ys))



-- Problem 6
-- myreplaceall
 --
 -- This function takes 2 numbers and a list  It returns a list such that 
 -- all the instances of the 2nd number are replaced by the 1st number
 -- The algorithm has a time complexity of O(n) where n is the length of the list
 --
 -- Arguments:
 -- -- replace: the number that will replace the orginial
 -- -- original: the original numbers in the list
 -- -- lst: the list
 --
 -- Returns: the new list
 --

myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall replace original lst
	| null lst 					= lst
	| (head lst) == original	= replace:(myreplaceall replace original (tail lst))
	| otherwise					= (head lst):(myreplaceall replace original (tail lst))

myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
myreplaceall_pm _ _ [] 			= []
myreplaceall_pm n m (x:xs)
	= case (x == m) of
		True -> n:(myreplaceall_pm n m xs)
		False -> x:(myreplaceall_pm n m xs)


-- Problem 7
-- myordered
 --
 -- This function takes in a list.  It returns a boolean indicating 
 -- whether or not the list is ordered from lowest to highest.
 -- The algorithm has a time complexity of O(n) where n is the length of the list
 --
 -- Arguments:
 -- -- lst: the list
 --
 -- Returns: True if the list is ordered lowest to highest and False otherwise.
 --

myordered :: Ord a => [a] -> Bool 
myordered lst 
	| null lst 							= True
	| null (tail lst)					= True
	| (head lst) > (head (tail lst))	= False 	
	| otherwise						 	= myordered (tail lst)	

myordered_pm :: Ord a => [a] -> Bool
myordered_pm [] 		= True
myordered_pm [_]		= True
myordered_pm (x:xs)
	= case ( x > (head xs)) of 
		True -> False
		False -> myordered_pm (xs)
