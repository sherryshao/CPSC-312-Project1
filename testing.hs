generateStr1 :: String -> String -> Int -> String
generateStr1 str1 str2 index
	| canMove str1 str2 index	= replaceListElem str1 index '-'
	| otherwise 			= str1
	
canMove :: String -> Int -> Bool
canMove str1 str2 index
	| abs (length str1 - length str2) /= 1		= False
	| (str1 > str2) && (str2 !! )
	
isEmpty :: Char -> Bool
isEmpty block =		block == '-'
	
replaceListElem :: [a] -> Int -> a -> [a]
replaceListElem list index elem
	| null list		= []
	| index == 0	= elem : (tail list)
	| otherwise		= (head list) : replaceListElem (tail list) (index - 1) elem								
