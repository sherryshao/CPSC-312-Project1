-- 2013W1 CPSC 312 101
-- Project #1
-- Eric Chu (73265092) & Sherry Shao (60135084)

oska_x1y2 :: [String] -> Char -> Integer -> [String]
oska_x1y2 board us depth
	| us == 'w'		= generateBoard board us depth
	| otherwise		= generateBoard (flipBoard board) us depth

generateBoard :: [String] -> Char -> Integer -> [String]
generateBoard board us depth = generateBestBoard (generateNewStates board us depth)

generateBestBoard :: [([String], Integer)] -> [String]
generateBestBoard weightedBoard = generateBestBoard_tr weightedBoard [] 0

generateBestBoard_tr :: [([String], Integer)] -> [String] -> Integer -> [String]
generateBestBoard_tr weightedBoard bestBoard lastWeight
	| null weightedBoard		= bestBoard
	| currWeight >= lastWeight	= 
		generateBestBoard_tr (tail weightedBoard) currBoard currWeight
	| otherwise					=
		generateBestBoard_tr (tail weightedBoard) bestBoard lastWeight
	where
		currBoard = fst (head weightedBoard)
		currWeight = snd (head weightedBoard)

generateNewStates :: [String] -> Char -> Integer -> [([String], Integer)]
generateNewStates currBoard us depth = 
	concat [generateLeftMove currBoard depth,
			generateRightMove currBoard depth,
			generateLeftJump currBoard depth,
			generateRightJump currBoard depth]

generateLeftMove currBoard us depth = 
	columnizeBoard( generateNew columnizeBoard( currBoard ) us '-' depth )

generateRightMove currBoard us depth = 
	reverseBoard( columnizeBoard( generateNew columnizeBoard( reverseBoard currBoard ) us '-' depth ))

generateLeftJump currBoard us depth = 
	columnizeBoard( generateNew columnizeBoard( currBoard ) us (getOpponent us) depth )

generateRightJump currBoard us depth = 
	reverseBoard( columnizeBoard( generateNew columnizeBoard( reverseBoard currBoard ) us (getOpponent us) depth ))

-- generateNew : moves to the left by default
generateNew :: [String] -> Char -> Char -> Integer -> [([String], Integer)]
generateNew currBoard us adj depth = 
	generateNew_tr currBoard us adj depth 0

generateNew_tr :: [String] -> Char -> Char -> Integer -> Integer -> [([String], Integer)]
generateNew_tr currBoard us adj depth posY
	| posY >= (length currBoard) = []
	| elem us currCol			 = 
		generateNew_tr currBoard us adj depth (posY+1)
	| otherwise					 =
		(generateNewBoards currBoard us adj posY) ++
		(generateNew_tr currBoard us adj depth (posY+1))
	where
		currCol = colAtPos currBoard posY

-- generateNewColumn

generateNewColumn :: String -> Integer -> Char -> Char -> String
generateNewColumn currCol pos us adj
	| pos + (length oldSegment) > length currCol	= currCol
	| segmentEqual currCol pos oldSegment			=
		(generateNewRow (replaceSegment currCol pos newSegment) (pos + 1) us)
	| otherwise										= 
		(generateNewRow currCol (pos + 1) us)
	where 
		oldSegment = us:adj:[]
		newSegment = '-':us:[]

-- Helper Functions

-- getOpponent : Get opponent character given what we are

getOpponent :: Char -> Char
getOpponent us
	| us == 'w'	= 'b'
	| otherwise	= 'w'

-- colAtPos : Returns column at position on board

colAtPos :: [String] -> Integer -> String
colAtPos board posY
	| posY == 0	= head board
	| otherwise	= colAtPos (tail board) (posY-1)

-- replaceColAtPos : Returns new board / state after replacing one column

replaceColAtPos :: [String] -> String -> Integer -> Integer -> [String]
replaceColAtPos grid newCol pos start
	| null grid 				 	= []
	| pos == start					= 
		newCol:(replaceColAtPos (tail grid) newCol pos (start+1))
	| otherwise		 	 			= 
		(head grid):(replaceColAtPos (tail grid) newCol pos (start+1))

-- segmentEqual : (snippet from lecture slides) check current column with segment to be replaced

segmentEqual :: String -> Integer -> String -> Bool
segmentEqual oldCol pos oldSegment = 
   (oldSegment == take (length oldSegment) (drop pos oldCol))

-- replaceSegment : (snippet from lecture slides) replace segment in list

replaceSegment :: String -> Integer -> String -> String
replaceSegment oldCol pos segment
   | pos == 0  = segment ++ drop (length segment) oldCol
   | otherwise = 
        (head oldCol):
        (replaceSegment (tail oldCol) (pos - 1) segment)

-- flipBoard : change player side (our player is on the side of the first row)
flipBoard :: [String] -> [String]
flipBoard board = reverse (map reverse board)

-- reverseBoard : change diretion of movements (i.e. left vs right)
reverseBoard :: [String] -> [String]
reverseBoard board = map reverse board

-- columnizeBoard : standardize board to be able to move forward (deault towards the left)
columnizeBoard :: [String] -> [String]
columnizeBoard board = board

-- ERIC'S CODE STARTS HERE

generateStr1 :: String -> String -> Int -> String
generateStr1 str1 str2 index	= replaceListElem str1 index '-'
	
generateStr2s :: String -> String -> Int -> [String]
generateStr2s str1 str2 index	= generateStr2s' str1 str2 index
	
generateStr2s' :: String -> String -> Int -> [String]
generateStr2s' str1 str2 index
	| length str1 > length str2		= generateStr2sLongShort (str1 !! index) str2 index
	| length str1 < length str2		= generateStr2sShortLong (str1 !! index) str2 index
	| otherwise						= [] 
	
generateStr2sLongShort :: Char -> String -> Int -> [String]
generateStr2sLongShort pawn str2 index
	| index == 0			= [makeMove str2 0 pawn]
	| index == length str2	= [makeMove str2 (index - 1) pawn]
	| otherwise				= (makeMove str2 (index) pawn) : [(makeMove str2 (index - 1) pawn)]

generateStr2sShortLong :: Char -> String -> Int -> [String]
generateStr2sShortLong pawn str2 index
	= (makeMove str2 (index) pawn) : [(makeMove str2 (index + 1) pawn)]
	
makeMove :: String -> Int -> Char -> String
makeMove str2 index pawn
	| canMove pawn (str2 !! index)	= replaceListElem str2 index pawn
	| otherwise						= []


-- utils
	
canMove :: Char -> Char -> Bool
canMove pawn1 pawn2		= (not (isEmpty pawn1)) && isEmpty pawn2

isEmpty :: Char -> Bool
isEmpty block =		block == '-'
	
replaceListElem :: [a] -> Int -> a -> [a]
replaceListElem list index elem
	| null list		= []
	| index == 0	= elem : (tail list)
	| otherwise		= (head list) : replaceListElem (tail list) (index - 1) elem		
	
getAllPawns :: [String] -> Char -> [(Int, Int)]
getAllPawns board player
	| null board	= []
	| otherwise		= getAllPawnsRow (head board) player : getAllPawns (tail) player
	
--getAllPawns :: String -> Char -> (Int, Int)
