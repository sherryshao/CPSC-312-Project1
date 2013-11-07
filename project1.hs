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



-- ERIC'S CODE STARTS HERE **** state generation working!
-- will add more comments
-- currently, calling generateNewStates given a board and player ('w' or 'b'), all states one step away from the current
-- state will be returned as a list. Score evaluation is current set to 0.
-- also, implementation assumes that 'b' pawns move start at the bottom and move upward while 'w' pawns start at the top
-- and move downward. 

generateNewStates :: [String] -> Char -> [([String], Int)]
generateNewStates currBoard player	
	| player == 'w'		= generateNewStates' currBoard player (getAllPawns currBoard player)
	| player == 'b'		= reverseNewStates (generateNewStates' (reverse currBoard) player (getAllPawns (reverse currBoard) player))
	| otherwise			= []

reverseNewStates :: [([String], Int)] -> [([String], Int)]
reverseNewStates states
	| null states 	= []
	| otherwise 	= (reverse (fst (head states)), snd (head states)) : reverseNewStates (tail states)

generateNewStates' :: [String] -> Char -> [(Int, Int)] -> [([String], Int)]
generateNewStates' currBoard player pawnlocs
	| null pawnlocs		= []
	| otherwise			= generateNewStatesPawn currBoard player (head pawnlocs) ++ generateNewStates' currBoard player (tail pawnlocs)

generateNewStatesPawn :: [String] -> Char -> (Int, Int) -> [([String], Int)]
generateNewStatesPawn currBoard player pawnloc 	
	= filter (\x -> not (null (fst x))) ((generateNewStatePawnMvLeft currBoard player pawnloc) : (generateNewStatePawnMvRight currBoard player pawnloc) : 
											(generateNewStatePawnJumpLeft currBoard player pawnloc) : (generateNewStatePawnJumpRight currBoard player pawnloc) : [])

generateNewStatePawnMvLeftDefault :: [String] -> Char -> (Int, Int) -> Bool -> [String]
generateNewStatePawnMvLeftDefault currBoard player pawnloc isJump
	| ((fst pawnloc) >= (length currBoard) - 1) || null (fst newStr2)		= []
	| not isJump	= replaceListElem (replaceListElem currBoard (fst pawnloc) (generateStr1 (currBoard !! (fst pawnloc)) (snd pawnloc))) (fst pawnloc + 1) (fst newStr2)
	| otherwise 	= generateNewStatePawnMvLeftDefault (replaceListElem (replaceListElem currBoard (fst pawnloc) (generateStr1 (currBoard !! (fst pawnloc)) (snd pawnloc))) (fst pawnloc + 1) (fst newStr2))
						player ((fst pawnloc + 1), snd newStr2) False
	where newStr2 = generateStr2MvLeft (currBoard !! (fst pawnloc)) (currBoard !! (fst pawnloc + 1)) (snd pawnloc) isJump

generateNewStatePawnMvLeft :: [String] -> Char -> (Int, Int) -> ([String], Int)
generateNewStatePawnMvLeft currBoard player pawnloc	
	= ((generateNewStatePawnMvLeftDefault currBoard player pawnloc False), 0)

generateNewStatePawnMvRight :: [String] -> Char -> (Int, Int) -> ([String], Int)
generateNewStatePawnMvRight currBoard player pawnloc	
	= (reverseBoard (generateNewStatePawnMvLeftDefault (reverseBoard currBoard) player (fst pawnloc, (length (currBoard !! (fst pawnloc)) - (snd pawnloc + 1))) False), 0)

generateNewStatePawnJumpLeft :: [String] -> Char -> (Int, Int) -> ([String], Int)
generateNewStatePawnJumpLeft currBoard player pawnloc
	= ((generateNewStatePawnMvLeftDefault currBoard player pawnloc True), 0)

generateNewStatePawnJumpRight :: [String] -> Char -> (Int, Int) -> ([String], Int)
generateNewStatePawnJumpRight currBoard player pawnloc
	= (reverseBoard (generateNewStatePawnMvLeftDefault (reverseBoard currBoard) player (fst pawnloc, (length (currBoard !! (fst pawnloc)) - (snd pawnloc + 1))) True), 0)

generateStr1 :: String -> Int -> String
generateStr1 str1 index	= replaceListElem str1 index '-'
	
generateStr2MvLeft :: String -> String -> Int -> Bool -> (String, Int)
generateStr2MvLeft str1 str2 index isJump
	| ((length str1 - length str2) == 1) && (index > 0)					= makeMove str2 (index - 1) (str1 !! index) isJump
	| ((length str1 - length str2) == -1) && (index < length str1)		= makeMove str2 index (str1 !! index) isJump
	| otherwise															= ([], 0)
	
makeMove :: String -> Int -> Char -> Bool -> (String, Int)
makeMove str2 index pawn isJump
	| (isJump && canJump pawn (str2 !! index)) || (not isJump && canMove pawn (str2 !! index))	
					= ((replaceListElem str2 index pawn), index)
	| otherwise						= ([], 0)


-- helper functions
	
canMove :: Char -> Char -> Bool
canMove pawn1 pawn2		= (not (isEmpty pawn1)) && isEmpty pawn2

canJump :: Char -> Char -> Bool
canJump pawn1 pawn2
	| (pawn1 == 'w') && (pawn2 == 'b')	= True
	| (pawn1 == 'b') && (pawn2 == 'w') 	= True
	| otherwise							= False

isEmpty :: Char -> Bool
isEmpty block =		block == '-'
	
replaceListElem :: [a] -> Int -> a -> [a]
replaceListElem list index elem
	| null list		= []
	| index == 0	= elem : (tail list)
	| otherwise		= (head list) : replaceListElem (tail list) (index - 1) elem		
	
getAllPawns :: [String] -> Char -> [(Int, Int)]
getAllPawns board player	= getAllPawns' board player 0

getAllPawns' :: [String] -> Char -> Int -> [(Int, Int)]
getAllPawns' board player rowNum
	| null board 	= []
	| otherwise 	= getAllPawnsRow (head board) player rowNum ++ getAllPawns' (tail board) player (rowNum + 1)
	
getAllPawnsRow :: String -> Char -> Int -> [(Int, Int)]
getAllPawnsRow row player rowNum	= getAllPawnsRow' row player rowNum 0

getAllPawnsRow' :: String -> Char -> Int -> Int -> [(Int, Int)]
getAllPawnsRow' row player rowNum colNum
	| null row 				= []
	| (head row) == player	= (rowNum, colNum) : getAllPawnsRow' (tail row) player rowNum (colNum + 1)
	| otherwise				= getAllPawnsRow' (tail row) player rowNum (colNum + 1)

reverseBoard :: [String] -> [String]
reverseBoard board = map reverse board




