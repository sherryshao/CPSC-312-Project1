-- 2013W1 CPSC 312 101
-- Project #1
-- Eric Chu (73265092) & Sherry Shao (60135084)

oska_x1y2 :: [String] -> Char -> Int -> [String]
oska_x1y2 board player depth = generateNewBoard board player depth

-- generateNewBoard : Set up initial state of paths and min max state

generateNewBoard :: [String] -> Char -> Int -> [String]
generateNewBoard board player depth = generateNewBoardWithDepth [[(board, 0)]] player depth True

-- generateNewBoardWithDepth : Recursively generates a set of new paths for given depth

generateNewBoardWithDepth :: [[([String], Int)]] -> Char -> Int -> Bool -> [String]
generateNewBoardWithDepth currPaths player depth isMax
	| depth == 0 	= generateEvaluatedBoard currPaths isMax
	| otherwise		= generateNewBoardWithDepth (generateNewLevel currPaths player) newPlayer (depth-1) (not isMax)
	where
		newPlayer = if (player == 'w') then 'b' else 'w'

-- generateNewLevel : Returns a new set of paths that includes a new level in the tree

generateNewLevel :: [[([String], Int)]] -> Char -> [[([String], Int)]]
generateNewLevel currPaths player = generateNewLevel_tr currPaths player []

generateNewLevel_tr :: [[([String], Int)]] -> Char -> [[([String], Int)]] -> [[([String], Int)]]
generateNewLevel_tr currPaths player newPaths
	| null currPaths	= newPaths
	| otherwise			= 
		generateNewLevel_tr (tail currPaths) player ((generateNewPaths currPath (generateNewStates currBoard player)) ++ newPaths)
	where
		currPath = (head currPaths)
		currBoard = fst (head currPath)

-- generateNewPaths : Attach new states to the head of the current path; return newly generated paths

generateNewPaths :: [([String], Int)] -> [([String], Int)] -> [[([String], Int)]]
generateNewPaths currPath newStates
	| null newStates 	= []
	| otherwise			= ((head newStates):currPath):(generateNewPaths currPath (tail newStates))

-- Evaluation Functions

-- generateEvaluatedBoard : Determines if we're looking for best or worst board

generateEvaluatedBoard :: [[([String], Int)]] -> Bool -> [String]
generateEvaluatedBoard paths isMax
	| isMax  	= generateBestBoard (tail paths) currBoard currWeight
	| otherwise	= generateWorstBoard (tail paths) currBoard currWeight
	where
		currBoard = fst (last (init (head paths)))
		currWeight = snd (last (init (head paths)))

-- generateBestBoard : Returns best board give a set of paths (max)

generateBestBoard :: [[([String], Int)]] -> [String] -> Int -> [String]
generateBestBoard paths bestBoard lastWeight
	| null paths				= bestBoard
	| currWeight > lastWeight	= 
		generateBestBoard (tail paths) currBoard currWeight
	| otherwise					=
		generateBestBoard (tail paths) bestBoard lastWeight
	where
		currBoard = fst (last (init (head paths)))
		currWeight = snd (last (init (head paths)))

-- generateWorstBoard : Returns worst board give a set of paths (min)

generateWorstBoard :: [[([String], Int)]] -> [String] -> Int -> [String]
generateWorstBoard paths worstBoard lastWeight
	| null paths				= worstBoard
	| currWeight < lastWeight	= 
		generateWorstBoard (tail paths) currBoard currWeight
	| otherwise					=
		generateWorstBoard (tail paths) worstBoard lastWeight
	where
		currBoard = fst (last (init (head paths)))
		currWeight = snd (last (init (head paths)))

-- New State Generation Fuctions

-- Currently, calling generateNewStates given a board and player ('w' or 'b'), all states one step away from the current
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




