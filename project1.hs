-- 2013W1 CPSC 312 101
-- Project #1
-- Eric Chu (73265092) & Sherry Shao (60135084)

oska_x1y2 :: [String] -> Char -> Int -> [String]
oska_x1y2 board player depth = generateNewBoard board player depth

-- generateNewBoard : Set up initial state of paths and min max state

generateNewBoard :: [String] -> Char -> Int -> [String]
generateNewBoard board player depth 
	| depth == 0			= board
	| otherwise				= fst (generateNewBoardWithDepth player depth True (board, 0))

-- generateNewBoardWithDepth : i.e. the minimax function
-- 		Recursively pass up the min / max weight up the tree
--		to compare until we reach the "next step" level

generateNewBoardWithDepth :: Char -> Int -> Bool -> ([String], Int) -> ([String], Int)
generateNewBoardWithDepth player depth isMax board
	| null newBoards	= board
	| depth == 1 		= generateEvaluatedBoard newBoards isMax
	| otherwise			= generateEvaluatedBoard (zip (map fst newBoards) (map snd newTuples)) isMax
	where
		newPlayer = if (player == 'w') then 'b' else 'w'
		newBoards = generateNewStates (fst board) player isMax
		newTuples = map (generateNewBoardWithDepth newPlayer (depth-1) (not isMax)) newBoards

-- Evaluation Functions

-- generateEvaluatedBoard : Determines if we're looking for best or worst board

generateEvaluatedBoard :: [([String], Int)] -> Bool -> ([String], Int)
generateEvaluatedBoard boards isMax
	| isMax  	= generateBestBoard boards (head boards)
	| otherwise	= generateWorstBoard boards (head boards)

-- generateBestBoard : Returns best board give a set of boards (max)

generateBestBoard :: [([String], Int)] -> ([String], Int) -> ([String], Int)
generateBestBoard boards bestBoard
	| null boards				= bestBoard
	| (snd currBoard) > (snd bestBoard)	= 
		generateBestBoard (tail boards) currBoard
	| otherwise					=
		generateBestBoard (tail boards) bestBoard
	where
		currBoard = head boards

-- generateWorstBoard : Returns worst board give a set of boards (min)

generateWorstBoard :: [([String], Int)] -> ([String], Int) -> ([String], Int)
generateWorstBoard boards worstBoard
	| null boards				= worstBoard
	| (snd currBoard) < (snd worstBoard)	= 
		generateWorstBoard (tail boards) currBoard
	| otherwise					=
		generateWorstBoard (tail boards) worstBoard
	where
		currBoard = head boards

-- New State Generation Fuctions

-- Currently, calling generateNewStates given a board and player ('w' or 'b'), all states one step away from the current
-- state will be returned as a list. Score evaluation is current set to 0.
-- also, implementation assumes that 'b' pawns move start at the bottom and move upward while 'w' pawns start at the top
-- and move downward. 

generateNewStates :: [String] -> Char -> Bool -> [([String], Int)]
generateNewStates currBoard player isMax
	| ((player == 'w') && isMax) || ((player == 'b') && not isMax)		
						= assignScores (generateNewStatesNoScore currBoard 'w' (getAllPawns currBoard 'w')) player (fromIntegral (length (getAllPawns currBoard player)))
	| ((player == 'b') && isMax) || ((player == 'w') && not isMax)		
						= assignScores (generateNewStatesNoScore (reverse currBoard) 'b' (getAllPawns (reverse currBoard) 'b')) player (fromIntegral (length (getAllPawns (reverse currBoard) player)))
	| otherwise			= []

assignScores :: [[String]] -> Char -> Float -> [([String], Int)]
assignScores newStates player numOfPawns
	| null newStates 	= []
	| otherwise 		= assignScore (head newStates) player numOfPawns : assignScores (tail newStates) player numOfPawns

assignScore :: [String] -> Char -> Float -> ([String], Int)
assignScore state player numOfPawns
	| player == 'w'		= (state, ((assignScore' state 'w' 0 numOfPawns (fromIntegral (length (state !! 0)))) - (assignScore' (reverse state) 'b' 0 numOfPawnsOppo (fromIntegral (length (state !! 0))))))
	| otherwise			= (state, ((assignScore' (reverse state) 'b' 0 numOfPawns (fromIntegral (length (state !! 0)))) - (assignScore' state 'w' 0 numOfPawnsOppo (fromIntegral (length (state !! 0))))))
	where numOfPawnsOppo = fromIntegral (length (getAllPawns state (getOpponent player)))

assignScore' :: [String] -> Char -> Int -> Float -> Float -> Int
assignScore' state player level numOfPawns totalPawns
	| null state 	= 0
	| otherwise		= (assignScoreRow (head state) player level numOfPawns totalPawns) + (assignScore' (tail state) player (level + 1) numOfPawns totalPawns)

assignScoreRow :: String -> Char -> Int -> Float -> Float -> Int
assignScoreRow row player level numOfPawns totalPawns
	| null row 					= 0
	| (head row) ==  player 	= (10 * fromIntegral level) + ((floor ((((0.8 * 10 * totalPawns) / numOfPawns) / ((2 * totalPawns) - 4)) * fromIntegral level)) * fromIntegral (round totalPawns - round numOfPawns)) 
									+ (assignScoreRow (tail row) player level numOfPawns totalPawns)
	| otherwise					= assignScoreRow (tail row) player level numOfPawns totalPawns

generateNewStatesNoScore :: [String] -> Char -> [(Int, Int)] -> [[String]]
generateNewStatesNoScore currBoard player pawnlocs
	| player == 'w'		= generateNewStates' currBoard player pawnlocs
	| player == 'b'		= reverseNewStates (generateNewStates' currBoard player pawnlocs)
	| otherwise			= []

reverseNewStates :: [[String]] -> [[String]]
reverseNewStates states
	| null states 	= []
	| otherwise 	= reverse (head states): reverseNewStates (tail states)

generateNewStates' :: [String] -> Char -> [(Int, Int)] -> [[String]]
generateNewStates' currBoard player pawnlocs
	| null pawnlocs		= []
	| otherwise			= generateNewStatesPawn currBoard player (head pawnlocs) ++ generateNewStates' currBoard player (tail pawnlocs)

generateNewStatesPawn :: [String] -> Char -> (Int, Int) -> [[String]]
generateNewStatesPawn currBoard player pawnloc 	
	= filter (\x -> not (null x)) ((generateNewStatePawnMvLeft currBoard player pawnloc) : (generateNewStatePawnMvRight currBoard player pawnloc) : 
											(generateNewStatePawnJumpLeft currBoard player pawnloc) : (generateNewStatePawnJumpRight currBoard player pawnloc) : [])

generateNewStatePawnMvLeftDefault :: [String] -> Char -> (Int, Int) -> Bool -> [String]
generateNewStatePawnMvLeftDefault currBoard player pawnloc isJump
	| ((fst pawnloc) >= (length currBoard) - 1) || null (fst newStr2)		= []
	| not isJump	= replaceListElem (replaceListElem currBoard (fst pawnloc) (generateStr1 (currBoard !! (fst pawnloc)) (snd pawnloc))) (fst pawnloc + 1) (fst newStr2)
	| otherwise 	= generateNewStatePawnMvLeftDefault (replaceListElem (replaceListElem currBoard (fst pawnloc) (generateStr1 (currBoard !! (fst pawnloc)) (snd pawnloc))) (fst pawnloc + 1) (fst newStr2))
						player ((fst pawnloc + 1), snd newStr2) False
	where newStr2 = generateStr2MvLeft (currBoard !! (fst pawnloc)) (currBoard !! (fst pawnloc + 1)) (snd pawnloc) isJump

generateNewStatePawnMvLeft :: [String] -> Char -> (Int, Int) -> [String]
generateNewStatePawnMvLeft currBoard player pawnloc	
	= generateNewStatePawnMvLeftDefault currBoard player pawnloc False

generateNewStatePawnMvRight :: [String] -> Char -> (Int, Int) -> [String]
generateNewStatePawnMvRight currBoard player pawnloc	
	= reverseBoard (generateNewStatePawnMvLeftDefault (reverseBoard currBoard) player (fst pawnloc, (length (currBoard !! (fst pawnloc)) - (snd pawnloc + 1))) False)

generateNewStatePawnJumpLeft :: [String] -> Char -> (Int, Int) -> [String]
generateNewStatePawnJumpLeft currBoard player pawnloc
	= generateNewStatePawnMvLeftDefault currBoard player pawnloc True

generateNewStatePawnJumpRight :: [String] -> Char -> (Int, Int) -> [String]
generateNewStatePawnJumpRight currBoard player pawnloc
	= reverseBoard (generateNewStatePawnMvLeftDefault (reverseBoard currBoard) player (fst pawnloc, (length (currBoard !! (fst pawnloc)) - (snd pawnloc + 1))) True)

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

getOpponent :: Char -> Char
getOpponent player
	| player == 'w' 	= 'b'
	| otherwise			= 'w'




