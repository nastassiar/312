-- This is a program to solve a simple rushHour game.
-- The goal of the game is to get the vehicle represented by XX out of the traffic jam
-- by moving the other cars. The exit is in the 3rd row on the far right. 


-- To run:
-- rushing startState

type State = [String]
type Position = (Int,Int)


-- Start states created for testing purposes
startState = ["--B---","--B---","XXB---","--AA--","------","------","------"] 
startState2 =["--B---","--B---","XXB---","--AA--","-CC---","------","------"] 
startState3 =["--B---","--B---","XXB---","--AA--","------","---CCC","------"]

--
-- rushing:
-- This function takes in a state representing the start state
-- and outputs to the console a readable list of states representing the
-- the moves to solve the puzzle
--
-- Arguments:
-- start: the start state of the puzzle
--
-- Returns: prints to the console a readable list of states
--
rushing :: State -> IO ()
rushing state = putStrLn (printAllOutput(rushHour state))

--
-- rushHour:
-- This function takes in a state (which is a list of strings) representing the start state
-- and returns a new list states that is the moves necessary to solve the puzzle
--
-- Arguments:
-- start: the start state of the puzzle
--
-- Returns: the new list of states 
--
rushHour :: State -> [State]
rushHour start = reverse (statesearch [start] [])


--
-- stateSearch:
-- This function takes in 2 list of states 
-- and returns a new list states that is the moves necessary to solve the puzzle
--
-- Arguments:
-- unexplored: the unexplored states of the puzzle
-- path: the path found so far
--
-- Returns: the new list of states 
--
statesearch :: [State] -> [State] -> [State]
statesearch unexplored path
   | null unexplored              		= []
   | isGoalState (head unexplored)      = (head unexplored):path
   | (not (null result))         		= result
   | otherwise                   		= 
        statesearch (tail unexplored) path
     where 
     	result = statesearch (generateNewStates (head unexplored) path) ((head unexplored):path)

--
-- generateNewStates:
-- This function take in a state and a list of states 
-- and will produce a list of unexplored states from the current state 
-- that are not already in the path
--
-- Arguments:
-- currState: the current state
-- path: the path found so far
--
-- Returns: the new list of states that can be found from the currentState and are not in the path already
--
generateNewStates :: State -> [State] -> [State]
generateNewStates currState path = checkStates (createStates currState (0,0)) path

--
-- checkStates:
-- This function take in 2 list of states  
-- and return all states from the first list that are not found
-- in the second list
--
-- Arguments:
-- states: the new states
-- path: the path found so far
--
-- Returns: the new list of states from the passed in states that are not in the path
--
checkStates :: [State] -> [State] -> [State]
checkStates states path
	| null states 						= []
	| elem (head states) path 	 		= checkStates (tail states) path
	| otherwise	 						= (head states) : (checkStates (tail states) path)


--
-- createStates:
-- This function take in a state and a position  
-- and will create all states adjacent to the current state
--
-- Arguments:
-- state: the current state
-- (x,y): the current position 
--
-- Returns: the list of states possible from the state and the position
--
createStates :: State -> Position -> [State]
createStates state (x,y)
	| (y > ((length state)-1))						= []
	| x > (length (head state)-1)					= createStates state (0,y+1)
	| checkPosition state (x,y) == '-' 				= createStates state (x+1,y)
	| otherwise 									= (makeAllPossibleStates state (x,y)) ++ (createStates state ((x+1),y))

--
-- makeAllPossibleStates:
-- This function take in a state and a position  
-- and will create all states adjacent to the current state
--
-- Arguments:
-- state: the current state
-- (x,y): the current position 
--
-- Returns: the list of states possible from the state and the position
--
makeAllPossibleStates :: State -> Position -> [State]
makeAllPossibleStates state (x,y)
	-- we will only generate new states once for each vehicle.  The first guard ensures this.
	| getPosition state char /= (x,y) 	= []
	| isHorizontal state char	= (moveLeft state char (x,y) ln):(moveRight state char (x,y) ln):[]
	| otherwise 				= (moveUp state char (x,y) ln):(moveDown state char (x,y) ln):[]
	where 
		char = checkPosition state (x,y)
		ln = lengthOfVehicle state char

--
-- moveLeft:
-- This function takes in a state, and a Char, and a Position and a Int 
-- that correspond to different aspects of a vehicle. This function will 
-- produce a new state that is the same as the passed in state, but with 
-- the vehicle moved to the left one space.
--
-- Arguments:
-- state: the current state
-- char: character representing the Vehicle to be moved
-- (x,y): the position of the top left most character of the vehicle
-- ln : the length of the vehicle
--
-- Returns: the state after the vehicle has been moved to the left
--
moveLeft :: State -> Char -> Position -> Int-> State
moveLeft state char (x,y) ln
	| (x-1) < 0 								= []
	| checkPosition state ((x-1),y) /= '-' 		= []
	| otherwise 								= setPosition (setPosition state char (x-1,y) 0) '-' (x+ln-1,y) 0

--
-- moveRight:
-- This function takes in a state, and a Char, and a Position and a Int 
-- that correspond to different aspects of a vehicle. This function will 
-- produce a new state that is the same as the passed in state, but with 
-- the vehicle moved to the right one space.
--
-- Arguments:
-- state: the current state
-- char: character representing the Vehicle to be moved
-- (x,y): the position of the top left most character of the vehicle
-- ln : the length of the vehicle
--
-- Returns: the state after the vehicle has been moved to the right
--
moveRight :: State -> Char -> Position -> Int-> State
moveRight state char (x,y) ln
	| (x+ln) > (length (head state))			= []
	| checkPosition state ((x+ln),y) /= '-' 	= []
	| otherwise 								= setPosition (setPosition state char (x+ln,y) 0) '-' (x,y) 0

--
-- moveUp:
-- This function takes in a state, and a Char, and a Position and a Int 
-- that correspond to different aspects of a vehicle. This function will 
-- produce a new state that is the same as the passed in state, but with 
-- the vehicle moved to the up one space.
--
-- Arguments:
-- state: the current state
-- char: character representing the Vehicle to be moved
-- (x,y): the position of the top left most character of the vehicle
-- ln : the length of the vehicle
--
-- Returns: the state after the vehicle has been moved to the up
--
moveUp :: State -> Char -> Position -> Int-> State
moveUp state char (x,y) ln
	| (y-1) < 0 								= []
	| checkPosition state (x,(y-1)) /= '-' 		= []
	| otherwise 								= setPosition (setPosition state char (x,y-1) 0) '-' (x,y+ln-1) 0

--
-- moveDown:
-- This function takes in a state, and a Char, and a Position and a Int 
-- that correspond to different aspects of a vehicle. This function will 
-- produce a new state that is the same as the passed in state, but with 
-- the vehicle moved to the down one space.
--
-- Arguments:
-- state: the current state
-- char: character representing the Vehicle to be moved
-- (x,y): the position of the top left most character of the vehicle
-- ln : the length of the vehicle
--
-- Returns: the state after the vehicle has been moved to the down
--
moveDown :: State -> Char -> Position -> Int-> State
moveDown state char (x,y) ln
	| (y+ln) > length state						= []
	| checkPosition state (x,(y+ln)) /= '-' 	= []
	| otherwise 								= setPosition (setPosition state char (x,y+ln) 0) '-' (x,y) 0

--
-- setPosition:
-- This function takes in a state, and a char, and a Position and a Int 
-- and will produce a new state that is the same as the passed in state, but with 
-- the value at the passed in position set to the passed in char.
--
-- Arguments:
-- state: the current state
-- char: character to set the position in the state to
-- (x,y): the position in the state to set to the char
-- acc: accumulator to keep track of current position
--
-- Returns: the state after the char at the position has been changed
--
setPosition :: State -> Char -> Position -> Int -> State 
setPosition state char (x,y) acc
	| y == acc					= (changeStr x char (head state)) : (tail state)
	| otherwise 				= (head state): setPosition (tail state) char (x,y) (acc+1)

-- We based this changeStr function on one found on stack overflow
-- http://stackoverflow.com/questions/20011566/how-to-change-a-char-within-a-list-of-strings-in-haskell
--
-- changeStr:
-- This function an Int, a Char and a String  
-- and will produce a String with the character at the passed 
-- in position set to the Char
--
-- Arguments:
-- x: The position to change
-- char: character to change to
-- zs: the string
--
-- Returns: a new string with the character at position x changed to char
--
changeStr :: Int -> Char -> String -> String
changeStr x char zs = take (x) zs ++ [char] ++ drop (x+1) zs
	
--
-- lengthOfVehicle:
-- This function takes in a state and a char and returns the length of the vehicle  
-- represented by the char.
--
-- Arguments:
-- state: the current state
-- ch: character representing the vehicle
--
-- Returns: the length of the vehicle
--
lengthOfVehicle :: State -> Char -> Int
lengthOfVehicle state ch
	| isHorizontal state ch 	= horizontalLength state position ch 0
	| otherwise 				= verticalLength state position ch 0 
	where position = getPosition state ch

--
-- horizontalLength:
-- This function takes in a state, a position, a char and an int and 
-- returns the length of the horizontal vehicle represented by the char.
--
-- Arguments:
-- state: the current state
-- (x, y): The upper left most position of the character representing the vehicle
-- ch: character representing the vehicle
-- acc: accumulator used to track the current position in the state
--
-- Returns: the length of the vehicle
--
horizontalLength :: State -> Position -> Char -> Int -> Int 
horizontalLength state (x,y) ch acc
	| ch /= checkPosition state (x,y) 	= acc
	| otherwise 						= horizontalLength state (x+1,y) ch (acc+1)

--
-- verticalLength:
-- This function takes in a state, a position, a char and an int and 
-- returns the length of the vertical vehicle represented by the char.
--
-- Arguments:
-- state: the current state
-- (x, y): The upper left most position of the character representing the vehicle
-- ch: character representing the vehicle
-- acc: accumulator used to track the current position in the state
--
-- Returns: the length of the vehicle
--
verticalLength :: State -> Position -> Char -> Int -> Int 
verticalLength state (x,y) ch acc
	| ch /= checkPosition state (x,y) 	= acc
	| otherwise 						= verticalLength state (x,y+1) ch (acc+1)

--
-- printAllOutput:
-- This function takes in a list of states and returns a string representing the states.
--
-- Arguments:
-- states: a list of states
--
-- Returns: a string representing the states separated by newline characters
--
printAllOutput :: [State] -> String
printAllOutput states
	| null states 			    = ""
	| otherwise 				= (printOutput (head states)) ++ "\n" ++ (printAllOutput (tail states))

--
-- printAllOutput:
-- This function takes in a state and returns a string representing the state.
--
-- Arguments:
-- state: state
--
-- Returns: a string representing the different lines of the state separated by newline characters
--
printOutput :: State -> String
printOutput state
	| null state 				= ""
	| otherwise					= (head state) ++ "\n" ++ (printOutput (tail state))

--
-- isHorizontal:
-- This function takes in a state and a char and returns true if the
-- vehicle represented by the char is horizonatl and false otherwise.
--
-- Arguments:
-- state: the current state
-- char: the char representing the vehicle
--
-- Returns: a bool indicating whether the vehicle is horizontal of not
--
isHorizontal :: State -> Char -> Bool
isHorizontal state ch 
	| (checkPosition state (x,y+1)) == ch 	= False
	| otherwise 							= True
	where (x,y) = getPosition state ch 

--
-- checkPosition:
-- This function takes in a state and a position and returns the char
-- at this position
--
-- Arguments:
-- state: the current state
-- positon: the position to check
--
-- Returns: the char at the position in the state
--
checkPosition :: State -> Position -> Char
checkPosition state (x,y) 			
	| y < (length state) && x < (length (head state))	= (state !! y) !! x
	| otherwise 										= '!'

--
-- getPosition:
-- This function takes in a state and a char and returns the upper left most 
-- positon of this char.
--
-- Arguments:
-- state: the current state
-- char: the char to find
--
-- Returns: the upper left most positon of the char in the State
--
getPosition:: State -> Char -> Position
getPosition state ch = getPosition_helper state ch (0,0)

--
-- getPosition_helper:
-- This function takes in a state, a char and a position and returns
-- the upper left most positon of this char.
--
-- Arguments:
-- state: the current state
-- char: the char to find
-- (x,y): the current position
--
-- Returns: the upper left most positon of the char in the State
--
getPosition_helper :: State -> Char -> Position -> Position
getPosition_helper state ch (x,y)
	| null state 			 		= (-1,-1) -- we need to fix this
	| elem ch (head state) 			= getInnerPosition (head state) ch (x,y) 
	| otherwise	 					= getPosition_helper (tail state) ch (0,y+1)

--
-- getInner:
-- This function takes in a String, a char and a position and return
-- the position of the char in the string.
--
-- Arguments:
-- InnerState: a string
-- ch: the char to find
-- (x,y): the current position
--
-- Returns: the position of the char in the string
--
getInnerPosition :: String -> Char -> Position -> Position
getInnerPosition innerState ch (x,y)
	| (head innerState) == ch 		= (x,y)
	| otherwise						= getInnerPosition (tail innerState) ch (x+1,y)

--
-- isGoalState:
-- This function takes in a state, and returns true if it is the goal state.
--
-- Arguments:
-- state: the current state
--
-- Returns: true if the state is the goal state and false otherwise
--
isGoalState :: State -> Bool
isGoalState state 
	| getPosition state 'X' == (4,2) 	= True
	| otherwise 						= False
