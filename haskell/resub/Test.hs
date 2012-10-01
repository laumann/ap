import World
import MEL
import Test.HUnit
import Test.QuickCheck as QC


defaultWorld = initialWorld testMaze

testWorld pos dir = (testMaze, Robot pos dir []) 

-- | Function for starting programs from arbitrary position. Should ONLY be used
-- for testing purposes, otherwise runProg is the way to go.
testProg :: Position -> Direction -> Program -> MEL.Result ([Position], Direction)
testProg p d program = let (RC prog) = interp program
                              in case (prog $ testWorld p d) of
                                 (Right (_, r)) -> MEL.Success $ status r
                                 (Left r)       -> MEL.Failure $ status r
  where status robot = ((pos robot):(hist robot), (dir robot))

-- | In order for QuickCheck to generate positions
newtype LegalPosition = LP Position deriving (Eq, Show)

instance QC.Arbitrary LegalPosition where 
   arbitrary = QC.elements [LP (x,y) | x <- [0..4], y <- [0..4]]
   
instance QC.Arbitrary Direction where
   arbitrary = QC.elements [North, East, South, West]

-- | The wall follower algorithm specified here: http://en.wikipedia.org/wiki/Maze_solving_algorithm
-- Using the left-hand rule.
wallFollowProg = While (Not AtGoalPos)
                               (If (Wall ToLeft)
                                   (If (Wall Ahead) TurnRight Forward)
                                   (Block [TurnLeft, Forward])
                               )   
   
   
wallFollower (LP pos) dir = testProg pos dir wallFollowProg
                         
testWallfollower lopos dir = let result = wallFollower lopos dir
                             in case result of
                               MEL.Failure _               -> False
                               MEL.Success (pos:hist, dir) -> pos == (4,4)

----- Unit Tests

-- | Simple If tests, also check if the condition was evaluated correctly
testIf = TestCase $ assertBool "Test simple If command" $ 
         (MEL.Success ([(0,0)], East) == (runProg testMaze (If (Wall Ahead) TurnRight TurnLeft)))
         && evalCond (defaultWorld) (Wall Ahead)
         
testIf2 = TestCase $ assertBool "Test simple If command" $ 
          (MEL.Success ([(0,0)], West) == (runProg testMaze (If (Wall ToRight) TurnRight TurnLeft)))
          && not(evalCond (defaultWorld) (Wall ToRight))

-- | Empty block, should return input world
testEmptyBlock = TestCase $ assertBool "Test if empty block returns unchanged Robot" $
		 MEL.Success ([(0,0)], North) == runProg testMaze (Block [])  

-- | All basic moves 
testNavi = TestCase $ assertBool "Test some simple navigation" $ 
           MEL.Success ([(1,1),(1,0),(1,1),(1,0),(0,0)],North) == 
           (runProg testMaze ( Block [TurnRight, Forward, TurnLeft, Forward, Backward, Forward]))

-- | Simple while loop
testWhile = TestCase $ assertBool "Test simple While command" $ 
            MEL.Success ([(1,0),(0,0)],East) == runProg testMaze ( Block [ While (Wall Ahead) TurnLeft, Forward])

-- | Checks if running into a wall results in failure
testError = TestCase $ assertBool "Test if crossing walls fails and later commands are ignored" $ 
            MEL.Failure([(0,0)], North) == runProg testMaze (Block[Forward, TurnRight, Forward, Forward])

-- | Checks if correct mazes are accepted
testGoodMaze = TestCase $ assertBool "Test if well-formed maze is accepted" $
               let maze = fromList goodMaze 
               in getGoalPos maze == (1,1)

-- | All tests we want to succeed
successTests = TestList [testGoodMaze, testEmptyBlock, testNavi, testIf, testIf2, testWhile, testError]

-- | Several kinds of malformed mazes
testBadMaze1 = TestCase $ assertBool "Test if mazes missing South walls are rejected" $
               let maze = fromList badMaze1 
               in getGoalPos maze == (1,1)
                  
testBadMaze2 = TestCase $ assertBool "Test if mazes missing East walls are rejected" $
               let maze = fromList badMaze2
               in getGoalPos maze == (1,1)
                  
testBadMaze3 = TestCase $ assertBool "Test if mazes with inconsistent neighbor walls are rejected" $
               let maze = fromList badMaze3
               in getGoalPos maze == (1,1)
                  
testBadMaze4 = TestCase $ assertBool "Test if mazes missing cells are rejected" $
               let maze = fromList badMaze4
               in getGoalPos maze == (1,1)

-- | These tests should fail
errorTests = TestList [testBadMaze1, testBadMaze2, testBadMaze3, testBadMaze4]

-- | Run all tests
testAll = do
          print "The following tests should succeed:"
          runTestTT successTests
          quickCheck testWallfollower
          print "The following tests should all result in an Error:"
          runTestTT errorTests

goodMaze :: [(Position, Cell)]
goodMaze = [((0,0), [West, South])
           ,((0,1), [West, North])
           ,((1,0), [East, South])
           ,((1,1), [East, North])]
          
badMaze1 :: [(Position, Cell)]
badMaze1 = [((0,0), [West])
           ,((0,1), [West, North])
           ,((1,0), [East, South])
           ,((1,1), [East, North])]
           
badMaze2 :: [(Position, Cell)]
badMaze2 = [((0,0), [West, South])
           ,((0,1), [West, North])
           ,((1,0), [South])
           ,((1,1), [East, North])]
           
badMaze3 :: [(Position, Cell)]
badMaze3 = [((0,0), [West, East, South])
           ,((0,1), [West, North])
           ,((1,0), [East, South])
           ,((1,1), [East, North])]

badMaze4 :: [(Position, Cell)]
badMaze4 = [((0,0), [West, South])
           ,((0,1), [West, North])
           ,((1,1), [East, North])]
           