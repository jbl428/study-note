import Control.Monad.State

data Input = Coin | Turn
    deriving Show

data Machine = Machine {
    locked :: Bool
    , candies :: Int
    , coins :: Int
} deriving Show

turn' :: Machine -> Machine
turn' beforeMachine
    | not locked && candies > 0 = Machine True (candies - 1) coins
    | otherwise = beforeMachine
    where (Machine locked candies coins) = beforeMachine

coin' :: Machine -> Machine
coin' beforeMachine
    | locked && candies > 0 = Machine False candies (coins + 1)
    | otherwise = beforeMachine
    where (Machine locked candies coins) = beforeMachine

handleInput :: Input -> State Machine ()
handleInput Coin = modify coin'
handleInput Turn = modify turn'

simulateMachine :: [Input] -> State Machine (Int, Int)
simulateMachine inputs = do
    forM_ inputs handleInput
    (Machine _ a b) <- get
    return (a, b)
