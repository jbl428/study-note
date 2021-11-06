import Control.Monad.State

data Input = Coin | Turn
    deriving Show

data Machine = Machine {
    locked :: Bool
    , candies :: Int
    , coins :: Int
} deriving Show

handleInput :: Input -> State Machine (Int, Int)
handleInput Coin = do
    Machine locked candies coins <- get
    if candies == 0
        then return (candies, coins)
        else do
            put (Machine False candies (coins + 1))
            return (candies, coins + 1)
handleInput Turn = do
    Machine locked candies coins <- get
    (a, b, c) <- return (turn locked candies coins)
    put (Machine a b c)
    return (b, c)


turn :: Bool -> Int -> Int -> (Bool, Int, Int)
turn locked 0 coins = (locked, 0, coins)
turn True candies coins = (True, candies, coins)
turn False candies coins
    | coins > 0 && candies > 0 = (True, candies - 1, coins -1)
    | otherwise = (True, candies, coins)
