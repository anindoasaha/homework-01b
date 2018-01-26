{-# LANGUAGE OverloadedStrings #-}

{- Author: Anindo Ashim Saha -}
import CodeWorld

-- Change to exercise1, exercise2, exercise3, exercise4, etc.

main :: IO ()
main = exercise3

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))

maze2 :: Integer -> Integer -> Tile
maze2 x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

data Coord = C Integer Integer

-- exercise 1
initialCoord :: Coord
initialCoord = C 0 2

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

checkPosn :: Coord -> Tile
checkPosn (C x y) = maze x y

movePossible :: Tile -> Coord -> Direction -> Coord
movePossible Ground c d = adjacentCoord d c
movePossible Storage c d = adjacentCoord d c
movePossible _ c _ = c


adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = movePossible (checkPosn (adjacentCoord R c)) c R
    | key == "Up"    = movePossible (checkPosn (adjacentCoord U c)) c U
    | key == "Left"  = movePossible (checkPosn (adjacentCoord L c)) c L
    | key == "Down"  = movePossible (checkPosn (adjacentCoord D c)) c D
    | otherwise      = c
handleEvent _ c      = c

player :: Picture
player = translated 0 0.25 (circle 0.2) -- head
       & path [(0,0),(0.3,0.07)] -- upper hand
       & path [(0,0),(0.3,-0.07)] -- lower hand
       & path [(0,-0.2),(0,0.1)] -- torso
       & path [(0,-0.2),(-0.1,-0.5)] -- left leg
       & path [(0,-0.2),(0.1,-0.5)] -- right leg

drawState :: Coord -> Picture
drawState c = atCoord c player & pictureOfMaze

exercise1 :: IO ()
exercise1 = interactionOf initialCoord handleTime handleEvent drawState

-- exercise2
player2 :: Direction -> Picture
player2 R = translated 0 0.25 (circle 0.2) -- head
       & path [(0,0),(0.3,0.07)] -- upper hand
       & path [(0,0),(0.3,-0.07)] -- lower hand
       & path [(0,-0.2),(0,0.1)] -- torso
       & path [(0,-0.2),(-0.1,-0.5)] -- left leg
       & path [(0,-0.2),(0.1,-0.5)] -- right leg

player2 L = translated 0 0.25 (circle 0.2) -- head
       & path [(0,0),(-0.3,0.07)] -- upper hand
       & path [(0,0),(-0.3,-0.07)] -- lower hand
       & path [(0,-0.2),(0,0.1)] -- torso
       & path [(0,-0.2),(-0.1,-0.5)] -- left leg
       & path [(0,-0.2),(0.1,-0.5)] -- right leg

player2 U = translated 0 0.25 (solidCircle 0.2) -- head
       & path [(0,0),(-0.3,0.07)] -- left hand
       & path [(0,0),(0.3,0.07)] -- right hand
       & path [(0,-0.2),(0,0.1)] -- torso
       & path [(0,-0.2),(-0.1,-0.5)] -- left leg
       & path [(0,-0.2),(0.1,-0.5)] -- right leg

player2 D = translated 0 0.25 (circle 0.2) -- head
       & path [(0,0),(-0.3,0.07)] -- left hand
       & path [(0,0),(0.3,0.07)] -- right hand
       & path [(0,-0.2),(0,0.1)] -- torso
       & path [(0,-0.2),(-0.1,-0.5)] -- left leg
       & path [(0,-0.2),(0.1,-0.5)] -- right leg

data State = State Coord Direction

drawState2 :: State -> Picture
drawState2 (State c d)  = (atCoord c (player2 d)) & pictureOfMaze

handleEvent2 :: Event -> State -> State
handleEvent2 (KeyPress key) (State c d)
    | key == "Right" = State (movePossible (checkPosn (adjacentCoord R c)) c R) R
    | key == "Up"    = State (movePossible (checkPosn (adjacentCoord U c)) c U) U
    | key == "Left"  = State (movePossible (checkPosn (adjacentCoord L c)) c L) L
    | key == "Down"  = State (movePossible (checkPosn (adjacentCoord D c)) c D) D
    | otherwise      = State c d
handleEvent2 _ s     = s

initialState :: State
initialState = State (C 0 2) R

handleTime2 :: Double -> State -> State
handleTime2 _ c = c

exercise2 :: IO ()
exercise2 = interactionOf initialState handleTime2 handleEvent2 drawState2

-- exercise 3

resetableInteractionOf :: world -> (Double -> world -> world) -> (Event -> world -> world) -> (world -> Picture) -> IO ()
resetableInteractionOf initialState handleTime handleEvent drawState = (interactionOf initialState handleTime handleEvent drawState)

exercise3 :: IO ()
exercise3 = resetableInteractionOf initialState handleTime2 handleEvent3 drawState2
    where handleEvent3 (KeyPress key) (State c d)
            | key == "Esc" = initialState
          handleEvent3 event state = handleEvent2 event state

-- exercise4

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 11  || abs y > 11  = Blank
  | abs x == 11 || abs y == 11 = Wall
  | x ==  2 && y <= -3         = Wall
  | x > 3 && y == -2           = Storage
  | x < 0 && x > -8 && y == 0  = Box
  | otherwise                  = Ground




