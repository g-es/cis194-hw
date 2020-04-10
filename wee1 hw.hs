{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 10

trafficLight :: Integer -> Picture
trafficLight 1  = botCircle green & midCircle black & topCircle black & frame
trafficLight 2 = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 3 = botCircle black & midCircle black & topCircle red & frame 
trafficLight 4 = botCircle black & midCircle yellow & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
  | round (t) `mod` 6 <= 1 = trafficLight 1
  | round (t) `mod` 6 == 2 = trafficLight 2
  | round (t) `mod` 6 <= 4 && (round (t) `mod` 6 >2) = trafficLight 3
  | otherwise                = trafficLight 4

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))
dots :: Integer -> Double -> Picture
dots n t
  | n == 0 = colored yellow (solidCircle (0.1 * t))
  | otherwise = translated 0 1 (rotated (pi/10) (dots (n-1) t) & rotated (-pi/10) (dots (n-1) t))

ourPicture :: Picture
ourPicture = tree 8

flower :: Double -> Picture
flower t
  | round(t) <= 10 = dots 8 (t/5) & ourPicture
  | otherwise      = dots 8 2 & ourPicture
exercise2 :: IO ()
exercise2 = animationOf flower

-- Exercise 3

wall, ground, storage, box :: Picture
wall    = colored gray (solidRectangle 1 1)
ground  = colored yellow (solidRectangle 1 1)
storage = colored brown (solidRectangle 1 1)
box     = solidCircle 0.3 & colored yellow (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile n
  | n == 1 = wall
  | n == 2 = ground
  | n == 3 = box
  | n == 4 = storage
  | otherwise = blank

drawMaze :: Double -> Double -> Picture
drawMaze x y 
  | x <= 4 = translated x y (drawTile (maze x y)) & drawMaze (x+1) y
  | otherwise = blank

drawAllRow :: Double -> Double -> Picture
drawAllRow x y
  | y <= 4 = drawMaze x y & drawAllRow x (y+1)
  | otherwise = blank

pictureOfMaze :: Picture
pictureOfMaze = drawAllRow (-4) (-4)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Double -> Double -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
 