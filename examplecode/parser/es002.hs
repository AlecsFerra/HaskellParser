x :: Int
x = 1 + 2 * 3

z :: Int
z =
  let y = 2
  in y

data Color = Red | Blue | Green

f :: Color -> Int
f Red = 0
f Blue = 1
f Green = 2

