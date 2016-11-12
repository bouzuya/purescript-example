module Data.Picture where

import Prelude
import Global as Global
import Math as Math

import Data.Maybe
import Data.Array ((:))
import Data.Foldable (foldl, sum)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
-- gcd n m =
--   if n > m
--   then gcd (n - m) m
--   else gcd n (m - n)
gcd n m | n > m = gcd (n - m) m
        | otherwise = gcd n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _ = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- 5.5 (2) TODO

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

person1 = { name: "name 1", address: { street: "street 1", city: "city 1" } }
person2 = { name: "name 2", address: { street: "street 2", city: "city 2" } }
person3 = { name: "name 3", address: { street: "street 3", city: "city 1" } }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [] = default
fromSingleton _ [x] = x
fromSingleton default _array = default

lzs :: Array Int -> Array Int
lzs [] = []
lzs _ = []
lzs xs = case sum xs of
           0 -> xs
           _ -> lzs (unsafePartial tail xs)

partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true
-- partialFunction true = true -- compile error

redundantCase :: Boolean -> Boolean
redundantCase true = true
redundantCase false = false
redundantCase false = false

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

exampleLine :: Shape
exampleLine = Line p1 p2
  where
  p1 :: Point
  p1 = Point { x: 0.0, y: 0.0 }

  p2 :: Point
  p2 = Point { x: 100.0, y: 50.0 }

showPoint :: Point -> String
showPoint (Point { x: x, y: y }) =
  "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle c r) = "Circle " <> showPoint c <> " " <> show r
showShape (Rectangle c w h) =
  "Rectangle " <> showPoint c <> " " <> show w <> " " <> show h
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) = "Text " <> showPoint loc <> " " <> show text

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

circle :: Shape
circle = Circle origin 10.0

scaleShape :: Shape -> Shape
scaleShape (Circle c r) = (Circle c (r * 2.0))
scaleShape (Rectangle c w h) = (Rectangle c (w * 2.0) (h * 2.0))
scaleShape (Line p1 (Point { x: x, y: y })) = (Line p1 p2)
  where
    p2 = (Point { x: (x * 2.0), y: (y * 2.0) })
scaleShape (Text c s) = (Text c s)

extractString :: Shape -> Maybe String
extractString (Text _ s) = Just s
extractString _ = Nothing

newtype Pixels = Pixels Number
newtype Inches = Inches Number

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }


-- copy from purescript-book
intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

-- copy from purescript-book
infixl 4 intersect as /\

-- copy from purescript-book
emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

-- copy from purescript-book
shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b



