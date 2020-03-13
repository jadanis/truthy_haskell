module Truthy
( Truthy
, if_
, then_
, else_
, switch_
, case_
, cases_
, cases_pure_
, breaks_
, not_
, is_
, and_
, or_
, nor_
, nand_
, xor_
, iff_
, imply_
, default_
)
where

import Data.Char (ord)

class Truthy a where
  not_ :: a -> Bool

instance Truthy Bool where
  not_ = not

instance Truthy Int where
  not_ = (>=) 0
 
instance Truthy Float where
  not_ = (>=) 0

instance Truthy Double where
  not_ = (>=) 0

instance Truthy Integer where
  not_ = (>=) 0

instance Truthy [a] where
  not_ [] = True
  not_ _ = False

instance Truthy (Maybe a) where
  not_ Nothing = True
  not_ _ = False

-- Maybe should be false for all none printable characters?
instance Truthy Char where
  not_ c
    | ((ord c) < 32) || ((ord c) == 127) = True
    | otherwise = False

-- Purely for readability
then_ :: a -> a
then_ = id
else_ :: a -> a
else_ = id

is_ :: (Truthy a) => a -> Bool
is_ = not_ . not_

if_ :: (Truthy a) => a -> (x -> b) -> x -> (y -> b) -> y -> b
if_ t f x g y = if (is_ t) then f x else g y

ifnot_ :: (Truthy a) => a -> (x -> b) -> x -> (y -> b) -> y -> b
ifnot_ = if_ . not_

and_ :: (Truthy a) => a -> a -> Bool
and_ x y = (&&) (is_ x) (is_ y)

or_ :: (Truthy a) => a -> a -> Bool
or_ x y = (||) (is_ x) (is_ y)

nand_ :: (Truthy a) => a -> a -> Bool
nand_ x y = not $ and_ x y

nor_ :: (Truthy a) => a -> a -> Bool
nor_ x y= not $ or_ x y

xor_ :: (Truthy a) => a -> a -> Bool
xor_ x y = and_ (or_ x y) (nand_ x y)

iff_ :: (Truthy a) => a -> a -> Bool
iff_ x y = not $ xor_ x y

imply_ :: (Truthy a) => a -> a -> Bool
imply_ x y = or_ (is_ y) (not_ x)

while_ :: (Truthy a) => x -> (x -> a) -> (x -> x) -> x
while_ inp t f
  | is_ cond = while_ res t f
  | otherwise = inp
  where
    cond = t inp
    res = f inp

do_ :: (Truthy a) => (x -> (x -> a) -> (x -> x) -> x) -> x -> (x -> a) -> (x -> x) -> x
do_ w inp t f = w (f inp) t f

case_ :: (Truthy a) => (x -> a) -> (x -> b) -> (x -> Maybe b)
case_ t f = (\x -> if_ (t x) then_ (Just . f $ x) else_ Nothing)

default_ = id

switch_ :: x -> [x -> Maybe b] -> (b -> b) -> (x -> b) -> b
switch_ inp cases def g =
  case cases of
    [] -> (def . g) inp
    (f:fs) ->
      case (f inp) of
        Nothing -> switch_ inp fs def g
        (Just res) -> res

cases_ :: (Truthy a) => [(x -> a, x -> b)] -> [x -> Maybe b]
cases_ keys =
  case keys of
    [] -> []
    ((t,f):rs) -> (case_ t f):(cases_ rs)

cases_pure_ :: (Truthy a) => [(x -> a, b)] -> [x -> Maybe b]
cases_pure_ keys = cases_ [ (t,(\_ -> b)) | (t,b) <- keys ]

breaks_ :: (Truthy a) => [x -> a] -> [b] -> [x -> Maybe b]
breaks_ tests res = cases_pure_ $ zip tests res

-- Silly examply function
example_ :: Int -> String
example_ x =
  switch_ x mycases default_ bummer
  where
    mycases = breaks_ [(>= 100), (>= 50), (>= 25), (>= 10)] ["Woo hoo!", "Nice!", "Getting there", "Not quite"]
    bummer = (\_ -> "Bummer")
