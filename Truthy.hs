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
)
where

class Truthy a where
  if_ :: a -> (x -> b) -> x -> (y -> b) -> y -> b
  not_ :: a -> Bool

instance Truthy Bool where
  if_ False f r1 g r2 = g r2
  if_ True f r1 g r2 = f r1
  not_ = not

instance Truthy Int where
  if_ 0 = if_ False
  if_ _ = if_ True
  not_ 0 = True
  not_ _ = False
 
instance Truthy Float where
  if_ 0 = if_ False
  if_ _ = if_ True
  not_ 0 = True
  not_ _ = False

instance Truthy Double where
  if_ 0 = if_ False
  if_ _ = if_ True
  not_ 0 = True
  not_ _ = False

instance Truthy Integer where
  if_ 0 = if_ False
  if_ _ = if_ True
  not_ 0 = True
  not_ _ = False

instance Truthy [a] where
  if_ [] = if_ False
  if_ _ = if_ True
  not_ [] = True
  not_ _ = False

instance Truthy (Maybe a) where
  if_ Nothing = if_ False
  if_ _ = if_ True
  not_ Nothing = True
  not_ _ = False

-- Maybe should be false for all none printable characters?
instance Truthy Char where
  if_ c
    | ((ord c) < 32) || ((ord c) == 127) = if_ False
    | otherwise = if_ True
  not_ c
    | ((ord c) < 32) || ((ord c) == 127) = True
    | otherwise = False

-- Purely for readability
then_ :: a -> a
then_ = id
else_ :: a -> a
else_ = id

ifnot_ :: (Truthy a) => a -> (x -> b) -> x -> (y -> b) -> y -> b
ifnot_ = if_ . not_

is_ :: (Truthy a) => a -> Bool
is_ = not_ . not_

and_ :: (Truthy a) => a -> a -> Bool
and_ x y = (&&) (is_ x) (is_ y)

or_ :: (Truthy a) => a -> a -> Bool
or_ x y = (||) (is_ x) (is_ y)

nand_ :: (Truthy a) => a -> a -> Bool
nand_ = not_ . and_

nor_ :: (Truthy a) => a -> a -> Bool
nor_ = not_ . or_

xor_ :: (Truthy a) => a -> a -> Bool
xor_ x y = and_ (or_ x y) (nand_ x y)

iff_ :: (Truthy a) => a -> a -> Bool
iff_ = not_ . xor_

imply_ :: (Truthy a) => a -> a -> Bool
imply_ x y = or_ y (not_ x)

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
case_ t f = (\x -> if_ (t x) then_ (Just . f $ x) else_ Nothing

default_ = id

switch_ :: x -> [x -> Maybe b] -> (b -> b) -> (x -> b) -> b
switch_ inp cases def g =
  case cases of
    [] -> (def . g) inp
    (f:fs) ->
      case (f inp) of
        Nothing -> switch_ inp fs def
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
example x =
  switch_ x mycases default_ bummer
  where
    mycases = breaks_ [(>= 100), (>= 50), (>= 25), (>= 10)] ["Woo hoo!", "Nice!", "Getting there", "Not quite"]
    bummer = (\_ -> "Bummer")
