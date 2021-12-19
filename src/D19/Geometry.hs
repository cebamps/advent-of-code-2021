module D19.Geometry where

import Data.Bifunctor (Bifunctor (first))
import Data.List (delete)

--- Triples of values

type Triple a = (a, a, a)

-- can't be bothered to make a newtype and Applicative for this
map3 :: (a -> b) -> Triple a -> Triple b
map3 f (x, y, z) = (f x, f y, f z)

map3On :: Triple (a -> b) -> a -> Triple b
map3On fs x = map3 ($x) fs

ap3 :: Triple (a -> b) -> Triple a -> Triple b
ap3 (fx, fy, fz) (x, y, z) = (fx x, fy y, fz z)

lift3 :: (a -> b -> c) -> Triple a -> Triple b -> Triple c
lift3 f = ap3 . map3 f

--- Addressing triples

data Axis = X | Y | Z deriving (Eq, Show, Enum, Ord, Bounded)

getAxis :: Axis -> Triple a -> a
getAxis X (x, _, _) = x
getAxis Y (_, y, _) = y
getAxis Z (_, _, z) = z

permute :: Triple Axis -> Triple a -> Triple a
permute p = map3On (map3 getAxis p)

--- Axis algebra

cross :: Axis -> Axis -> (Bool, Axis)
cross x y = let z = cross' x y in (not $ oriented x y z, z)

-- cross product without the sign
cross' :: Axis -> Axis -> Axis
cross' x y | x == y = error "cross received the same axis twice"
cross' x y = head . delete x . delete y $ [X, Y, Z]

oriented :: Axis -> Axis -> Axis -> Bool
oriented x y z | x == y || y == z || z == x = error "got the same axis twice"
oriented X Y Z = True
oriented Y Z X = True
oriented Z X Y = True
oriented _ _ _ = False

--- Rotations

-- Let's keep this simple: we encode a permutation of axes + a flipping of
-- axes. This encoding gets us the full octahedral symmetry group, but we can
-- easily stick to the orientation-preserving subgroup by examining the number
-- of flips.

-- To apply a rotation, flip each axis according to the flip flags then
-- rearrange them as specified in the permutation.
--
-- For example, (1, 2, 3) under (Z, X, Y) (False, True, True) becomes
-- (1, -2, -3) then (-3, 1, -2): -3 goes into X, 1 into Y, -2 into Z.
data Rotation = Rotation {getPermutation :: Triple Axis, getFlip :: Triple Bool} deriving (Eq, Show)

idRotation :: Rotation
idRotation = Rotation (X, Y, Z) (False, False, False)

--- $> let r = Rotation (Z, X, Y) (False, True, True) in rotate r $ (1, 2, 3)

flip3 :: Num a => Triple Bool -> Triple a -> Triple a
flip3 = lift3 flip1
  where
    flip1 :: Num a => Bool -> a -> a
    flip1 False = id
    flip1 True = negate

type OrientedAxis = (Axis, Bool)

-- Takes the forward axis and the up axis, and makes a rotation. The rotation
-- moves the forward axis to +x and the up axis to +z.
makeRotation :: OrientedAxis -> OrientedAxis -> Rotation
makeRotation (x', flipX) (z', flipZ) =
  let (flipY, y') = first not $ cross x' z'
   in Rotation (x', y', z') (flipX, flipY, flipZ)

rotationGroup :: [Rotation]
rotationGroup =
  [ makeRotation fwd up
    | (fwdAxis, upAxis) <- [(X, Z), (Y, Z), (X, Y), (Y, X), (Z, X), (Z, Y)],
      fwdFlip <- [False, True],
      let fwd = (fwdAxis, fwdFlip),
      upFlip <- [False, True],
      let up = (upAxis, upFlip)
  ]

-- $> length rotationGroup == 24

invertPermutation :: Triple Axis -> Triple Axis
invertPermutation = map3On (map3 locate (X, Y, Z))
  where
    locate :: Eq a => a -> Triple a -> Axis
    locate x t | x == getAxis X t = X
    locate x t | x == getAxis Y t = Y
    locate x t | x == getAxis Z t = Z
    locate _ _ = error "element not in triple"

-- that's a semidirect product
invertRotation :: Rotation -> Rotation
invertRotation (Rotation p fs) =
  let p' = invertPermutation p
      fs' = permute p fs
   in Rotation p' fs'

--- $> permute <*> invertPermutation $ (Z, Y, X)

rotate :: Num a => Rotation -> Triple a -> Triple a
rotate r = permute (getPermutation r) . flip3 (getFlip r)

--- $> let r = makeRotation (Y, False) (Z, False) in rotate r $ (1, 0, 0)

--- Translation

add3 :: Num a => Triple a -> Triple a -> Triple a
add3 = lift3 (+)

negate3 :: Num a => Triple a -> Triple a
negate3 = map3 negate

sub3 :: Num a => Triple a -> Triple a -> Triple a
sub3 u v = add3 u (negate3 v)
