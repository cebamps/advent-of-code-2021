{-# OPTIONS_GHC -Wall #-}

module D19.Solution (solve) where

import Control.Monad (forM_)
import D19.Geometry (Rotation, Triple, add3, idRotation, lift3, map3, rotate, rotationGroup, showRotation, sub3)
import Data.Ix (inRange)
import Data.Monoid (First (First, getFirst))
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser)

requiredOverlap :: Int
requiredOverlap = 12

-- $> :m + System.IO.Unsafe

-- $> testInput = unsafePerformIO $ readFile "inputs/d19-test.txt" >>= parseOrFail inputP

solve :: String -> IO ()
solve inputStr = do
  input <- parseOrFail inputP inputStr
  let placements = accumulatePlacedRegions input
  print $ solve1 placements
  print $ solve2 placements
  putStr "\n"
  debugPlacements placements

-- Approach: find a quick way to determine whether and how two regions overlap.
-- We have 35 regions in total in our input, which makes 595 unordered pairs.
-- We won't need to test all pairs, because we can stop when we find one
-- overlap. Each region is guaranteed to overlap with (at least?) one other.
--
-- If we have multiple overlaps for one region, can they be inconsistent? That
-- is, do we have to reject any? It seems not, because the example gives 4
-- scanners and says that since 0 and 1 overlap, they must be paired together.
--
-- I have around two dozen beacons per scanner. An overlap test can fix the
-- relative orientation of the regions (there are 24 of them, not 24 * 24),
-- pick one beacon in region 1, match it against one beacon in region 2 to
-- produce a candidate offset, and test the overlap.
--
-- An overlap test could exploit nice data structures such as R-trees, but I'll
-- be using Sets. The test will check that all nodes in the intersection exist
-- on both sides.

type Beacon = Triple Int

type Shift = Triple Int

type Transformation = (Rotation, Shift)

data ScannerRegion = ScannerRegion {sBeacons :: Beacons, sId :: Int} deriving (Eq, Show)

type Beacons = S.Set Beacon

type Input = [ScannerRegion]

data Placement = Placement
  { pTransformation :: Transformation,
    pRegion :: ScannerRegion,
    pMatch :: Int
  }
  deriving (Eq, Show)

--- part 1

-- Strategy: accumulate "placed" regions into a list. Those regions have their
-- coordinates transformed, but we will also keep track of the transformation
-- just in case.
--
-- We start with the first region considered as placed.
--
-- To place a new region, we test the 24 possible rotations and attempt
-- alignment with each of the placed regions. Under the assumptions of the
-- problem, we don't need to verify that the alignment we found is consistent
-- with the other regions.
--
-- We may not find a match because not all two regions match; if so we move on
-- to the next and try again later.
--
-- We can simply take the union of all sets to get the full picture (in the
-- first region's coordinate system).

firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . foldMap First

-- extracts a transformed element from a list, if we can
pickBy :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
pickBy _ [] = Nothing
pickBy f (x : xs) = case f x of
  Nothing -> case pickBy f xs of
    Nothing -> Nothing
    Just (y, xs') -> Just (y, x : xs')
  Just y -> Just (y, xs)

takeSet :: Ord a => S.Set a -> [(a, S.Set a)]
takeSet s = [(x, S.delete x s) | x <- S.elems s]

-- I could take a more efficient approach by matching against unplaced regions
-- if I can not find a match on placed regions, but then I would have to
-- compose transformations and handle more nested data structures.
accumulatePlacedRegions :: [ScannerRegion] -> [Placement]
accumulatePlacedRegions [] = []
accumulatePlacedRegions (reg : regs) = let start = [Placement idTr reg 0] in go start regs
  where
    go :: [Placement] -> [ScannerRegion] -> [Placement]
    go ps [] = ps
    go ps remainingRegs = case pickBy (pushPlacedRegion ps) remainingRegs of
      Nothing -> error "could not find any region that aligns"
      Just (ps', remainingRegs') -> go ps' remainingRegs'
    idTr :: Transformation
    idTr = (idRotation, (0, 0, 0))

pushPlacedRegion :: [Placement] -> ScannerRegion -> Maybe [Placement]
pushPlacedRegion ps reg = do
  p <- placeRegion ps reg
  return $ ps ++ [p]

placeRegion :: [Placement] -> ScannerRegion -> Maybe Placement
placeRegion refs reg = firstJust $ do
  Placement (_, origin) ref _ <- refs
  return $ matchRegions origin ref reg

-- takes an untransformed region, returns a transformed one + the transformation
matchRegions :: Shift -> ScannerRegion -> ScannerRegion -> Maybe Placement
matchRegions origin ref reg = firstJust $ do
  r <- rotationGroup
  let rotatedBeacons = S.map (rotate r) (sBeacons reg)
  return $ do
    (shift, transformedBeacons) <- matchRotatedRegions origin (sBeacons ref) rotatedBeacons
    return $ Placement (r, shift) reg {sBeacons = transformedBeacons} (sId ref)

-- takes a rotated region, returns a transformed one + the shift
matchRotatedRegions :: Shift -> Beacons -> Beacons -> Maybe (Shift, Beacons)
matchRotatedRegions origin refB regB = firstJust $ do
  -- extract one point from each region.
  -- Nice optimization borrowed from elsewhere after looking for other
  -- solutions online: once we have 11 points left from the reference set,
  -- there's no need to try and match the rest. Indeed, if we find a successful
  -- match, there will be at least 12 points of overlap, which means we would
  -- have tried that pairing of (vRef, vReg) before and succeeded.
  (vRef, refB') <- take (S.size refB - requiredOverlap + 1) $ takeSet refB
  (vReg, regB') <- takeSet regB
  -- Shifting transformation to apply to move vReg onto vRef. The shift is the
  -- vector joining the origin of the reference region to the origin of the
  -- candidate region.
  --
  -- Origin of reference = (origin), in placement frame
  -- Origin of candidate = (0,0,0), in candidate frame
  -- relShift = candidate origin in reference frame
  -- placementShift = candidate origin in placement frame

  let placementShift = sub3 vRef vReg
  let relShift = sub3 placementShift origin
  let regB'Shifted = S.map (add3 placementShift) regB'
  let (rlb, rub) = getIntersection relShift
  let bounds = (add3 origin rlb, add3 origin rub)
  return $
    -- beware: we have taken one point out so the minimum overlap is 11 points,
    -- not 12
    if beaconsMatch (requiredOverlap - 1) bounds refB' regB'Shifted
      then -- vRef is vReg shifted in place
        Just (placementShift, vRef `S.insert` regB'Shifted)
      else Nothing
  where
    -- here's where the magic happens
    beaconsMatch :: Int -> (Triple Int, Triple Int) -> Beacons -> Beacons -> Bool
    beaconsMatch expectedN bounds r1 r2 =
      let filterReg = S.filter (inRange bounds)
          r1f = filterReg r1
          r2f = filterReg r2
       in S.size r1f >= expectedN && r1f == r2f

-- Get the boundaries of the region intersection in the reference coordinates
-- relative to the first region's coordinate system.
--
-- This is simple vector math, but intuitively: if we have no shift to apply on
-- the pair of points that we chose, it means they share the same coordinate
-- system, so the regions overlap perfectly. Otherwise, if we have to shift to
-- the right for example it means the shifted region to the right of the
-- reference, so the left part of the reference region is excluded.
getIntersection :: Shift -> (Triple Int, Triple Int)
getIntersection (sx, sy, sz)
  | abs sx > 2000
      || abs sy > 2000
      || abs sz > 2000 =
    error "no intersection between the regions"
getIntersection s =
  let lb = (-1000, -1000, -1000)
      ub = (1000, 1000, 1000)
      lbs = add3 lb s
      ubs = add3 ub s
   in ( lift3 max lb lbs,
        lift3 min ub ubs
      )

--- $> getIntersection (2000,0,0)

debugPlacements :: [Placement] -> IO ()
debugPlacements sol = do
  -- print sol
  -- let fullMap = S.unions . fmap (sBeacons . pRegion) $ sol
  -- print $ S.toList fullMap

  putStrLn "Placements:"
  forM_ sol $ \pl -> do
    putStr "\n"
    let Placement {pMatch = refId, pTransformation = (rot, shift), pRegion = ScannerRegion {sId = plId}} = pl
    putStrLn $ show plId ++ " matched to " ++ show refId
    putStrLn $ "  Rotation: " ++ showRotation rot
    putStrLn $ "  Shift: " ++ show shift

solve1 :: [Placement] -> Int
solve1 = S.size . S.unions . fmap (sBeacons . pRegion)

--- part 2

manhattan :: Num a => Triple a -> Triple a -> a
manhattan u v = let (x, y, z) = map3 abs (u `sub3` v) in x + y + z

solve2 :: [Placement] -> Int
solve2 placements =
  let scanners = snd . pTransformation <$> placements
   in maximum [manhattan s1 s2 | s1 <- scanners, s2 <- scanners]

--- parsing

inputP :: Parser Input
inputP = sepEndBy scannerP (many1 endOfLine)

scannerP :: Parser ScannerRegion
scannerP = do
  sid <- headerP
  beacons <- S.fromList <$> sepEndBy beaconP endOfLine
  return $ ScannerRegion beacons sid
  where
    headerP = fmap read $ string "--- scanner " *> many1 digit <* string " ---" <* endOfLine

beaconP :: Parser Beacon
beaconP = (,,) <$> signedIntP <*> (char ',' *> signedIntP) <*> (char ',' *> signedIntP)
  where
    signedIntP = read <$> (maybe id (:) <$> optionMaybe (char '-') <*> many1 digit)

parseOrFail :: Parser a -> String -> IO a
parseOrFail parser = either (fail . show) return . parse parser ""
