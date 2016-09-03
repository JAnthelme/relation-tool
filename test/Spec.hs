{-# LANGUAGE ScopedTypeVariables #-}
 
import Test.QuickCheck
import Algebra
import Data.List (nub)
import System.Random (Random(..))

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Time (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Control.Monad (replicateM)

newtype QCRelvar = QCRelvar Relvar deriving Show                                  -- for QuickCheck's relvars generation
newtype QCMatchingRelvar    = QCMatchingRelvar (Relvar, Relvar) deriving Show     -- relvars with same attributes
newtype QCDisjointRelvar    = QCDisjointRelvar (Relvar, Relvar) deriving Show     -- relvars with disjoint attributes
newtype QCOverlappingRelvar = QCOverlappingRelvar (Relvar, Relvar) deriving Show  -- relvars with overlapping attributes, but not disjoint

instance Arbitrary QCRelvar where arbitrary = QCRelvar <$> (sized genRelvar)
instance Arbitrary QCMatchingRelvar where arbitrary = QCMatchingRelvar <$> (sized genMatchingRelvars)
instance Arbitrary QCDisjointRelvar where arbitrary = QCDisjointRelvar <$> (sized genDisjointRelvars)
instance Arbitrary QCOverlappingRelvar where arbitrary = QCOverlappingRelvar <$> (sized genOverlappingRelvars) -- TODO: code genOverlappingRelvars

main :: IO ()
main = do
    let mxsss = 1000 -- increase for more tests.
    putStrLn "\nprop00 - test simple relvar generation"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop00
    putStrLn "\nprop01 - union r r == r"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop01
    putStrLn "\nprop02 - intersect r r == r"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop02
    putStrLn "\nprop03 - card of minus r r == 0"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop03
    putStrLn "\nprop04 - (r1 \\ r2) union (r1 intersect r2) == r1"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop04
    putStrLn "\nprop05 - card r1 + card r2 >= card (r1 `union` r2)"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop05
    putStrLn "\nprop06 - (card r1) `min` (card r2) >= card (r1 `intersection` r2)"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop06
    putStrLn "\nprop07 - card (r1 `times` r2) == (card r1) * (card r2) for disjoint relations"
    quickCheckWith stdArgs { maxSuccess = mxsss } prop07
-- TODO: ADD more tests    
    
-- ======================
-- == properties
-- ======================

-- TODO: add dee and dum tests
-- make sure that relvar function works
prop00 :: QCRelvar -> Bool
prop00 (QCRelvar r) = (card r) >= 0

-- self union is identity
prop01 :: QCRelvar -> Bool
prop01 (QCRelvar r) = union r r == r

-- self intersection is identity
prop02 :: QCRelvar -> Bool
prop02 (QCRelvar r) = intersection r r == r

-- self difference has no rows
prop03 :: QCRelvar -> Bool
prop03 (QCRelvar r) = (card $ minus r r) == 0

-- (r1 \\ r2) union (r1 intersect r2) == r1
prop04 :: QCMatchingRelvar -> Bool
prop04 (QCMatchingRelvar (r1, r2)) = union (minus r1 r2) (intersection r1 r2) == r1

-- card r1 + card r2 >= card r1 `union` r2
prop05 :: QCMatchingRelvar -> Bool
prop05 (QCMatchingRelvar (r1, r2)) = (card r1 + card r2) >= card (r1 `union` r2)

-- (card r1) `min` (card r2) >= card r1 `intersection` r2
prop06 :: QCMatchingRelvar -> Bool
prop06 (QCMatchingRelvar (r1, r2)) = (card r1 `min` card r2) >= card (r1 `intersection` r2)
    
-- card (r1 `times` r2) == (card r1) * (card r2) for disjoint relations
prop07 :: QCDisjointRelvar -> Bool
prop07 (QCDisjointRelvar (r1, r2)) = card (r1 `times` r2) == (card r1) * (card r2)

-- ======================
-- == generators
-- ======================

const_MAXCOL = 5 -- maximum column # generated
const_MAXROW = 5 -- maximum row # generated

-- | single Relvar generator.
-- >>> sample (arbitrary :: Gen QCRelvar)
genRelvar :: Int -> Gen Relvar
genRelvar s = sized $ \s -> do
    coln  <- choose (1, const_MAXCOL)
    lbls  <- suchThat (vectorOf coln (listOf1 (arbitrary :: Gen Char))) (\xs-> not $ hasDups xs)
    tys   <- vectorOf coln (arbitrary :: Gen TypeRep)
    rown  <- choose (0, s `min` const_MAXCOL)
    es    <- genElems $ map genElem tys
    ess   <- replicateM rown $ genElems $ map genElem tys
    return $ relvar lbls tys ess

-- | Matching Relvars generator.
genMatchingRelvars :: Int -> Gen (Relvar, Relvar)
genMatchingRelvars s = sized $ \s -> do
    coln  <- choose (1, const_MAXCOL)
    lbls  <- suchThat (vectorOf coln (listOf1 (arbitrary :: Gen Char))) (\xs-> not $ hasDups xs)
    tys   <- vectorOf coln (arbitrary :: Gen TypeRep)
    rown1 <- choose (0, s `min` const_MAXCOL)
    rown2 <- choose (0, s `min` const_MAXCOL)
    ess1  <- replicateM rown1 $ genElems $ map genElem tys
    ess2  <- replicateM rown2 $ genElems $ map genElem tys
    ess2' <- case (ess1++ess2) of
                []       -> return []
                othewise -> listOf $ elements $ (ess1 ++ ess2) -- garantees that r2 sometime has common rows as r1
    let r1 = relvar lbls tys ess1
        r2 = relvar lbls tys ess2'
    return $ (r1, r2)

-- | Disjoint Relvars generator.
genDisjointRelvars :: Int -> Gen (Relvar, Relvar)
genDisjointRelvars s = sized $ \s -> do
    coln1 <- choose (1, const_MAXCOL)
    coln2 <- choose (1, const_MAXCOL)
    lbls  <- suchThat (vectorOf (coln1+coln2) (listOf1 (arbitrary :: Gen Char))) (\xs-> not $ hasDups xs)
    let lbls1 = take coln1 lbls
        lbls2 = drop coln1 lbls
    tys1  <- vectorOf coln1 (arbitrary :: Gen TypeRep)
    tys2  <- vectorOf coln2 (arbitrary :: Gen TypeRep)    
    rown1 <- choose (0, s `min` const_MAXCOL)
    rown2 <- choose (0, s `min` const_MAXCOL)
    ess1  <- replicateM rown1 $ genElems $ map genElem tys1
    ess2  <- replicateM rown2 $ genElems $ map genElem tys2
    let r1 = relvar lbls1 tys1 ess1
        r2 = relvar lbls2 tys2 ess2
    return $ (r1, r2)

-- TODO: code this.
-- | Overlapping Relvars generator.
genOverlappingRelvars :: Int -> Gen (Relvar, Relvar)
genOverlappingRelvars s = sized $ \s -> do
    return (dum,dum)


-------------------------
-- Elem functions
-------------------------

-- | from a given TypeRep create the corresponding Gen Elem
genElem :: TypeRep -> Gen Elem
genElem t
    | (t == tyB)  = B <$> (arbitrary :: Gen Bool)
    | (t == tyC)  = C <$> choose ('a', 'z')
    | (t == tyS)  = S <$> (arbitrary :: Gen String)
    | (t == tyT)  = T <$> (arbitrary :: Gen Text)
    | (t == tyI)  = I <$> choose (-10, 10)
    | (t == tyJ)  = J <$> choose (-10, 10)
    | (t == tyD)  = D <$> choose (-10.0, 10.0)
    | (t == tyDD) = DD <$> elements [fromGregorian 2016 1 1
                                    ,fromGregorian 2016 6 1
                                    ,fromGregorian 2016 12 1]
    | (t == tyDT) = let time = UTCTime {utctDay = fromGregorian 2016 1 1, utctDayTime = secondsToDiffTime (3600*1)} in
                        DT <$> elements [time
                                        ,time {utctDayTime = secondsToDiffTime (3600*2)}
                                        ,time {utctDayTime = secondsToDiffTime (3600*3)}]
    | otherwise  = elements [Nil]

-- | need to convert a [Gen Elem] into a Gen [Elem]
genElems :: [Gen a] -> Gen [a]
genElems (g:[]) = do
    x <- g
    return [x]
genElems (g:gs) = do
    x  <- g
    xs <- genElems gs
    return ([x] ++ xs)
genElems _ = undefined

-------------------------
-- Text functions
-------------------------
instance Arbitrary Text where
    arbitrary = T.pack <$> (arbitrary :: Gen String)


-------------------------
-- TypeRep functions
-------------------------

-- TODO : add tests with non-basics Elem (eg R r, A a...)
rndtys     = [tyB,tyC,tyS, tyT,tyI,tyJ,tyD,tyDD,tyDT] --removing temporarily tyBS
rndtysrng = map (/(fromIntegral $ length rndtys)) [1.0, 2.0..]

instance Random TypeRep where
  randomR (x,y) g = case (randomR (0,1) g) of
                (r::Double,g') 
                      | r < rndtysrng !! 0 -> (rndtys !! 0, g')
                      | r < rndtysrng !! 1 -> (rndtys !! 1, g')
                      | r < rndtysrng !! 2 -> (rndtys !! 2, g')
                      | r < rndtysrng !! 3 -> (rndtys !! 3, g')
                      | r < rndtysrng !! 4 -> (rndtys !! 4, g')
                      | r < rndtysrng !! 5 -> (rndtys !! 5, g')
                      | r < rndtysrng !! 6 -> (rndtys !! 6, g')
                      | r < rndtysrng !! 7 -> (rndtys !! 7, g')
                      | r < rndtysrng !! 8 -> (rndtys !! 8, g')
                      -- | r < rndtysrng !! 9 -> (rndtys !! 9, g')
                      | otherwise          -> (rndtys !! length rndtys, g')
  random g = case (random g) of 
                (r::Double,g') 
                      | r < rndtysrng !! 0 -> (rndtys !! 0, g')
                      | r < rndtysrng !! 1 -> (rndtys !! 1, g')
                      | r < rndtysrng !! 2 -> (rndtys !! 2, g')
                      | r < rndtysrng !! 3 -> (rndtys !! 3, g')
                      | r < rndtysrng !! 4 -> (rndtys !! 4, g')
                      | r < rndtysrng !! 5 -> (rndtys !! 5, g')
                      | r < rndtysrng !! 6 -> (rndtys !! 6, g')
                      | r < rndtysrng !! 7 -> (rndtys !! 7, g')
                      | r < rndtysrng !! 8 -> (rndtys !! 8, g')
                      -- | r < rndtysrng !! 9 -> (rndtys !! 9, g')
                      | otherwise          -> (rndtys !! length rndtys, g')

instance Arbitrary TypeRep where
    arbitrary     = elements rndtys -- choose (tyB, tyBS)
    
-------------------------
-- Local functions
-------------------------

-- True if duplicates exist.
hasDups :: [String] -> Bool
hasDups xs = length xs > (length $ nub xs)
