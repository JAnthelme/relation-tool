{-|
Module      : Relvar.LabelledSet
Description : Labelled set definitions and operators.
Copyright   : (c) Janthelme, 2016
License     : BDS3
Maintainer  : janthelme@gmail.com
Stability   : experimental
Portability : POSIX

Implementation of convenience operators on \"labelled sets\", 'LSet's, which are a type synonym for @'Set.Set' ('String', a)@,
that is a set of (label, value) pairs. Unicity of labels is required and blank labels are prohibited.

Relations are defined by their 'Relvar.Attributes' and the 'Set.Set' of their 'Relvar.Rows' which are both defined as 'LSet's : 

@type Attributes = LSet 'Typeable.TypeRep'@

@type Row = LSet 'Relvar.Elem'@

-}

{-# LANGUAGE FlexibleInstances #-}

module Relvar.LSet (

    -- * Types
      LSet(..)

    -- * Class
    , LabelOp (..)

    -- * Construction
    -- $SECTION3
    , empty
    , singleton
    , insert
    , delete

    -- * Combine
    , project
    , projectaway
    , common
    , union
 
    -- * Query
    
    -- ** Information
    
    -- $SECTION1
    , extract
    , extractWithLabels
    , extract2
    , values
    -- ** Logical
    -- $SECTION2

    -- * Conversion
    , toList
    , fromList

    -- * Miscellaneous
     , checkLabels
     , compatibleLabels
)
where

import qualified Data.Set as Set (Set, filter, toList, fromList, size, intersection, union, map, insert, empty, singleton)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map (fromList, lookup, findWithDefault)
import Data.Maybe (fromJust)

-- | Generic 'Set.Set' of (label, value) elements.
type LSet a = Set.Set (String, a)

-- | Labels operators typeclass. Any 'LSet' and 'Relvar.Relvar' are part of this class.
-- 
-- For 'Relvar.Relvar's, the operators apply to the 'Relvar.Relvar''s 'Relvar.Attributes' (defined as @'LSet' 'Typeable.TypeRep'@).
class LabelOp a where
    -- | Check that two 'LSet's or two 'Relvar.Relvar's are \"compatible\", e.g. there is no label equality without value equality.
    -- For 'Relvar.Relvar's, @True@ if their 'Relvar.Attributes' are compatible.
    compatible :: (Ord a, Show a) => a -> a -> Bool
    -- | 'LSet's equality. For 'Relvar.Relvar's, @True@ if the 'Relvar.Attributes' of two relations are equal.
    match      :: Eq a => a -> a -> Bool
    -- | @True@ if 'LSet's are 'compatible' and 'disjoint'.
    disjoint   :: Ord a => a -> a -> Bool
    -- | Replace 'LSet's' labels. For 'Relvar.Relvar's, rename its 'Relvar.Attributes''s labels.
    rename      :: a -> [String] -> [String] -> a
    -- | Return 'LSet''s labels. For 'Relvar.Relvar's, return its 'Relvar.Attributes''s labels.
    labels :: a -> [String]
    -- | Return the number of elements in the set. For 'Relvar.Relvar's, return its 'Relvar.Attributes''s size (e.g. its number of columns).
    degree     :: a -> Int

-- implementation for LSet a
instance (Ord a, Show a) => LabelOp (LSet a) where
    compatible lset1 lset2
        | incompatibleSet == [] = True
        | otherwise             = error $ errIncompatiblePair incompatibleSet
          where uset = union lset1 lset2
                lbls = labels uset
                incompatibleSet = nub $ filter ((>1) . Set.size) $ map (\ lbl -> Set.filter ((==lbl) . fst) uset) lbls
    match = (==)
    disjoint lset1 lset2 
        | compatible lset1 lset2 = Set.size (Set.intersection lset1 lset2) == 0
        | otherwise              = False
    rename lset olds news
        | hasDups olds        = error errDupLbl
        | hasDups news        = error errDupLbl  
        | hasDups newlbls     = error errDupLbl
        | otherwise           = renamed
          where mp      = Map.fromList $ zip olds news
                rplc    = \x -> Map.findWithDefault x x mp
                newlbls = map rplc $ labels lset
                renamed = Set.map (\(l,e) -> (rplc l, e)) $ lset
    labels xs = map fst $ toList xs
    degree lset = Set.size lset

-- implementation for [String]
instance LabelOp ([String]) where
    compatible l1 l2 = True -- compatible does not really make sense for [String] types (either two lists match or not)
    match lbls1 lbls2 = sort lbls1 == sort lbls2    
    disjoint ls1 ls2 = undefined -- Set.size (Set.intersection lset1 lset2) == 0
    rename lbls olds news
        | hasDups olds        = error errDupLbl
        | hasDups news        = error errDupLbl  
        | hasDups newlbls     = error errDupLbl
        | otherwise           = newlbls
          where mp      = Map.fromList $ zip olds news
                rplc    = \x -> Map.findWithDefault x x mp
                newlbls = map rplc lbls    
    labels = id
    degree = length
    
-- ======================
-- == Construction
-- ======================
-- $SECTION3 For completeness, but it might be simpler to use the conversion functions (e.g. 'fromList').
-- Also see 'LabelOp' class instance function 'rename'.

-- | Empty set
empty :: LSet a
empty = Set.empty

-- | Singleton set from a label and its corresponding element.
singleton :: String -> a -> LSet a
singleton lbl val = Set.singleton (lbl, val)

-- | Insert a label and corresponding value in an LSet. -- CHECK : 1) that the label does not exist already 2) that the label is not blank
insert :: Ord a => String -> a -> LSet a -> LSet a
insert lbl val lset = Set.insert (lbl, val) lset

-- | Delete an element in an LSet.
delete :: String -> LSet a -> LSet a
delete lbl lset = Set.filter ((/=lbl) . fst) lset

-- ======================
-- == Conversion
-- ======================

-- | Convert the set to a list of (label,value) pairs.
toList :: LSet a -> [(String,a)]
toList = Set.toList

-- | Create a LSet from a list of (label,value) pairs.
fromList' :: Ord a => [(String,a)] -> LSet a
fromList' xs
    | hasBlankLbl xs = error errBlankLbl
    | hasLblDups xs  = error errDupLbl
    | otherwise      = Set.fromList xs

-- | Create a LSet from a list of labels and a corresponding list of values.
fromList :: Ord a => [String] -> [a] -> LSet a
fromList lbls es = fromList' $ zip lbls es

-- ======================
-- == Combine
-- ======================

-- | Return a LSet of the input LSet elements which labels are included in the provided list.
project :: LSet a -> [String] -> LSet a
project lset lbls = Set.filter ((flip elem lbls) . fst) lset  -- CHECK compatible labels?

-- | Return a LSet of the input LSet elements which labels are not included in the provided list.
projectaway :: (Ord a, Show a) => LSet a -> [String] -> LSet a
projectaway lset lbls = project lset lbls'              -- CHECK compatible
    where lbls' = filter (\ l -> not $ elem l lbls) $ labels lset

-- | Return common labels between the two LSets.
common :: Ord a => LSet a -> LSet a -> LSet a
common lset1 lset2    = Set.intersection lset1 lset2 -- CHECK compatible?

-- | Union of the two LSets.
union :: Ord a => LSet a -> LSet a -> LSet a
union lset1 lset2     = Set.union lset1 lset2 -- CHECK compatible?

-- ======================
-- == Query
-- ======================

-- $SECTION1 Also see 'LabelOp' class instance functions 'labels' and 'degree'.

-- | extract values provided labels. Preserve input labels order.
extract :: (Ord a, Show a) => Bool -> LSet a -> [String] -> [a]
extract chckDup lset lbls = case compatibleLabels chckDup lset lbls of
    False -> undefined -- will error out via compatibleLabels
    True  -> map (fromJust . (flip Map.lookup m)) lbls
              where m = Map.fromList $ Set.toList lset

-- | extract (label, value) provided labels. Preserve input labels order.
extractWithLabels :: (Ord a, Show a) => Bool -> [String] -> LSet a -> [(String, a)]
extractWithLabels chckDup lbls lset = zip lbls $ extract chckDup lset lbls

extract2 :: Ord a => [String] -> [(String, a)] -> [(String, a)]
extract2 lbls xs = zip lbls $ map (fromJust . (flip Map.lookup m)) lbls
    where m = Map.fromList $ xs
              


-- | Return values of a given LSet. Note that the order of the list is determined by the internal 'Set.Set' representation of the LSet input.
values :: LSet a -> [a]
values xs = map snd $ toList xs

-- $SECTION2 See 'LabelOp' class instance functions 'match', 'disjoint' and 'compatible'.

-- ======================
-- == Misc
-- ======================

-- True if label duplicates exist (values do not matter).
hasLblDups :: [(String,a)] -> Bool
hasLblDups xs = hasDups $ map fst xs

-- True if label duplicates exist (values do not matter).
hasLblDups' :: LSet a -> Bool
hasLblDups' = hasLblDups . Set.toList

-- True if duplicates exist.
hasDups :: [String] -> Bool
hasDups xs = length xs > (length $ nub xs)

-- True if a blank labels are found.
hasBlankLbl :: [(String,a)] -> Bool
hasBlankLbl xs = elem "" $ map fst xs

-- True if a blank strins are found.
hasBlnk :: [String] -> Bool
hasBlnk xs = elem "" xs

-- | Return @True@ if no blank labels and, if chckDup flag is @True@, no duplicate labels are found.
checkLabels :: Bool -> [String] -> Bool
checkLabels chckDup lbls
    | hasBlnk lbls            = error errBlankLbl
    | chckDup && hasDups lbls = error errDupLbl
    | otherwise               = True

-- | Return @True@ if all labels are included in the provided LSet and if 'checkLabels' is also @True@.
-- 
-- Similar to 'compatible' but tests the compatibility of LSet's labels vs the provided list of labels.
compatibleLabels :: (Ord a, Show a) => Bool -> LSet a -> [String] -> Bool
compatibleLabels chckDup lset lbls 
    | lbls == []                                                            = error errEmptySet
    | not $ checkLabels chckDup lbls                                        = False
    | length (filter (==False) $ map (\s -> elem s $ labels lset) lbls) > 0 = error errLabelMismatch
    | otherwise = True


-- ======================
-- == Error messages
-- ======================

errBlankLbl :: String
errBlankLbl = "Blank string are not allowed as label name."

errDupLbl :: String
errDupLbl = "Duplicate label names are not allowed."

errInvalidRename :: String
errInvalidRename = "Invalid renaming (check for duplicates)."

errIncompatiblePair :: Show a => a -> String
errIncompatiblePair xs = "incompatible (labels, value) pairs:\n" ++ show xs ++ "."

errEmptySet :: String
errEmptySet = "Empty list of labels is not allowed."

errLabelMismatch :: String
errLabelMismatch = "Some labels aren't included in the set"