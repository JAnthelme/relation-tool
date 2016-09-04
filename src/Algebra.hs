{-|
Module      : Algebra
Description : Relational algebra operators
Copyright   : (c) Janthelme, 2016
License     : BDS3
Maintainer  : janthelme@gmail.com
Stability   : experimental
Portability : POSIX

Implementation of relational algebra and other operators over 'Relvar's as defined in C.J. Date, \"/An Introduction to Database Systems/\" Eighth Edition, chapter 7 (\'Relational Algebra\') .

-}

{-# LANGUAGE FlexibleInstances #-}

module Algebra
( module Relvar,
  -- module Attribute,
  -- * Types
  OpElem (..)
  -- * Algebra operators
  , union
  , intersection  
  , minus  
  , times
  , restrict
  , project
  , projectaway
  , join
  -- * Other operators
  , semiJoin
  , semiMinus  
  , extend
  , summarize
  , group
  , ungroup

) where

-- TO DO :  clean code. put module the right way. stack?

import Relvar
import qualified Relvar.LSet as LSet hiding (LSet)
import Relvar.LSet (LSet)

-- import Data.Typeable (TypeRep)

import qualified Data.Set as Set (Set, union, intersection, difference, filter, map, empty, member, foldl, elemAt, fromList, toList, partition, singleton)
import Data.List as L (sort, transpose)

-- import Control.Applicative ((<*>), ZipList)


-- | Summarize operators.
data OpElem = Sum    
            | Count  -- ^ Count elements. 
            | Avg    -- ^ Arithmetic Average.
            | Max    -- ^ Return the maximum element.
            | Min    -- ^ Return the minimum element.
            | Sdev   -- ^ Standard deviation.
            | Var    -- ^ Variance.
            deriving (Eq, Show)

-- ======================
-- == Algebra (see C.J. Date)
-- ======================

-- | Union operator.
-- The two relations' attributes must be equal.
union :: Relvar -> Relvar -> Relvar
union r1 r2
    | r1 `match` r2 = r1 {rdata = Set.union (rdata r1) (rdata r2)}
    | otherwise     = error $ errDiffAttr r1 r2

-- | Intersection operator
-- The two relations' attributes must be equal (see 'Relvar.match').
intersection :: Relvar -> Relvar -> Relvar
intersection r1 r2
    | r1 `match` r2 = r1 {rdata = Set.intersection (rdata r1) (rdata r2)}
    | otherwise     = error $ errDiffAttr r1 r2

-- | Difference operator.
-- The two relations' attributes must be equal (see 'Relvar.match').
minus :: Relvar -> Relvar -> Relvar
minus r1 r2
    | r1 `match` r2 = r1 {rdata = Set.difference (rdata r1) (rdata r2)}
    | otherwise     = error $ errDiffAttr r1 r2

-- | Product operator.
-- The two relations' attributes must be disjoint.
times :: Relvar -> Relvar -> Relvar
times r1 r2 
    | disjoint r1 r2 = Relvar {attributes = prodattr, rdata = proddata}
    | otherwise      = error $ errCommonAttr r1 r2
     where prodattr = LSet.union (attributes r1) (attributes r2)
           proddata = flattenSet $ Set.map (\rw -> Set.map (Set.union rw) $ rdata r2) $ rdata r1


-- | Filter a relation's rows given a (Row -> Bool) function
restrict' :: Relvar -> (Row -> Bool) -> Relvar
restrict' r f = r {rdata= Set.filter f (rdata r)}

-- | Filter a relation's rows given a ([Elem] -> Bool) function and a list of attribute names.
restrict :: Relvar -> ([Elem] -> Bool) -> [String] -> Relvar
restrict r f nms = restrict' r (f . (\ls -> LSet.extract True ls nms))

-- CHECK project on missing attributes = DUM?
-- | Projection on a given set of attributes.
-- Only attributes with labels in the provided list are included.
project :: Relvar -> [String] -> Relvar
project r lbls = r {attributes = projattr, rdata = projdata}
    where projattr = LSet.project (attributes r) lbls
          projdata = Set.map (flip LSet.project lbls) $ rdata r

-- | Projection on a given set of attributes. 
-- Attributes with labels matching the list are excluded.
projectaway :: Relvar -> [String] -> Relvar
projectaway r lbls = project r $ LSet.labels $ LSet.projectaway (attributes r) lbls

-- | Natural join operator.
-- The two relations' attributes must not be disjoint.
-- The resulting 
join :: Relvar -> Relvar -> Relvar
join r1 r2
    | disjoint r1 r2 = times r1 r2
    | r1 `match` r2  = intersection r1 r2 
    | otherwise      = Relvar {attributes = joinattr, rdata = joindata}
       where cmnlbls  = LSet.labels $ common r1 r2 -- common labels (type-checked)
             joinattr = LSet.union (attributes r1) (attributes r2)
             joindata = flattenSet $ Set.map f $ rdata r1
             f rwa    = Set.map (Set.union rwa) $ rdata $ restrict2 r2 rwa cmnlbls
             

-- ======================
-- == Additional Operators (see C.J. Date)
-- ======================

-- | Similar to the join operator projected over the attributes of the first relation.
-- Loosely speaking the results of semiJoin r1 r2 are the rows of r1 which have a counterpart in r2.
-- 
-- prop> r1 `semiJoin` r2 == project (r1 `join` r2) (labels r1)
semiJoin :: Relvar -> Relvar -> Relvar
semiJoin r1 r2 
    | disjoint r1 r2 = r1 {rdata = Set.empty}
    | r1 `match` r2  = intersection r1 r2
    | otherwise      = restrict' r1 f
       where cmnlbls  = LSet.labels $ common r1 r2 -- common labels (type-checked)
             target = rdata $ project r2 cmnlbls
             f rw   = Set.member (LSet.project rw cmnlbls) target

-- | Similar to the minus operator projected over the attributes of the first relation.
-- Loosely speaking the results of semiMinus r1 r2 are the rows of r1 which do not have a counterpart in r2.
-- 
-- prop> r1 `semiMinus` r2 == project (r1 `minus` r2) (labels r1)
-- prop> r1 `semiMinus` r2 == r1 `minus` (r1 `semiJoin` r2)
semiMinus :: Relvar -> Relvar -> Relvar
semiMinus r1 r2 = minus r1 $ semiJoin r1 r2

-- | Add a derived column to an existing relation. 
-- Each element in the new column is the result of a computation of some other columns.
extend :: Relvar -> ([Elem] -> Elem) -> [String] -> String -> Relvar -- CHECK: can extend to several columns?
extend r f inlbls outlbl = Relvar {attributes = LSet.insert outlbl outty $ attributes r, rdata = Set.map (extendRowWith inlbls outlbl f) $ rdata r}
    where args0 = LSet.extract True (Set.elemAt 0 $ rdata r) inlbls -- arguments corresponding to the 1st row
          outty = typeRep $ f args0                             -- this only works for basic types and relations. CHECK: can we do better?              
-- FIXME: bug when the lbls do not contain a valid col names. (in fact when project is on invalid column... FIX project)
-- | @summarize r per ops inlbls outlbls@ computes ops over inlbls columns into outlbls column.
summarize :: Relvar -> Relvar -> [OpElem] -> [String] -> [String] -> Relvar
summarize r per ops lbls lbls' = foldl1 union rs
    where (rws, rels1) = catalog r per $ compute ops lbls lbls' -- structure : ([per's row], [computation for corresponding row])
          rels2 = map (\rw -> per {rdata = Set.fromList [rw]}) rws 
          rs = zipWith times rels2 rels1          
-- CHECK can per and lbls have common labels?
-- CHECK are all compatibility checks done in compute?

-- CHECK improve doc explanations... :(
-- | @group r grplbls reslbl@ returns a relation with all of r's attributes, excluding grplbls columns, plus reslbl column. 
-- The elements in reslbl column are of type 'Relvar' and the result of grouping of grplbls columns over the non-grplbls of r.
-- 
-- prop> degree (group r grplbls reslbl) == degree r - (length grplbls) + 1
group :: Relvar -> [String] -> String -> Relvar
group r grplbls reslbl = Relvar {attributes = LSet.fromList ([reslbl] ++ labels per) ([tyR] ++ types per), rdata = Set.fromList ess}
    where per = projectaway r grplbls
          (rws, grprels) = catalog r per (flip project grplbls)  -- structure : ([per's row], [projection of r on grplbls for corresponding row])
          ess = zipWith (\r rw -> LSet.insert reslbl (R r) rw) grprels rws

-- | 'group' inverse operator.
ungroup :: Relvar -> String -> Relvar
ungroup r reslbl
    | not $ elem tyR (types' r False [reslbl]) = error $ errNonRelField reslbl
    | otherwise = foldl1 union $ zipWith times grprels nongrprels
        where -- rws = Set.toList $ rdata r
              (grprows, nongrprows) = unzip $ Set.toList $ Set.map (Set.partition ((==reslbl) . fst)) $ rdata r -- structure: [rows of reslbl] and [rows of nongrouplbls]
              grprels    = map (unboxRel . head . LSet.values) grprows -- structure: [Relvar]
              nongrprels = map (\rw-> Relvar {attributes= LSet.delete reslbl $ attributes r, rdata = Set.singleton rw}) nongrprows
              
              
              
-- CHECK reslbl is part of r on implictly done?

-- ======================
-- == Local functions
-- ======================
flattenSet :: Ord a => Set.Set (Set.Set a) -> Set.Set a
flattenSet s = Set.foldl Set.union Set.empty s

-- | restrict rows which match a given target sub-row, over specified labels
restrict2 :: Relvar -> Row -> [String] -> Relvar
restrict2 r target lbls = restrict' r (\rw -> match (LSet.project rw lbls) (LSet.project target lbls))

-- extendRow :: (String, Elem) -> Row -> Row
-- extendRow e rw = Set.union rw $ Set.singleton e
    
extendRowWith :: [String] -> String -> ([Elem] -> Elem) -> Row -> Row
extendRowWith inlbls outlbl f rw = LSet.insert outlbl (f args) rw
    where args = LSet.extract True rw inlbls


-- | compute ops over lbls into lbls'
compute :: [OpElem] -> [String] -> [String] -> Relvar -> Relvar
compute ops lbls lbls' r
    | not $ LSet.compatibleLabels False (attributes r) lbls        = undefined
    | not $ LSet.checkLabels True lbls'                            = undefined
    | (length lbls /= length ops) || (length lbls /= length lbls') = error errSizeMismatch
    | otherwise                                                    = relvar lbls' tys' ess'
        where tys  = LSet.extract False (attributes r) lbls
              tys' = summaryTy ops tys
              ess  = elems' r False lbls
              ess' = transpose $ map (:[]) $ zipWith ($) (map summaryFn ops) $ transpose ess
            
-- | fst is the list of all (semiJoin r per) rows and snd is the result of f applied to each corresponding row
catalog :: Relvar -> Relvar -> (Relvar -> a) -> ([Row], [a])
catalog r per f
    | False = undefined
    | otherwise    = (rws, map (f . flter) rws)
    where rws      = Set.toList (rdata $ semiJoin per r)
          flter    = \ rw -> restrict2 r rw $ labels per
          
-- | Types for the summary columns
summaryTy  :: [OpElem] -> [TypeRep] -> [TypeRep]
summaryTy ops tys = map (\(t,o) -> if o==Count then tyI else t) $ zip tys ops

-- | Summary function given an OpElem
summaryFn :: OpElem -> ([Elem] -> Elem)
summaryFn ops = case ops of 
    Count -> countElem
    Sum   -> sumElem
    Avg   -> avgElem
    Min   -> minElem
    Max   -> maxElem
    Sdev  -> sdevElem
    Var   -> varElem

countElem :: [Elem] -> Elem 
countElem es = I (length es)

sumElem' :: Elem -> Elem -> Elem
sumElem' (I x) (I y) = I (x+y)
sumElem' (J x) (J y) = J (x+y)
sumElem' (D x) (D y) = D (x+y)
sumElem' _ _ = Nil

sumElem :: [Elem] -> Elem
sumElem = foldl1 sumElem'

avgElem :: [Elem] -> Elem
avgElem es = let s = sumElem es in case s of
    I x -> I (x `div` (length es))
    J x -> J (x `div` (fromIntegral $ length es))
    D x -> D (x / (fromIntegral $ length es))
    _   -> Nil

maxElem :: [Elem] -> Elem
maxElem = maximum 

minElem :: [Elem] -> Elem
minElem = minimum

varElem :: [Elem] -> Elem
varElem = nilElem -- CHECK - TBI

sdevElem :: [Elem] -> Elem
sdevElem = nilElem -- CHECK - TBI

nilElem :: [Elem] -> Elem
nilElem = const Nil

-- unbox Relvar elements (ugly?)
unboxRel :: Elem -> Relvar
unboxRel (R x) = x
unboxRel _ = dum


-- ======================
-- == Error messages
-- ======================

errDiffAttr :: Relvar -> Relvar -> String
errDiffAttr r1 r2 = "Relations do not have the same attributes : " ++ show (L.sort $ LSet.toList $ attributes r1) ++ " vs " ++ show (L.sort $ LSet.toList $ attributes r2)

errCommonAttr :: Relvar -> Relvar -> String
errCommonAttr r1 r2 = "Relations share common attributes : " ++ show (common r1 r2)

errSizeMismatch :: String
errSizeMismatch = "Size mismatch"

errNonRelField :: String -> String
errNonRelField lbl = "The type associated with the label " ++ lbl ++ " is not Relation"
