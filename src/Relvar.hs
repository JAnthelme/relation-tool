{-|
Module      : Relvar
Description : Basic types and definitions
Copyright   : (c) Janthelme, 2016
License     : BDS3
Maintainer  : janthelme@gmail.com
Stability   : experimental
Portability : POSIX

A simple representation of relation types based on C.J. Date, \"/An Introduction to Database Systems/\" Eighth Edition.

The representation of 'Relvar' is based on 'Set.Set's, where each relation tuple is of the type 'Row', or a synonym for @'Set.Set' ('String', 'Elem')@. 'Elem' is an Algebraic Data Type representing each tuple's element. 
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- A simple and lightweight relational algebra operators library
-- based on C.J. Date "An Introduction to Database Systems" Eight Edition
module Relvar (
    
    -- * Re-export
    TypeRep (..)
    
    -- * Types
    ,  Elem (..)
    , Attributes
    , Row (..)
    , Relvar(..)
    , Table(..)
    , ColOrder(..)
    
    -- * Classes
    , Elementable(..)
    -- * Construction
    , relvar
    , dee
    , dum
    , rename    
    -- * Query
    
    -- * Information
    , degree
    , card
    , elems
    , elems'
    , labels
    , types
    , types'
    , common
    
    -- * Logical
    , compatible
    , match
    , disjoint
    
    -- * Table
    , Relvar.table
    
    -- * Label functions
    , keepLeft
    , keepRight
    
    -- * Utility functions
    , typeRep
    , tyB,tyC,tyS,tyT,tyI,tyJ,tyD,tyDD,tyDT,tyBS,tyR,tyA,tyT2,tyT3,tyZ


)
where

import qualified Relvar.LSet as LSet hiding (LSet, degree, match, compatible, disjoint, rename, labels)
import Relvar.LSet (LSet, degree, match, compatible, disjoint, rename, labels)

import qualified Data.Set as Set (Set, empty, singleton, fromList, toList, size, difference, filter, map)
import Data.Typeable (Typeable, TypeRep, typeOf)

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.ByteString (ByteString)

import Data.List (nub, sortOn, sort)

import Data.Ord (Down(..), comparing)

import Data.Map as Map (fromList, lookup,findWithDefault)
import Data.Maybe (fromMaybe)

-- import Debug.Trace (trace)

-- FIXME : isNil :: E -> Bool, Lazy version?


-- ======================
-- == Types
-- ======================

-- | ADT for selecting sorting in ascending or descending orders.
data ColOrder = Asc | Desc deriving (Eq, Show, Ord, Read)

-- | Type for 'Relvar's' elements.
-- Elem types are either primitive types or Relvars or array of Elems or tuple of Elems (2 and 3 arguments).
-- One can extend the primitive types by adding to the code's definitions (look for @\"-- ADD NEW TYPES HERE\"@ comments in code).
data Elem = 
          Nil                     
          -- primitive types (ADD NEW TYPES HERE)
          | B Bool | C Char | S String | T Text | I Int | J Integer | D Double | DD Day | DT UTCTime | BS ByteString         
          | A [Elem]              -- ^ array of Elems
          | T2 (Elem, Elem)       -- ^ pair of Elems
          | T3 (Elem, Elem, Elem) -- ^ triplet of Elems
          | R Relvar              -- ^ relation
          | DW (Down Elem)        -- ^ convenient for ordering Elems in descending order
          deriving (Eq, Ord, Typeable)
-- FIXME: possiblity of adding Enums, adding other Algebraic Data Type?
-- FIXME: change to Dynamic types

instance Show Elem where
    show Nil    = "-nil-"
    show (B x)  = show x
    show (C x)  = show x
    show (S x)  = show x
    show (T x)  = show x
    show (I x)  = show x
    show (J x)  = show x
    show (D x)  = show x
    show (DD x) = show x
    show (DT x) = show x
    show (BS x) = show x
    -- (ADD NEW TYPES HERE)
    show (A x)  = show x
    show (T2 x) = show x
    show (T3 x) = show x
    show (R r)  = show r
    show (DW x)  = show x    

-- | Class of any type @a@ which can be reprensented as an Elem.
class Elementable a where 
    -- | Conversion from types to their corresponding Elem ADT type.
    toElem :: a -> Elem
    fromElem :: Elem -> Maybe a

instance Elementable Bool 
    where toElem x = B x
          fromElem (B x) = Just x
          fromElem _ = Nothing
    
instance Elementable Char
    where toElem x = C x
          fromElem (C x) = Just x
          fromElem _ = Nothing
instance Elementable String
    where toElem x = S x
          fromElem (S x) = Just x
          fromElem _ = Nothing
instance Elementable Text
    where toElem x = T x
          fromElem (T x) = Just x
          fromElem _ = Nothing
instance Elementable Int
    where toElem x = I x
          fromElem (I x) = Just x
          fromElem _ = Nothing
instance Elementable Integer
    where toElem x = J x
          fromElem (J x) = Just x
          fromElem _ = Nothing
instance Elementable Double
    where toElem x = D x
          fromElem (D x) = Just x
          fromElem _ = Nothing
instance Elementable Day
    where toElem x = DD x
          fromElem (DD x) = Just x
          fromElem _ = Nothing
instance Elementable UTCTime
    where toElem x = DT x
          fromElem (DT x) = Just x
          fromElem _ = Nothing
instance Elementable ByteString
    where toElem x = BS x
          fromElem (BS x) = Just x
          fromElem _ = Nothing
-- (ADD NEW TYPES HERE)
instance Elementable [Elem] 
    where toElem x = A x
          fromElem (A x) = Just x
          fromElem _ = Nothing -- = Just [] -- alternative?
instance Elementable (Elem, Elem)
    where toElem x = T2 x
          fromElem (T2 x) = Just x
          fromElem _ = Nothing
instance Elementable (Elem, Elem, Elem)
    where toElem x = T3 x
          fromElem (T3 x) = Just x
          fromElem _ = Nothing
instance Elementable Relvar 
    where toElem x = R x
          fromElem (R x) = Just x
          fromElem _ = Nothing
instance Elementable (Down Elem)
    where toElem x = DW x
          fromElem (DW x) = Just x
          fromElem _ = Nothing

-- | Row is synonym for @'LSet' 'Elem'@, i.e. for @'Set' (String, Elem)@.
-- The LSet type enforces that there are no blank label, nor any duplicate label.
type Row = LSet Elem

-- | Attributes is the type synonym for @'LSet' 'TypeRep'@, i.e. for @'Set' (String, TypeRep)@.
-- The LSet type enforces that there are no blank label, nor any duplicate labels.
type Attributes = LSet TypeRep

-- | Relvar is the type for relations.
-- A relation is defined by the set of its attributes (i.e. labels' names and types) and of its tuples, or rows, (i.e. sets of Elems). 
-- There is unicity of Attributes labels and no two rows can be identical.
-- 
-- See C.J. Date, \"/An Introduction to Database Systems/\" Eighth Edition, chapter 6 (\'Relations\').
data Relvar = Relvar { attributes :: Attributes -- ^ Set of the relation's attributes.
                     , rdata :: Set.Set Row     -- ^ Set of the relation's tuples (i.e. rows).  
                     }    
                     deriving (Show, Typeable) -- FIXME need Read?

-- | Type for tables. A relation's table output is mostly for representation.
-- Unlike relations, tables can have duplicate rows and column headers. 
-- See full difference between relations and tables in C.J. Date, \"/An Introduction to Database Systems/\" Eighth Edition, chapter 6 (\'Relations\').
data Table = Table { header :: [String], tdata :: [[Elem]]} deriving (Show, Eq, Ord)

-------------------------
-- relVar instances
-------------------------

instance Eq Relvar where
    r1 == r2
        | attributes r1 /= attributes r2  = False
        | rows' r1 /= rows' r2            = False
        | otherwise                       = null $ Set.difference (rdata r1) (rdata r2)

-- arbitrary, but simple, Ord instance implementation
instance Ord Relvar where
    compare r1 r2 = LT --FIXME TBI

-- | On 'Relvar's, the operators apply on the relation's 'Attributes'.
instance LSet.LabelOp Relvar where
    -- True when the attributes of two relations are compatible (see LSet.compatible). 
    compatible r1 r2 = compatible (attributes r1) (attributes r2)
    -- True when the attributes of two relations are identical. 
    match r1 r2      = match (attributes r1) (attributes r2)
    -- True is two relations do not share any attribute labels (irrespective of their type). 
    disjoint r1 r2   = disjoint (attributes r1) (attributes r2)
    -- Rename a relation's attributes labels from old to new values. No duplicate are allowed.
    rename r olds news = r { attributes = rename (attributes r) olds news
                           , rdata = Set.map (\rw -> rename rw olds news) $ rdata r}
    -- Return a relation's attributes labels.
    labels r        = labels $ attributes r 
    -- Number of attributes (i.e. \"columns\") in a relation
    degree r        = degree $ attributes r

-- ======================
-- == Construction
-- ======================

-- | Create a relation variable provided its 'Relvar.Attributes'' labels and types (i.e. column names and types) and its 'Row' elements.
relvar :: [String] -> [TypeRep] -> [[Elem]] -> Relvar
relvar lbls tys ess = case LSet.checkLabels True lbls of 
    True  -> Relvar {attributes = LSet.fromList lbls tys, rdata = Set.fromList $ map (LSet.fromList lbls) ess}
    False -> dum -- this case should never happen (checkLabels either produces either error or True)
-- WARNING: FIXME : does not check that the elements are of the right type for each row    

-- | Constant relation with an empty set of attributes and 1-tuple (equal to the empty set) body ([C.J. Date] chap. 6).
-- 
-- @dee = Relvar {attributes= Set.empty, rdata = Set.singleton $ Set.empty}@.
dee = Relvar {attributes = LSet.fromList [] [], rdata = Set.singleton $ Set.empty}

-- | Constant relation with an empty set of attributes and no-tuple body ([C.J. Date] chap. 6).
--
-- @dum = Relvar {attributes= Set.empty, rdata = Set.empty}@.
dum = Relvar {attributes = LSet.fromList [] [], rdata = Set.empty}


-- ======================
-- == Query
-- ======================

-- | Cardinality of a relation (i.e. \# of tuples or \# of rows).
card :: Relvar -> Int
card r = Set.size $ rdata r

-- | Common attributes
common :: Relvar -> Relvar -> Attributes
common r1 r2 = LSet.common (attributes r1) (attributes r2)

-- | Extract all elements. 
elems :: Relvar -> [[(String, Elem)]]
elems r = map (Set.toList) $ Set.toList $ rdata r
--FIXME with labels and values in LabelledSet

-- | Extract elements given labels.
elems' :: Relvar -> Bool->  [String] -> [[ Elem]]
elems' r chckDup lbls = map (\ls -> LSet.extract chckDup ls lbls) $ Set.toList $ rdata r
--FIXME with labels and values in LabelledSet

-- | Return the all Attributes' types.
types :: Relvar -> [TypeRep]
types r = LSet.values $ attributes r

-- | Return Attributes' types for the matching labels.
types' :: Relvar -> Bool -> [String] -> [TypeRep]
types' r chckDup lbls = LSet.extract chckDup (attributes r) lbls

-- ======================
-- == table functions
-- ======================

-- FIXME labels compatible? not blank? or leave it?
-- FIXME project away labels?
-- | Output a table given a set of labels, a row indexing function 
-- and a list of row indices. 
--
-- If lbls is Nothing all relation labels are included (in default order). 
-- If the row indices argument is nothing, all rows are included.
table :: Relvar -> Maybe [String] -> Maybe [(String,ColOrder)] -> Maybe [Int]-> Table
table r lbls ordrw rwIdxs = Table {header = fromMaybe (labels r) lbls, tdata = els3} 
    where -- type [Row]
          els0 = Set.toList $ rdata r
          -- restrict on subset of labels (preserving lbls order)
          els1 = case lbls of
            Nothing    -> map Set.toList els0  -- type [[(String,Elem)]]
            Just lbls' -> map (LSet.extractWithLabels True lbls') els0    
          -- sorting rows on ordrw
          els2 = case ordrw of
            Nothing     -> map (map snd) els1
            Just ordrw' -> map (map snd) $ let (ordlbls, ordCO) = unzip ordrw' in sortOn (orderLabels ordlbls ordCO) els1
          -- take only rwIdxs row indices
          els3 = case rwIdxs of
            Nothing      -> els2
            Just rwIdxs' -> let rwIdxs'' = sort rwIdxs' in map snd $ filter ((flip elem rwIdxs') . fst) $ zip [0..] els2 
          
-- | Output a filtered [(String, Elem)] where only ordlbls labels are kept 
-- and Elems are potentially converted to (Down Elem) depending on ordCOs, 
-- which is then used to sort on.
orderLabels :: [String] -> [ColOrder] -> [(String, Elem)] -> [(String, Elem)]
orderLabels ordlbls ordCOs xs = ys 
    where mp = Map.fromList $ zip ordlbls ordCOs
          xs' = filter ((flip elem ordlbls) . fst) xs
          ys = map (\(x,y)-> (x, rev (Map.findWithDefault Asc x mp) y)) xs'

-- Reverse Elems' order
rev :: ColOrder -> Elem -> Elem
rev Asc x = x
rev Desc x = DW (Down x)

-- ======================
-- == labels convenience functions
-- ======================

-- | Given [ai0, ai1, ... ain] and [a0,a1,..aN], returns [ai0, ai1, ... ain, ... aN].
-- That is with lftlbls on the left and then the remaining labels from lbls.
keepLeft' :: [String] -> [String] -> [String]
keepLeft' lbls leftlbls  = leftlbls ++ filter (not . (flip elem leftlbls)) lbls

-- | Given a relation and some of its labels [ai0, ai1, ... ain], returns the relation's attributes
-- reordered with lftlbls on the left and then the remaining labels from lbls.
keepLeft :: Relvar -> [String] -> [String]
keepLeft r leftlbls = keepLeft' (labels r) leftlbls

-- | Given [ai0, ai1, ... ain] and [a0,a1,..aN], returns [ai0, ai1, ... ain, ... aN].
-- That is with lftlbls on the left and then the remaining labels from lbls.
keepRight' :: [String] -> [String] -> [String]
keepRight' lbls rightlbls  =  (filter (not . (flip elem rightlbls)) lbls) ++ rightlbls

-- | Given a relation and some of its labels [ai0, ai1, ... ain], returns the relation's attributes
-- reordered with lftlbls on the left and then the remaining labels from lbls.
keepRight :: Relvar -> [String] -> [String]
keepRight r rightlbls = keepRight' (labels r) rightlbls

-- FIXME: more convenience functions (eg left and righ plus some ordering in the middle...)


-- ======================
-- == utility functions
-- ======================

-- | Mapping from Elem to their corresponding typeRep. Only relevant for basic types and relations.
typeRep :: Elem -> TypeRep
typeRep (B x) = tyB
typeRep (C x) = tyC
typeRep (S x) = tyS
typeRep (T x) = tyT
typeRep (I x) = tyI
typeRep (J x) = tyJ
typeRep (D x) = tyD
typeRep (DD x) = tyDD
typeRep (DT x) = tyDT
typeRep (BS x) = tyBS
-- (ADD NEW TYPES HERE)
typeRep (R x) = tyR
typeRep (A x) = tyA
typeRep (T2 x) = tyT2
typeRep (T3 x) = tyT3
typeRep (DW x) = tyDW
typeRep Nil = tyZ

-- | TypeRep for Bool.
tyB  = typeOf True
-- | TypeRep for Char.
tyC  = typeOf 'a'
-- | TypeRep for String.
tyS  = typeOf "a"
-- | TypeRep for Text.
tyT  = typeOf (undefined :: Text)
-- | TypeRep for Int.
tyI  = typeOf (1::Int)
-- | TypeRep for Integer.
tyJ  = typeOf (1::Integer)
-- | TypeRep for Double
tyD  = typeOf (1.0 :: Double)
-- | TypeRep for Day.
tyDD = typeOf (undefined :: Day)
-- | TypeRep for UTCTime.
tyDT = typeOf (undefined :: UTCTime)
-- | TypeRep for ByteString.
tyBS = typeOf (undefined :: ByteString)
-- (ADD NEW TYPES HERE)
-- | TypeRep for Relvar.
tyR  = typeOf (undefined :: Relvar)
-- | TypeRep for [Elem].
tyA  = typeOf ([S "a", I 1])
-- | TypeRep for (Elem, Elem).
tyT2 = typeOf (S "a", I 1)
-- | TypeRep for (Elem, Elem, Elem).
tyT3 = typeOf (S "a", I 1, B True)
-- | TypeRep for (Down Elem).
tyDW = typeOf (undefined :: (Down Elem))
-- | TypeRep for Nil.
tyZ  = typeOf (())

-- ======================
-- == Local functions
-- ======================

-- relVar functions
-- attributes' :: Relvar -> Set.Set (String, TypeRep)
-- attributes' r = Set.fromList $ zip (anames r) (atypes r) --attributes are an (unordered) set...

-- output a relvar's number of rows
rows' :: Relvar -> Int
rows' r = Set.size (rdata r)



-- ======================
-- == Error messages
-- ======================

errNullCol :: String
errNullCol = "One or several column names are empty string."

errDupCol :: String
errDupCol = "Duplicate column names are not allowed."

errSizMismatch :: Int -> Int -> String
errSizMismatch lc lt = "Different size for column names (" ++ (show lc) ++ ") and types (" ++ (show lt) ++ ")"










