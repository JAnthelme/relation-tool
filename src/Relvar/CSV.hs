{-|
Module      : Relvar.CSV
Description : Simple import from CSV.
Copyright   : (c) Janthelme, 2016
License     : BDS3
Maintainer  : janthelme@gmail.com
Stability   : experimental
Portability : POSIX

Import and convert csv files of basic types elements (only) into 'Elem's
and produce the corresponding relation. Note that duplicate rows will be merged into one.
-}

{-# LANGUAGE FlexibleInstances #-}

module Relvar.CSV (
    CSVSettings(..)
    -- * Types
    , Relvar.CSV.fromCSV
    , toRelation
)
where

import Relvar
-- import qualified Data.CSV.Conduit hiding (Row, fromCSV)
import qualified Data.CSV.Conduit as CSV
import Data.CSV.Conduit (CSVSettings(..))

import qualified Data.Vector as V ((!), map, toList, tail)

import qualified Data.Text as T (pack, unpack)
import qualified Data.Text (Text)
import Data.Typeable (TypeRep)
import qualified Data.Map.Strict as Map (Map, findWithDefault, fromList)

import Data.Time (Day, fromGregorian)
import Data.List.Split (splitOn)

-- Given an appropriate TypeRep, convert a CSV string to Elem.
fromCSV' :: TypeRep -> String -> Elem
fromCSV' t s
    | t == tyB = B (read s)
    | t == tyS = S s
    | t == tyT = T (read s)
    | t == tyI = I (read s)
    | t == tyJ = J (read s)
    | t == tyD = D (read s)
-- Add DD and DT?   
-- change signature or code so that it takes several date formats
-- currently it only works for yyyy-mm-dd format
    | t == tyDD = DD (toDate "yyyy-mm-dd" s)
    | t == tyZ  = Nil
    | otherwise = Nil
    {- 
    | t == tyZ  = trace ("CSV error: Couldn't Parse: " ++ (show s)) Nil
    | otherwise = trace ("CSV error: Couldn't Parse: " ++ (show s)) Nil
    -}
-- CHECK: why a?
-- | Given an appropriate TypeRep mapping, convert a CSV string to Elem.
fromCSV :: Ord a => (Map.Map a TypeRep) -> a -> String  -> Elem
fromCSV m lbl strval = fromCSV' (Map.findWithDefault tyZ lbl m) strval

csvset :: Char ->  CSV.CSVSettings 
csvset c =  CSV.CSVSettings {csvSep  = c, csvQuoteChar = Just '"'} 

-- | @toRelation filepath delimiter labels types@ to corresponding IO ('Relvar.Relvar').
toRelation :: String -> Char -> [String] -> [TypeRep] -> IO (Relvar)
toRelation fp d lbls tys = do
    vs <- CSV.readCSVFile (csvset d) fp
    
    let hd  = vs V.! 0
    let vs' = V.tail vs
    let toEl col s = fromCSV (Map.fromList $ zip (map T.pack lbls) tys) col (T.unpack s)
    let rws = V.toList $ V.map (zipWith toEl hd) vs'   

    return $ relvar lbls tys rws
    
    
    
-- conversion from string to Day
toDate :: String -> String -> Day
toDate format sdate = case format of
    "yyyy-mm-dd" -> let y:m:d:xs = map read $ splitOn "-" sdate in fromGregorian (fromIntegral y) m d
    
    otherwise    -> undefined    