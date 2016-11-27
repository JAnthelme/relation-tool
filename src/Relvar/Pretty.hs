{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Relvar.Pretty
Description : Text rendering and output to stdout.
Copyright   : (c) Janthelme, 2016
License     : BDS3
Maintainer  : janthelme@gmail.com
Stability   : experimental
Portability : POSIX

Set of convenience functions to render relations and output them to stdout. 

Credit: This module is based on a modified version of Text.PrettyPrint.Boxes in the boxes package. Original version by Brent Yorgey and David Feuer <https://www.stackage.org/lts-6.13/package/boxes-0.1.4 here> and <https://github.com/treeowl/boxes here>. Modified version by rustydc <https://github.com/treeowl/boxes/pull/1 here>. 

The original boxes package cannot be used as is, as it does not handle Text type (and therefore does not allow Unicode characters output).
-}


module Relvar.Pretty(
    -- * Types
    Params (..)
    , Alignment(..)    
    -- * View Functions
    , render'
    , Relvar.Pretty.render
    , view'
    , view
    , viewAll
    , viewAll'
    
    -- * Misc
    , defParams

    
    ) where

import Relvar hiding(table)

import Relvar.Pretty.BoxesText hiding (render)                             -- Data.Text rendering
import Relvar.Pretty.BoxesText as B (render)

import Data.List (transpose, intersperse, sort, maximum)
import qualified Data.Set as Set (toList, map, findMax, foldl,fromList) 
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStr, putStrLn)

-- CHECK : TBD
import qualified Relvar.LSet as LSet (values)

-- CHECK: clean this module's functions
-- CHECK: TODO might want to output with or without double-quotes. Should headers always be double-quote free?
-- CHECK: TODO show '...' (continuation points) following partial view outputs?
-- CHECK: TODO show types in view outputs?

-- | Box parameters.
data Params = Params { col_width_min :: Int
                     , border_top :: Bool
                     , border_bot :: Bool
                     , border_lft :: Bool
                     , border_rgt :: Bool
                     , col_align  :: Alignment
                     }
                     deriving (Eq, Read, Show)

{- | Default parameters.

@
col_width_min = 10, border_top = False, border_bot = False, border_lft = False, border_rgt = False, col_align = left
@
-}
defParams = Params { col_width_min = 10
                   , border_top = False, border_bot = False, border_lft = False, border_rgt = False
                   , col_align = left
                   }
-- CHECK: remove this function                  
-- toElems :: Row -> [Elem] -- = Set.Set (String, Elem)
-- toElems rv = map snd $ Set.toList rv


-- | Any 'LSet' and 'Relvar.Relvar's are part of this class.
-- 
-- Class for Tabular types which can be represented as text boxes.
class Tabular a where
    -- | Render a Relvar as a Text, suitable for writing to the screen or
    --   a file.
    render' :: a -> Params -> [Int] -> Text

    -- | Render a Relvar as a Text. 
    -- Identical to 'render'' but 'defParams' is used and all rows are shown.
    render :: a -> Text

    -- | Output a relation to the screen, given parameters and the number of rows to output.
    view' :: a -> Params -> [Int] -> IO()

    -- | Output a relation to the screen. 
    -- Identical to 'view'' but with defParams.
    view :: a -> [Int] -> IO()

    -- | Output a relation to the screen. 
    -- Identical to 'view'' but showing all rows.
    viewAll' :: a -> Params -> IO()

    -- | Output a relation to the screen. 
    -- Identical to 'viewAll'' but with defParams.
    viewAll :: a -> IO()

instance Tabular Relvar where
    render' r pm ixs = B.render $ table pm (tableBox pm r) ixs
    render r = render' r defParams [0..] 
    view' r pm ixs
        | r == dee  = putStrLn "- dee -"
        | r == dum  = putStrLn "- dum -"
        | otherwise = printBox $ table pm (tableBox pm r) ixs
    view r ixs = view' r defParams ixs
    viewAll' r pm = view' r pm [0..]
    viewAll r = viewAll' r defParams

instance Tabular Table where
    render' tbl pm ixs = B.render $ table pm (tableBox' pm (header tbl) (tdata tbl)) ixs
    render t = render' t defParams [0..] 
    view' tbl pm ixs = printBox $ table pm (tableBox' pm (header tbl) (tdata tbl)) ixs
{-        | r == dee  = putStrLn "- dee -"
        | r == dum  = putStrLn "- dum -"
        | otherwise = printBox $ table pm (tableBox' pm lbls rws r) ixs
-}
    view t ixs = view' t defParams ixs
    viewAll' t pm = view' t pm [0..]
    viewAll t = viewAll' t defParams

{-
instance Tabular (([String], [[Elem]])) where
    render' (lbls,rws) pm ixs = B.render $ table pm (tableBox' pm lbls rws) ixs
    render t = render' t defParams [0..] 
    view' (lbls,rws) pm ixs = printBox $ table pm (tableBox' pm lbls rws) ixs
{-        | r == dee  = putStrLn "- dee -"
        | r == dum  = putStrLn "- dum -"
        | otherwise = printBox $ table pm (tableBox' pm lbls rws r) ixs
-}
    view t ixs = view' t defParams ixs
    viewAll' t pm = view' t pm [0..]
    viewAll t = viewAll' t defParams
-}

-- Text representation of Elem types
showElem :: Bool -> Elem -> Text
showElem _ (B x) = T.pack $ show x
showElem _ (C x) = T.pack $ show x
showElem _ (S x) = T.pack x
showElem _ (T x) = id x

showElem True (R r) = T.pack $ show (R r)
showElem False (R r) = T.pack $ show r -- "REL(" ++ show (rows2 r) ++ "," ++ show (cols2 r) ++ ")"
showElem _ (I x) = T.pack $ show x

showElem _ (DD x) = T.pack $ show x

showElem _ (D x) = T.pack $ show x
showElem _ (A x) = T.pack $ show x
showElem _  _ = T.pack $ show Nil

mxHeight :: Bool -> Row -> Int
mxHeight hh rw = Set.findMax $ Set.map f rw
    where f = \ (c,v) -> height hh v

maxHeight :: [Box] -> Int
maxHeight rw = maximum $ map rows rw
    
height :: Bool -> Elem -> Int
height hasHeader (R r) = (if hasHeader then 1 else 0) + (foldl (+) 0 $ map (mxHeight True) (Set.toList $ rdata r))
height _ _ = 1

toBox p (R r) = table p (tableBox p r) [0..]
toBox p e     = text $ showElem False e

hline :: Int -> Box
hline n = text $ T.pack $ concat $ replicate n "-"

vline bbot rhs rn = vcat left xs
    where xss  = map (\ rh -> (replicate rh $ text "|") ++ [text "+"]) rhs
          xs = case bbot of
                True  -> concat xss
                False -> init $ concat xss

f2' pm (n, bs) = xs
  where xs = intersperse (hline $ max n $ col_width_min pm) bs
                
tableBox' :: Params -> [String] -> [[Elem]] -> [[Box]]
tableBox' pm hdr ess = rws'
    where -- ess = [hdr'] ++ rws
          rrws = map (map $ toBox pm) $ ([hdr'] ++ ess)
          hs = map (maximum . map rows) rrws
          rws' = map f5 $ zip hs rrws -- with the horizontal lines
          hdr' = map (\ x-> S x) hdr -- CHECK: should use another func?
          f5 (n, bs) = map (alignVert top n) bs


tableBoxOLD' :: Params -> [String] -> [[Elem]] -> [[Box]]
tableBoxOLD' pm hdr ess = rws'
    where -- ess = [hdr'] ++ rws
          rrws = map (map $ toBox pm) $ ([hdr'] ++ ess)
          hs = map (maximum . map rows) rrws
          rws' = map f5 $ zip hs rrws -- with the horizontal lines
          hdr' = map (\ x-> S x) hdr -- CHECK: should use another func?
          f5 (n, bs) = map (alignVert top n) bs

tableBox :: Params -> Relvar -> [[Box]]
tableBox pm r = tableBox' pm (labels r) $ map LSet.values $ Set.toList $ rdata r -- CHECK: should use Relvar.elems or elems'



table :: Params -> [[Box]] -> [Int] -> Box
table _ bss [] = text ""
table pm bss ixs = punctuateH left (vline (border_bot pm) hs (length ixs)) rws''
    where ixs' = take (length bss) ixs -- so that ixs = [0..] is ok
          bss' = take (length ixs') $ drop (head ixs') bss
          
          hs = map (maximum . map rows) bss'
          bss'' = transpose $ bss'
          
          ws = map (maximum . map cols) bss''
          rws'' = map f3 $ zip ws bss'' -- with the horizontal lines
          f3 (n, bs) = xs'
             where bx = hline $ max n (col_width_min pm)
                   xs = punctuateV (col_align pm) bx bs
                   xs' = case border_bot pm of
                          True  -> vcat (col_align pm) $ [xs,bx] 
                          False -> xs

