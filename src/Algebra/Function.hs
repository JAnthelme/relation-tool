{-|
Module      : Algebra.Function
Description : Boilerplate code.
Copyright   : (c) Janthelme, 2016
License     : BDS3
Maintainer  : janthelme@gmail.com
Stability   : experimental
Portability : POSIX

Boilerplate code to translate @a->b@ functions into corresponding @'Relvar.Elem'->'Relvar.Elem'@ functions (see 'liftElem') and also into @['Relvar.Elem']->'Relvar.Elem'@ functions for the function 'Algebra.extend'.

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- usage
-- provides ways to produce [Elem]->Elem functions
-- Warning: A lot of boilerplate code. FIXME: use generics or dynamic instead (see http://stackoverflow.com/questions/27393968/from-a-b-to-mytype-mytype)
-- CHECK: use TH?
module Algebra.Function
( module Relvar,

  -- * Classes
    LiftElem (..)
  , LiftElem2 (..)
  , LiftElem3 (..)
  , BoolFun (..)
  , BoolFun2 (..)
  , BoolFun3 (..)

) where

-- import Debug.Trace
import Relvar
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map (fromList, findWithDefault)

-- | Lifting functions into the "Elem domain"
-- Use this class to translate f :: a -> b into the corresponding (Elem -> Elem) function
class (Elementable a, Elementable b) => LiftElem a b where
  liftLeft' :: (a -> b) -> (Elem -> Maybe b)
  liftLeft' f x = case fromElem x of
    Nothing -> Nothing
    Just x' -> Just (f x')

  -- necessary to define boolFun, necessary to restrict with unary filter functions
  liftLeft :: (a -> b) -> ([Elem] -> Maybe b)
  liftLeft f [] = error "Empty argument list"
  liftLeft f (x:xs) = (liftLeft' f) x

  liftEl'  :: (a -> b) -> (Elem -> Elem)
  liftEl' f x = case (liftLeft' f) x of
    Nothing -> Nil
    Just z -> toElem z

  liftEl   :: (a -> b) -> ([Elem] -> Elem)
  liftEl f (x:xs) = (liftEl' f) x
  liftEl f [] = Nil

-- | Lifting functions into the "Elem domain"
-- Use this class to translate f :: a1 -> a2 -> b into the corresponding (Elem -> Elem -> Elem) function
class (Elementable a1, Elementable a2, Elementable b) => LiftElem2 a1 a2 b where
  liftLeft2' :: (a1 -> a2 -> b) -> (Elem -> Elem -> Maybe b)
  liftLeft2' f x y = case (fromElem x, fromElem y) of
    (Just x', Just y') -> Just (f x' y')
    otherwise          -> Nothing

  -- necessary to define boolFun2, necessary to restrict with binary filter functions
  liftLeft2 :: (a1 -> a2 -> b) -> ([Elem] -> Maybe b)
  liftLeft2 f [] = error "Empty argument list"
  liftLeft2 f (x:[]) = error "Argument list: Need 2 or more elements"
  liftLeft2 f (x:y:xs) = (liftLeft2' f) x y

  liftEl2'  :: (a1 -> a2 -> b) -> (Elem -> Elem -> Elem)
  liftEl2' f x y = case (liftLeft2' f) x y of
    Nothing -> Nil
    Just z -> toElem z

  liftEl2  :: (a1 -> a2 -> b) -> ([Elem] -> Elem)
  liftEl2 f (x:y:xs) = (liftEl2' f) x y
  liftEl2 f _        = Nil

-- | Lifting functions into the "Elem domain"
-- Use this class to translate f :: a1 -> a2 -> a3 -> b into the corresponding (Elem -> Elem -> Elem -> Elem) function
class (Elementable a1, Elementable a2, Elementable a3, Elementable b) => LiftElem3 a1 a2 a3 b where
  liftLeft3' :: (a1 -> a2 -> a3 -> b) -> (Elem -> Elem -> Elem -> Maybe b)
  liftLeft3' f x1 x2 x3 = case (fromElem x1, fromElem x2, fromElem x3) of
    (Just x1', Just x2', Just x3') -> Just (f x1' x2' x3')
    otherwise          -> Nothing

  -- necessary to define boolFun3, necessary to use restrict with ternary filter functions
  liftLeft3 :: (a1 -> a2 -> a3 -> b) -> ([Elem] -> Maybe b)
  liftLeft3 f [] = error "Empty argument list"
  liftLeft3 f (x1:[]) = error "Argument list: Need 3 or more elements"
  liftLeft3 f (x1:x2:[]) = error "Argument list: Need 3 or more elements"
  liftLeft3 f (x1:x2:x3:xs) = (liftLeft3' f) x1 x2 x3

  liftEl3'  :: (a1 -> a2 -> a3 -> b) -> (Elem -> Elem -> Elem -> Elem)
  liftEl3' f x1 x2 x3 = case (liftLeft3' f) x1 x2 x3 of
    Nothing -> Nil
    Just z -> toElem z

  liftEl3  :: (a1 -> a2 -> a3 -> b) -> ([Elem] -> Elem)
  liftEl3 f (x1:x2:x3:xs) = (liftEl3' f) x1 x2 x3
  liftEl3 f _             = Nil

-- ADD ADDITIONAL CLASSES FOR HIGHER ARITY

-- | "Lifting" (a->Bool) functions to ([Elem]->Bool) functions.
-- This is a convenience function when using 'Relvar.restrict' function.
class LiftElem a Bool => BoolFun a where
    liftBoolFun :: (a -> Bool) -> ([Elem] -> Bool)
    liftBoolFun f x = fromMaybe False $ liftLeft f x

-- | "Lifting" (a1->a2->Bool) functions to ([Elem]->Bool) functions.
-- This is a convenience function when using 'Relvar.restrict' function.
class LiftElem2 a1 a2 Bool => BoolFun2 a1 a2 where
    liftBoolFun2 :: (a1 -> a2 -> Bool) -> ([Elem] -> Bool)
    liftBoolFun2 f x = fromMaybe False $ liftLeft2 f x

-- | "Lifting" (a1->a2->Bool) functions to ([Elem]->Bool) functions.
-- This is a convenience function when using 'Relvar.restrict' function.
class LiftElem3 a1 a2 a3 Bool => BoolFun3 a1 a2 a3 where
    liftBoolFun3 :: (a1 -> a2 -> a3 -> Bool) -> ([Elem] -> Bool)
    liftBoolFun3 f x = fromMaybe False $ liftLeft3 f x


-- ADD ADDITIONAL CLASSES FOR HIGHER ARITY
-- FIXME: no instance for arrays (A x) and tuples (T2 x) and (T3 x)

-- == START : Code generated by genBoilerCode1 ==
instance BoolFun Bool
instance BoolFun Char
instance BoolFun String
instance BoolFun Text
instance BoolFun Int
instance BoolFun Integer
instance BoolFun Double
instance BoolFun Day
instance BoolFun UTCTime
instance BoolFun ByteString
instance BoolFun Relvar
instance Elementable a => LiftElem Bool a
instance Elementable a => LiftElem Char a
instance Elementable a => LiftElem String a
instance Elementable a => LiftElem Text a
instance Elementable a => LiftElem Int a
instance Elementable a => LiftElem Integer a
instance Elementable a => LiftElem Double a
instance Elementable a => LiftElem Day a
instance Elementable a => LiftElem UTCTime a
instance Elementable a => LiftElem ByteString a
instance Elementable a => LiftElem Relvar a
-- == END : Code generated by genBoilerCode1 ==

-- == START : Code generated by genBoilerCode2 ==
instance BoolFun2 Bool Bool
instance BoolFun2 Bool Char
instance BoolFun2 Bool String
instance BoolFun2 Bool Text
instance BoolFun2 Bool Int
instance BoolFun2 Bool Integer
instance BoolFun2 Bool Double
instance BoolFun2 Bool Day
instance BoolFun2 Bool UTCTime
instance BoolFun2 Bool ByteString
instance BoolFun2 Bool Relvar
instance BoolFun2 Char Bool
instance BoolFun2 Char Char
instance BoolFun2 Char String
instance BoolFun2 Char Text
instance BoolFun2 Char Int
instance BoolFun2 Char Integer
instance BoolFun2 Char Double
instance BoolFun2 Char Day
instance BoolFun2 Char UTCTime
instance BoolFun2 Char ByteString
instance BoolFun2 Char Relvar
instance BoolFun2 String Bool
instance BoolFun2 String Char
instance BoolFun2 String String
instance BoolFun2 String Text
instance BoolFun2 String Int
instance BoolFun2 String Integer
instance BoolFun2 String Double
instance BoolFun2 String Day
instance BoolFun2 String UTCTime
instance BoolFun2 String ByteString
instance BoolFun2 String Relvar
instance BoolFun2 Text Bool
instance BoolFun2 Text Char
instance BoolFun2 Text String
instance BoolFun2 Text Text
instance BoolFun2 Text Int
instance BoolFun2 Text Integer
instance BoolFun2 Text Double
instance BoolFun2 Text Day
instance BoolFun2 Text UTCTime
instance BoolFun2 Text ByteString
instance BoolFun2 Text Relvar
instance BoolFun2 Int Bool
instance BoolFun2 Int Char
instance BoolFun2 Int String
instance BoolFun2 Int Text
instance BoolFun2 Int Int
instance BoolFun2 Int Integer
instance BoolFun2 Int Double
instance BoolFun2 Int Day
instance BoolFun2 Int UTCTime
instance BoolFun2 Int ByteString
instance BoolFun2 Int Relvar
instance BoolFun2 Integer Bool
instance BoolFun2 Integer Char
instance BoolFun2 Integer String
instance BoolFun2 Integer Text
instance BoolFun2 Integer Int
instance BoolFun2 Integer Integer
instance BoolFun2 Integer Double
instance BoolFun2 Integer Day
instance BoolFun2 Integer UTCTime
instance BoolFun2 Integer ByteString
instance BoolFun2 Integer Relvar
instance BoolFun2 Double Bool
instance BoolFun2 Double Char
instance BoolFun2 Double String
instance BoolFun2 Double Text
instance BoolFun2 Double Int
instance BoolFun2 Double Integer
instance BoolFun2 Double Double
instance BoolFun2 Double Day
instance BoolFun2 Double UTCTime
instance BoolFun2 Double ByteString
instance BoolFun2 Double Relvar
instance BoolFun2 Day Bool
instance BoolFun2 Day Char
instance BoolFun2 Day String
instance BoolFun2 Day Text
instance BoolFun2 Day Int
instance BoolFun2 Day Integer
instance BoolFun2 Day Double
instance BoolFun2 Day Day
instance BoolFun2 Day UTCTime
instance BoolFun2 Day ByteString
instance BoolFun2 Day Relvar
instance BoolFun2 UTCTime Bool
instance BoolFun2 UTCTime Char
instance BoolFun2 UTCTime String
instance BoolFun2 UTCTime Text
instance BoolFun2 UTCTime Int
instance BoolFun2 UTCTime Integer
instance BoolFun2 UTCTime Double
instance BoolFun2 UTCTime Day
instance BoolFun2 UTCTime UTCTime
instance BoolFun2 UTCTime ByteString
instance BoolFun2 UTCTime Relvar
instance BoolFun2 ByteString Bool
instance BoolFun2 ByteString Char
instance BoolFun2 ByteString String
instance BoolFun2 ByteString Text
instance BoolFun2 ByteString Int
instance BoolFun2 ByteString Integer
instance BoolFun2 ByteString Double
instance BoolFun2 ByteString Day
instance BoolFun2 ByteString UTCTime
instance BoolFun2 ByteString ByteString
instance BoolFun2 ByteString Relvar
instance BoolFun2 Relvar Bool
instance BoolFun2 Relvar Char
instance BoolFun2 Relvar String
instance BoolFun2 Relvar Text
instance BoolFun2 Relvar Int
instance BoolFun2 Relvar Integer
instance BoolFun2 Relvar Double
instance BoolFun2 Relvar Day
instance BoolFun2 Relvar UTCTime
instance BoolFun2 Relvar ByteString
instance BoolFun2 Relvar Relvar
instance Elementable a => LiftElem2 Bool Bool a
instance Elementable a => LiftElem2 Bool Char a
instance Elementable a => LiftElem2 Bool String a
instance Elementable a => LiftElem2 Bool Text a
instance Elementable a => LiftElem2 Bool Int a
instance Elementable a => LiftElem2 Bool Integer a
instance Elementable a => LiftElem2 Bool Double a
instance Elementable a => LiftElem2 Bool Day a
instance Elementable a => LiftElem2 Bool UTCTime a
instance Elementable a => LiftElem2 Bool ByteString a
instance Elementable a => LiftElem2 Bool Relvar a
instance Elementable a => LiftElem2 Char Bool a
instance Elementable a => LiftElem2 Char Char a
instance Elementable a => LiftElem2 Char String a
instance Elementable a => LiftElem2 Char Text a
instance Elementable a => LiftElem2 Char Int a
instance Elementable a => LiftElem2 Char Integer a
instance Elementable a => LiftElem2 Char Double a
instance Elementable a => LiftElem2 Char Day a
instance Elementable a => LiftElem2 Char UTCTime a
instance Elementable a => LiftElem2 Char ByteString a
instance Elementable a => LiftElem2 Char Relvar a
instance Elementable a => LiftElem2 String Bool a
instance Elementable a => LiftElem2 String Char a
instance Elementable a => LiftElem2 String String a
instance Elementable a => LiftElem2 String Text a
instance Elementable a => LiftElem2 String Int a
instance Elementable a => LiftElem2 String Integer a
instance Elementable a => LiftElem2 String Double a
instance Elementable a => LiftElem2 String Day a
instance Elementable a => LiftElem2 String UTCTime a
instance Elementable a => LiftElem2 String ByteString a
instance Elementable a => LiftElem2 String Relvar a
instance Elementable a => LiftElem2 Text Bool a
instance Elementable a => LiftElem2 Text Char a
instance Elementable a => LiftElem2 Text String a
instance Elementable a => LiftElem2 Text Text a
instance Elementable a => LiftElem2 Text Int a
instance Elementable a => LiftElem2 Text Integer a
instance Elementable a => LiftElem2 Text Double a
instance Elementable a => LiftElem2 Text Day a
instance Elementable a => LiftElem2 Text UTCTime a
instance Elementable a => LiftElem2 Text ByteString a
instance Elementable a => LiftElem2 Text Relvar a
instance Elementable a => LiftElem2 Int Bool a
instance Elementable a => LiftElem2 Int Char a
instance Elementable a => LiftElem2 Int String a
instance Elementable a => LiftElem2 Int Text a
instance Elementable a => LiftElem2 Int Int a
instance Elementable a => LiftElem2 Int Integer a
instance Elementable a => LiftElem2 Int Double a
instance Elementable a => LiftElem2 Int Day a
instance Elementable a => LiftElem2 Int UTCTime a
instance Elementable a => LiftElem2 Int ByteString a
instance Elementable a => LiftElem2 Int Relvar a
instance Elementable a => LiftElem2 Integer Bool a
instance Elementable a => LiftElem2 Integer Char a
instance Elementable a => LiftElem2 Integer String a
instance Elementable a => LiftElem2 Integer Text a
instance Elementable a => LiftElem2 Integer Int a
instance Elementable a => LiftElem2 Integer Integer a
instance Elementable a => LiftElem2 Integer Double a
instance Elementable a => LiftElem2 Integer Day a
instance Elementable a => LiftElem2 Integer UTCTime a
instance Elementable a => LiftElem2 Integer ByteString a
instance Elementable a => LiftElem2 Integer Relvar a
instance Elementable a => LiftElem2 Double Bool a
instance Elementable a => LiftElem2 Double Char a
instance Elementable a => LiftElem2 Double String a
instance Elementable a => LiftElem2 Double Text a
instance Elementable a => LiftElem2 Double Int a
instance Elementable a => LiftElem2 Double Integer a
instance Elementable a => LiftElem2 Double Double a
instance Elementable a => LiftElem2 Double Day a
instance Elementable a => LiftElem2 Double UTCTime a
instance Elementable a => LiftElem2 Double ByteString a
instance Elementable a => LiftElem2 Double Relvar a
instance Elementable a => LiftElem2 Day Bool a
instance Elementable a => LiftElem2 Day Char a
instance Elementable a => LiftElem2 Day String a
instance Elementable a => LiftElem2 Day Text a
instance Elementable a => LiftElem2 Day Int a
instance Elementable a => LiftElem2 Day Integer a
instance Elementable a => LiftElem2 Day Double a
instance Elementable a => LiftElem2 Day Day a
instance Elementable a => LiftElem2 Day UTCTime a
instance Elementable a => LiftElem2 Day ByteString a
instance Elementable a => LiftElem2 Day Relvar a
instance Elementable a => LiftElem2 UTCTime Bool a
instance Elementable a => LiftElem2 UTCTime Char a
instance Elementable a => LiftElem2 UTCTime String a
instance Elementable a => LiftElem2 UTCTime Text a
instance Elementable a => LiftElem2 UTCTime Int a
instance Elementable a => LiftElem2 UTCTime Integer a
instance Elementable a => LiftElem2 UTCTime Double a
instance Elementable a => LiftElem2 UTCTime Day a
instance Elementable a => LiftElem2 UTCTime UTCTime a
instance Elementable a => LiftElem2 UTCTime ByteString a
instance Elementable a => LiftElem2 UTCTime Relvar a
instance Elementable a => LiftElem2 ByteString Bool a
instance Elementable a => LiftElem2 ByteString Char a
instance Elementable a => LiftElem2 ByteString String a
instance Elementable a => LiftElem2 ByteString Text a
instance Elementable a => LiftElem2 ByteString Int a
instance Elementable a => LiftElem2 ByteString Integer a
instance Elementable a => LiftElem2 ByteString Double a
instance Elementable a => LiftElem2 ByteString Day a
instance Elementable a => LiftElem2 ByteString UTCTime a
instance Elementable a => LiftElem2 ByteString ByteString a
instance Elementable a => LiftElem2 ByteString Relvar a
instance Elementable a => LiftElem2 Relvar Bool a
instance Elementable a => LiftElem2 Relvar Char a
instance Elementable a => LiftElem2 Relvar String a
instance Elementable a => LiftElem2 Relvar Text a
instance Elementable a => LiftElem2 Relvar Int a
instance Elementable a => LiftElem2 Relvar Integer a
instance Elementable a => LiftElem2 Relvar Double a
instance Elementable a => LiftElem2 Relvar Day a
instance Elementable a => LiftElem2 Relvar UTCTime a
instance Elementable a => LiftElem2 Relvar ByteString a
instance Elementable a => LiftElem2 Relvar Relvar a
-- == END : Code generated by genBoilCode2 ==


-------------------------
-- boilerplate code generation -- CHECK: use TH?
-------------------------

-- update these lists with Elem definitions (eg when adding a new basic type)
genStrTy = ["Bool", "Char", "String", "Text", "Int", "Integer", "Double", "Day", "UTCTime", "ByteString", "Relvar"]
genStrEl = ["B", "C", "S", "T", "I", "J", "D", "DD", "DT", "BS", "R"]
genTypeMap = Map.fromList $ zip genStrTy genStrEl

-- boilerplate functions
-- unary functions
genBoolFun1 :: String -> String
genBoolFun1 strTy1 = "instance BoolFun " ++ strTy1

genLiftElem1 :: String -> String
genLiftElem1 strTy1 = "instance Elementable a => LiftElem " ++ strTy1 ++ " a"

-- boilercode generation for unary functions (run genBoilerCode11, copy stdout and paste in code)
genBoilerCode1 :: IO()
genBoilerCode1 = do
    -- let pairs = [(x,y) | x <- genStrTy, y <- genStrTy]
    putStrLn "-- == START : Code generated by genBoilerCode1 =="
    mapM_ putStrLn $ map genBoolFun1 genStrTy
    mapM_ putStrLn $ map genLiftElem1 genStrTy
    putStrLn "-- == END : Code generated by genBoilerCode1 =="


-- binary functions
genBoolFun2 :: (String, String) -> String
genBoolFun2 (strTy1,strTy2) = "instance BoolFun2 " ++ strTy1 ++ " " ++ strTy2

genLiftElem2 :: (String, String) -> String
genLiftElem2 (strTy1,strTy2) = "instance Elementable a => LiftElem2 " ++ strTy1 ++ " " ++ strTy2 ++ " a"

-- boilercode generation for binary function (run genBoilerCode12)
genBoilerCode2 :: IO()
genBoilerCode2 = do
    let pairs = [(x,y) | x <- genStrTy, y <- genStrTy]
        xs = ["-- == START : Code generated by genBoilerCode2 =="]
             ++ map genBoolFun2 pairs
             ++ map genLiftElem2 pairs
             ++ ["-- == END : Code generated by genBoilCode2 =="]
    mapM_ putStrLn xs








