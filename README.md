# Relation Toolbox
> A lightweight relational algebra operators library.

## Installation

Install the Haskell Tool [Stack](https://docs.haskellstack.org/en/stable/README/).

From the shell prompt:
```sh
git clone https://github.com/janthelme/relation-tool.git
cd relation-tool
stack build
stack exec basics-exe
```
There are 3 executable examples available: basics-exe, csv-exe and jcdate-exe.


## Documentation
Check the [Haddock documentation](https://janthelme.github.io/relation-tool/).

## Basics
For the proper definition of most concepts, see C.J. Date, "*An Introduction to Database Systems*" Eighth Edition, chapter 7 ('*Relational Algebra*').

Loosely speaking a relation is a collection of tuples, or "rows", of elements and each of its "column" is associated to an attribute, i.e. a column name (*label*) and a column type. "Collection" is in the sense of **unordered set**; neither tuples nor attributes ("columns") are ordered and duplicate rows or columns are not allowed.

Relations are of type `Relvar`. Relation elements are of type `Elem`:
```haskell
data Elem =
          -- basic types
          | B Bool | C Char | S String | T Text | I Int | J Integer | D Double | DD Day | DT UTCTime | BS ByteString
          -- others
          | A [Elem]              -- ^ array of Elems
          | T2 (Elem, Elem)       -- ^ pair of Elems
          | T3 (Elem, Elem, Elem) -- ^ triplet of Elems
          | R Relvar              -- ^ relation
          | DW (Down Elem)        -- ^ convenient for ordering Elems in descending order
          | Nil
          deriving (Eq, Ord, Typeable)
```
Motivation for this representation (non-exhaustive list): **Pros**: Simpler code. Easy to extend. **Cons**: Boilerplate code. Users need to update the code in order to add new Elem types.

Note that `R Relvar` is included in `Elem` and in this way relations can contain relations.

`Attributes` and `Row` types are defined in terms of labelled sets `LSet`, a type synonym for `Set (String, a)`. The use of `Set` garantees that duplicates are excluded.
```haskell
type Attributes = LSet TypeRep
```
```haskell
type Row = LSet Elem
```
Relations type:
```haskell
data Relvar = Relvar { attributes :: Attributes   -- ^ Set of the relation's attributes.
                     , rdata      :: Set.Set Row  -- ^ Set of the relation's tuples (i.e. rows).
                     }
                     deriving (Show, Typeable)
```

Unlike relations, tables are for representation; rows and columns are ordered and duplicate rows and columns are allowed. Table type:
```haskell
data Table = Table { header :: [String], tdata :: [[Elem]]} deriving (Show, Eq, Ord)
```

## Getting Started
### Elem
Lists of Elem, `[Elem]`, represent heterogeneous lists. They are necessary to define tuples and relations. Here is how to create such a list:
```haskell
mylist = [S "foo", D 3.14159, A [J 123, DD $ fromGregorian 2016 8 19], T2 (I 456, B True)]
```
... or using the `toElem` function:
```haskell
mylist = [toElem ("foo" :: String), toElem (3.14159 :: Double)
         , toElem [toElem (123 :: Integer), toElem $ fromGregorian 2016 8 19]
         , toElem (toElem (456 :: Int), toElem True)]
myotherlist = map toElem ["foo" :: String, "bar", "baz"] ++ map toElem [1..10::Int]
```
To embed a relation , `myrel`, in such a list:
```haskell
mylist = [S "foo", I 123, R myrel]
```
### Relvar
Relation initialization with the `relvar` function:
```haskell
myrel = relvar ["foo", "bar"] [typeOf (undefined::String), typeOf (undefined::Double)]
        [[S "abc", D 3.145159]
        ,[S "xyz", D 2.718281]
        ]
```
Since Haskell does not provide `TypeRep` literals, and because typing `typeOf (undefined::String)` is tedious, some predefined `TypeRep` "constants" are [available](https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Relvar.html#g:10) (FIXME: hyperlink to Haddock). Relations can then be created that way:
```haskell
myrel = relvar ["foo", "bar"] [tyS, tyD]
        [[S "abc", D 3.145159]
        ,[S "xyz", D 2.718281]
        ]
```
Unicode characters are represented via `(T Text)` `Elem`s:
```haskell
relchinese = relvar ["Hanzi", "Piyin"] [tyT, tyT]
            [[T "中", S "zhōng"]
            ,[T "國", S "guō"]
            ]
```
### Relation representation
Use `show` to print a relation to stdout without formatting:
```sh
*> myrel
Relvar {attributes = fromList [("bar",Double),("foo",[Char])], rdata = fromList [fromList [("bar",2.718281),("foo","xyz")],fromList [("bar",3.145159),("foo","abc")]]}
```
... or use the [view functions](https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Relvar-Pretty.html#g:2) FIXMEHL to give it in a tabular format:
```sh
*> viewAll myrel
bar       |foo
----------+----------
2.718281  |xyz
----------+----------
3.145159  |abc

```
... or create a table and print it:
```haskell
mytable = table myrel (Just ["foo","bar"]) Nothing Nothing
```
```sh
*> viewAll mytable
foo       |bar
----------+----------
xyz       |2.718281
----------+----------
abc       |3.145159
```
(here we just reordered the columns. Check-out the [`table`](https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Relvar.html#g:8) FIXMEHL function for more options).
### Relation operators
Given the following relation:
```sh
*> viewAll r_class
ANIMAL    |CLASS     
----------+----------
crocodile |reptiles  
----------+----------
dolphin   |mammals   
----------+----------
flamingo  |birds     
----------+----------
hippo     |mammals   
----------+----------
horse     |mammals   
----------+----------
kangaroo  |mammals   
----------+----------
lion      |mammals   
----------+----------
ostrich   |birds     
----------+----------
pelican   |birds     
----------+----------
platypus  |-nil-     
----------+----------
turtle    |reptiles  
```
... we can project on the "CLASS" column:
```haskell
r_project = project r_class ["CLASS"]
```
```
*> viewAll r_project
CLASS
----------
-nil-
----------
birds
----------
mammals
----------
reptiles
```
... we can restrict on the "mammals" class:
```haskell
f_mamm = (== "mammals") :: String -> Bool
r_restrict = restrict r_class (liftBoolFun f_mamm) ["CLASS"]
```
```sh
*> viewAll r_restrict
ANIMAL    |CLASS     
----------+----------
dolphin   |mammals   
----------+----------
hippo     |mammals   
----------+----------
horse     |mammals   
----------+----------
kangaroo  |mammals   
----------+----------
lion      |mammals   
```
(check the explanation on `liftBoolFun` [here](https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Algebra-Function.html) FIXME).

Given the additional relation:
```sh
*> viewAll r_habitat
ANIMAL    |HABITAT   
----------+----------
crocodile |land      
----------+----------
crocodile |water     
----------+----------
dolphin   |water     
----------+----------
flamingo  |air       
----------+----------
hippo     |land      
----------+----------
hippo     |water     
----------+----------
horse     |land      
----------+----------
kangaroo  |land      
----------+----------
lion      |land      
----------+----------
ostrich   |land      
----------+----------
pelican   |air       
----------+----------
pelican   |water     
----------+----------
platypus  |water     
----------+----------
turtle    |water     
```
... we can join the relations together:
```haskell
r_join = r_class `join` r_habitat
```
```sh
*> viewAll r_join
ANIMAL    |CLASS     |HABITAT   
----------+----------+----------
crocodile |reptiles  |land      
----------+----------+----------
crocodile |reptiles  |water     
----------+----------+----------
dolphin   |mammals   |water     
----------+----------+----------
flamingo  |birds     |air       
----------+----------+----------
hippo     |mammals   |land      
----------+----------+----------
hippo     |mammals   |water     
----------+----------+----------
horse     |mammals   |land      
----------+----------+----------
kangaroo  |mammals   |land      
----------+----------+----------
lion      |mammals   |land      
----------+----------+----------
ostrich   |birds     |land      
----------+----------+----------
pelican   |birds     |air       
----------+----------+----------
pelican   |birds     |water     
----------+----------+----------
platypus  |-nil-     |water     
----------+----------+----------
turtle    |reptiles  |water     
```
... and restrict further:
```haskell
f_mamm_h2o x y = (x == ("mammals")) && (y == ("water"))
r_restrict2 = restrict r_join (liftBoolFun2 f_mamm_h2o) ["CLASS", "HABITAT"]
```
```sh
*> viewAll r_restrict2
ANIMAL    |CLASS     |HABITAT   
----------+----------+----------
dolphin   |mammals   |water     
----------+----------+----------
hippo     |mammals   |water     
```
(check the explanation on `liftBoolFun2` [here](https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Algebra-Function.html) FIXMEHL).

Finally if we join a third relation:
```sh
*> viewAll r_zoo
A#        |ANIMAL    
----------+----------
0         |platypus  
----------+----------
1         |ostrich   
----------+----------
2         |crocodile 
----------+----------
2         |dolphin   
----------+----------
3         |horse     
----------+----------
5         |hippo     
----------+----------
7         |kangaroo  
----------+----------
10        |lion      
----------+----------
13        |flamingo  
----------+----------
22        |pelican   
----------+----------
123       |turtle    
```
```haskell
r_joinall = r_join `join` r_zoo
```
... we can summarize and get the class x habitat pairs living at the zoo:
```haskell
r_summary = summarize r_joinall (project r_joinall ["CLASS", "HABITAT"]) [Count] ["A#"] ["ZOOTOTAL"]
```
```sh
*> viewAll r_summary
CLASS     |HABITAT   |ZOOTOTAL  
----------+----------+----------
-nil-     |water     |1         
----------+----------+----------
birds     |air       |2         
----------+----------+----------
birds     |land      |1         
----------+----------+----------
birds     |water     |1         
----------+----------+----------
mammals   |land      |4         
----------+----------+----------
mammals   |water     |2         
----------+----------+----------
reptiles  |land      |1         
----------+----------+----------
reptiles  |water     |2         
```

Check [Basics.hs]() FIXMEHL program and call `main` to run the examples above. And read the Haddock documentation for [all other operators](https://janthelme.github.io/relation-tool/relation-tool-0.1.0.0/Algebra.html) FIXMEHL. 

## CSV imports

Given the filepath, a delimiter, the column names (*labels*) and their types, inport a csv file into a Relvar:
```haskell
import qualified Relvar.CSV as CSV (CSVSettings(..), toRelation)
-- ...
main :: IO ()
main = do 
    r <- CSV.toRelation "./olympics2016_medalists.csv" ';' ["MEDALIST","COUNTRY","ISTEAM"] [tyS, tyS, tyB]
    viewAll r
```


See CSV.hs for more details FIXMEHL.

## Other examples
See C.J Date's book examples here FIXMEHLs.

## Test
See Spec.hs. With Stack, run `stack build --test` from the command shell.

## Built With

* [Stack](https://docs.haskellstack.org/en/stable/README/)


Github: https://github.com/JAnthelme/relation-toolbox