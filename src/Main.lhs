> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> module TutorialManipulation where
>
> import           Prelude hiding (sum)
>
> import           Opaleye (Column, Nullable, matchNullable, isNull,
>                          Table(Table), required, queryTable,
>                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
>                          (.++), ifThenElse, pgString, aggregate, groupBy,
>                          count, avg, sum, leftJoin, runQuery,
>                          showSqlForPostgres, Unpackspec,
>                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool,
>                          pgDay)
> import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
> import           Data.Profunctor.Product (p3)
> import           Data.Profunctor.Product.Default (Default, def)
> import           Data.Time.Calendar
> import qualified Opaleye.Internal.Unpackspec as U

Key words: BUG, TODO, NOTE

--------------------------------------------------
Cached types

> -- Container for plain/regular data values and the cache values
> data Cache' v c = Cache
>  { dataVal  :: v 
>  , cacheVal :: c } 

> type CacheSumWorth v = Cache' v (Column (Nullable PGFloat8))

> class Cache v c where
>   update :: v -> c -> String        -- Update data value, then cache value
>   -- getCache :: a -> a -- If null, then calculate cache value, TODO: else default cache value? e.g. security holdings $0.. see Data.Default 
>   -- calculate :: a     -- Calculate cache value based on data value

--------------------------------------------------
Types

Security

> -- example (Security "Co1 StockA" 40 100, 
> --          Security "Co1 StockB" 10 5000)
> data Security' a b c = Security
>  { secName  :: a
>  , secCount :: b
>  , secValue :: c }
> 
> type Security       = Security' String Int Int
> 
> type SecurityColumn = Security' (Column PGText) (Column PGInt4) (Column PGInt4)

Manager

> data Manager' a b = Manager 
>  { manName   :: a
>  , manSecurs :: b }
> 
> type Manager       = Manager' String [Security]
> 
> type ManagerColumn = Manager' (Column PGText) (Column SecurityColumn)
> 
> type ManagerColumnC = CacheSumWorth ManagerColumn  -- Cache each managers total 

History

> -- Container for data and a date
> data History' a b = History
>  { histDate  :: a
>  , histValue :: b }
> 
> type HistMngrSecColumn = History' (Column PGDate) (Column ManagerColumnC)
> 
> type HistMngrSecColumnC = CacheSumWorth HistMngrSecColumn -- Cache each quarterly total

--------------------------------------------------
Table

> $(makeAdaptorAndInstance "pCache" ''Cache')   
> $(makeAdaptorAndInstance "pHistory" ''History')   

> mngrSecHistTable :: Table HistMngrSecColumnC
>                           HistMngrSecColumnC
> mngrSecHistTable = Table "mngrSecHistTable" 
>                    (pCache Cache { dataVal  = pHistory (History (required "quarter") 
>                                                                 (required "managers"))
>                                  , cacheVal = required "quarterlyTotal" })

--------------------------------------------------
Queries

> mngrSecHistQuery :: Query HistMngrSecColumnC
> mngrSecHistQuery = queryTable mngrSecHistTable

> restrictDate :: Day -> QueryArr (History' (Column PGDate) a) ()
> restrictDate date = proc history -> do
>   restrict -< pgDay date .== histDate history
> 
> restrictMngr :: String -> QueryArr (Manager' (Column PGText) (Column a)) ()
> restrictMngr mngr = proc managers -> do 
>   restrict -< pgString mngr .== manName managers
> 
> 
> 

> mngrPercentQuery :: Date -> String -> QueryArr HistMngrSecColumnC (Column PGText, Column PGFloat8)
> mngrPercentQuery date mngrId = proc (Cache d c) -> do

if managerCache == Null then CalcCache and put calc in cache; return cache 
                        else return cache
if dateCache == Null then CalcCache and put calc in cache; return cache 
                        else return cache
[managerCache] / dateCache 


   restrictDate date   -< d
   restrictMngr mngrId -< 

bossQuery :: QueryArr (Column PGText, Column (Nullable PGText)) (Column PGText)
bossQuery = proc (name, nullableBoss) -> do
  returnA -< matchNullable (name .++ pgString " has no boss")
                           (\boss -> pgString "The boss of " .++ name
                                     .++ pgString " is " .++ boss)
                           nullableBoss

query (manager, quarter)

type QueryComp a = QuerryArr a ()

QueryComp 

managerQuarter :: Query ManagerQuarter
managerQuarter = proc () -> do
   
--------------------------------------------------
Maybe add this to code
newtype ManagerId = ManagerId { managerId :: String} 

--------------------------------------------------
Questions
code below doesn't work b/c (Eq a) can't be restricted on a@(Column a0) 
restrictDate :: Eq a => a -> QueryArr (History' a b) ()
