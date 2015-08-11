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

Cabal Repl
 conn <- connect ConnectInfo { connectHost="127.0.0.1",connectPort=5432,connectUser="postgres",connectPassword="password",connectDatabase = "managers" }
 a <- runQuery conn secQuery :: IO [Security]
 mapM_ (putStrLn.show) a

--------------------------------------------------
Types

Security

> data SecurityName' a = SecurityName a deriving Show
> $(makeAdaptorAndInstance "pSecurityName" ''SecurityName')
> type ColumnSecurityName = SecurityName' (Column PGText)
> type SecurityName       = SecurityName' String

> data SecurityQuant' a = SecurityQuant a deriving Show
> $(makeAdaptorAndInstance "pSecurityQuant" ''SecurityQuant')
> type ColumnSecurityQuant = SecurityQuant' (Column PGInt4)
> type SecurityQuant       = SecurityQuant' Int

> data SecurityValue' a = SecurityValue a deriving Show
> $(makeAdaptorAndInstance "pSecurityValue" ''SecurityValue')
> type ColumnSecurityValue = SecurityValue' (Column PGFloat8)
> type SecurityValue       = SecurityValue' Double

> data SecuritySector' a = SecuritySector a deriving Show
> $(makeAdaptorAndInstance "pSecuritySector" ''SecuritySector')
> type ColumnNullableSecuritySector = SecuritySector' (Column (Nullable PGText))
> type SecuritySector               = SecuritySector' (Maybe String)

> data Security' a b c d = Security
>  { secName   :: a
>  , secQuant  :: b
>  , secValue  :: c 
>  , secSector :: d } deriving Show
> $(makeAdaptorAndInstance "pSecurity" ''Security')
> type Security = Security' SecurityName SecurityQuant SecurityValue SecuritySector
> type ColumnSecurity = Security' ColumnSecurityName ColumnSecurityQuant ColumnSecurityValue ColumnNullableSecuritySector

Manager

> data Manager' a b = Manager 
>  { manName   :: a
>  , manSecurs :: b }
> 
> type ColumnManager = Manager' (Column PGText) (Column ColumnSecurity)

History

> -- Container for data and a date
> data History' a b = History
>  { histDate  :: a
>  , histValue :: b }
> 
> type ColumnQrtrlyMngrSec = History' (Column PGDate) (Column ColumnManager)

--------------------------------------------------
Table

> $(makeAdaptorAndInstance "pHistory" ''History')   

> mngrSecHistTable :: Table ColumnQrtrlyMngrSec
>                           ColumnQrtrlyMngrSec
> mngrSecHistTable = Table "mngrSecHistTable" 
>                    (pHistory $ History { histDate  = required "quarter"
>                                        , histValue = required "managers" })

--------------------------------------------------
Queries

> mngrSecHistQuery :: Query ColumnQrtrlyMngrSec
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


> secTable :: Table ColumnSecurity
>                   ColumnSecurity
> secTable = Table "security" 
>            (pSecurity $ Security { secName   = pSecurityName   $ SecurityName   $ required "name"
>                                  , secQuant  = pSecurityQuant  $ SecurityQuant  $ required "quant"
>                                  , secValue  = pSecurityValue  $ SecurityValue  $ required "value"
>                                  , secSector = pSecuritySector $ SecuritySector $ required "sector" } ) 

> secQuery :: Query ColumnSecurity
> secQuery = queryTable secTable


mngrPercentQuery :: Date -> String -> QueryArr QrtrlyMngrSecColumn (Column PGText, Column PGFloat8)
mngrPercentQuery date mngrId = proc (Cache d c) -> do

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
   
> printSql :: Default Unpackspec a a => Query a -> IO ()
> printSql = putStrLn . showSqlForPostgres

--------------------------------------------------
Maybe add this to code
newtype ManagerId = ManagerId { managerId :: String} 

--------------------------------------------------
Questions
code below doesn't work b/c (Eq a) can't be restricted on a@(Column a0) 
restrictDate :: Eq a => a -> QueryArr (History' a b) ()


