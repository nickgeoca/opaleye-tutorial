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
>                          (.++), ifThenElse, pgString, pgDouble, aggregate, groupBy,
>                          count, avg, sum, leftJoin, runQuery,
>                          showSqlForPostgres, Unpackspec,
>                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool,
>                          pgDay)
> import qualified Opaleye.Aggregate as A
> import           Opaleye.Aggregate (Aggregator, aggregate)
> import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
> import           Data.Profunctor (dimap, lmap, rmap)
> import           Data.Profunctor.Product (p3)
> import           Data.Profunctor.Product.Default (Default, def)
> import           Data.Time.Calendar
> import qualified Opaleye.Internal.Unpackspec as U

Key words: BUG, TODO, NOTE

Cabal Repl
 import Database.PostgreSQL.Simple.Internal
 conn <- connect ConnectInfo { connectHost="127.0.0.1",connectPort=5432,connectUser="postgres",connectPassword="password",connectDatabase = "managers" }
 a <- runQuery conn secQuery :: IO [Security]
 mapM_ print a
 abc <- runQuery conn $  aggregate secColNetWorth secQuery :: IO [Double]


--------------------------------------------------
Types

Security

> data SecurityName' a = SecurityName a deriving Show
> securityName (SecurityName x) = x
> $(makeAdaptorAndInstance "pSecurityName" ''SecurityName')
> securityName' s = pSecurityName $ SecurityName $ required s
> type ColumnSecurityName = SecurityName' (Column PGText)
> type SecurityName       = SecurityName' String

> data SecurityQuant' a = SecurityQuant a deriving Show
> securityQuant (SecurityQuant x) = x
> $(makeAdaptorAndInstance "pSecurityQuant" ''SecurityQuant')
> securityQuant' s = pSecurityQuant $ SecurityQuant $ required s
> type ColumnSecurityQuant = SecurityQuant' (Column PGFloat8)
> type SecurityQuant       = SecurityQuant' Double

> data SecurityValue' a = SecurityValue a deriving Show
> securityValue (SecurityValue x) = x
> $(makeAdaptorAndInstance "pSecurityValue" ''SecurityValue')
> securityValue' s = pSecurityValue $ SecurityValue $ required s
> type ColumnSecurityValue = SecurityValue' (Column PGFloat8)
> type SecurityValue       = SecurityValue' Double

> data SecuritySector' a = SecuritySector a deriving Show
> securitySector (SecuritySector x) = x
> $(makeAdaptorAndInstance "pSecuritySector" ''SecuritySector')
> securitySector' s = pSecuritySector $ SecuritySector $ required s
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
>  { mngrName     :: a
>  , mngrSecurCol :: b }
> 
> type ColumnManager = Manager' (Column PGText) (Column ColumnSecurity)

History

> data History' a b = History
>  { histDate  :: a
>  , histValue :: b }
> 
> type ColumnQrtrlyMngrSec = History' (Column PGDate) (Column ColumnManager)

--------------------------------------------------
Tables

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

> mngrNetWorthHist :: Query (History' (Column PGDate) 
>                                     (Column ColumnManagerNetWorth))
> mngrNetWorthHist = 
>    aggregate (pHistory $ History { histDate = groupBy
>                                  , histValue = mngrColNetWorth })
>              mngrSecHistQuery

--------------------------------------------------
Aggregators

> -- TODO: Return PGFloat8 instead of (Column PGFloat8)
> secColNetWorth :: Aggregator ColumnSecurity (Column PGFloat8)
> secColNetWorth = lmap (\sec ->  val sec * quant sec) sum
>   where val   = securityValue . secValue 
>         quant = securityQuant . secQuant 

> type ColumnManagerNetWorth = Manager' (Column PGText) (Column (Column PGFloat8))


> mngrColNetWorth :: Aggregator (Column ColumnManager) (Column ColumnManagerNetWorth)
> mngrColNetWorth = lmap (\(Manager name secCol) -> undefined) groupBy

(\colMngr -> fn1 colMngr


fn1 :: Column ColumnManager -> Column ColumnManagerNetWorth 
fn1 colMngrCol = fmap colMngrCol

mngrColNetWorth :: Aggregator ColumnManager (Manager' (Column PGText) (Column (Column PGFloat8)))
mngrColNetWorth = secColNetWorth

mngrColNetWorth :: Aggregator ColumnManager (Manager' (Column PGText) (Column (Column PGFloat8)))
mngrColNetWorth = dimap (\mngr -> mngrSecurCol mngr) (\f-> Manager (pgString "hey") f) secColNetWorth

    Expected type: Aggregator (Column ColumnSecurity) (Column (Column PGFloat8))
      Actual type: Aggregator ColumnSecurity (Column PGFloat8)
   
--------------------------------------------------
Util functions

> printSql :: Default Unpackspec a a => Query a -> IO ()
> printSql = putStrLn . showSqlForPostgres



-- ##################################################

> secTable :: Table ColumnSecurity
>                   ColumnSecurity
> secTable = Table "security" 
>            (pSecurity $ Security { secName   = securityName'   "name"
>                                  , secQuant  = securityQuant'  "quant"
>                                  , secValue  = securityValue'  "value"
>                                  , secSector = securitySector' "sector" 
>                                  }) 

> secQuery :: Query ColumnSecurity
> secQuery = queryTable secTable


> restrictDate :: Day -> QueryArr (History' (Column PGDate) a) ()
> restrictDate date = proc history -> do
>   restrict -< pgDay date .== histDate history
> 
> restrictMngr :: String -> QueryArr (Manager' (Column PGText) (Column a)) ()
> restrictMngr mngr = proc managers -> do 
>   restrict -< pgString mngr .== mngrName managers
