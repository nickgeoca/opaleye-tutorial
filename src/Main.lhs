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
> type SecurityName       = SecurityName' String
> type ColumnSecurityName = SecurityName' (Column PGText)

> data SecurityQuant' a = SecurityQuant a deriving Show
> securityQuant (SecurityQuant x) = x
> $(makeAdaptorAndInstance "pSecurityQuant" ''SecurityQuant')
> securityQuant' s = pSecurityQuant $ SecurityQuant $ required s
> type SecurityQuant       = SecurityQuant' Double
> type ColumnSecurityQuant = SecurityQuant' (Column PGFloat8)

> data SecurityValue' a = SecurityValue a deriving Show
> securityValue (SecurityValue x) = x
> $(makeAdaptorAndInstance "pSecurityValue" ''SecurityValue')
> securityValue' s = pSecurityValue $ SecurityValue $ required s
> type SecurityValue       = SecurityValue' Double
> type ColumnSecurityValue = SecurityValue' (Column PGFloat8)

> data SecuritySector' a = SecuritySector a deriving Show
> securitySector (SecuritySector x) = x
> $(makeAdaptorAndInstance "pSecuritySector" ''SecuritySector')
> securitySector' s = pSecuritySector $ SecuritySector $ required s
> type SecuritySectorM              = SecuritySector' (Maybe String)
> type ColumnNullableSecuritySector = SecuritySector' (Column (Nullable PGText))

> data Security' a b c d = Security
>  { secName   :: a
>  , secQuant  :: b
>  , secValue  :: c 
>  , secSector :: d } deriving Show
> $(makeAdaptorAndInstance "pSecurity" ''Security')
> security' n q v s = pSecurity $ Security { secName   = securityName' n
>                                          , secQuant  = securityQuant' q
>                                          , secValue  = securityValue' v
>                                          , secSector = securitySector' s
>                                          }

> type Security       = Security'       SecurityName       SecurityQuant       SecurityValue               SecuritySectorM
> type ColumnSecurity = Security' ColumnSecurityName ColumnSecurityQuant ColumnSecurityValue ColumnNullableSecuritySector

Manager

> data Manager' a b = Manager 
>  { mngrName     :: a
>  , mngrSecurCol :: b }
> $(makeAdaptorAndInstance "pManager" ''Manager')
> manager' name sec = pManager $ Manager { mngrName = required name, mngrSecurCol = sec }
> type Manager       = Manager'         String        Security
> type ColumnManager = Manager' (Column PGText) ColumnSecurity

History

> data History' a b = History
>  { histDate  :: a
>  , histValue :: b }
> 
> type QrtrlyMngrSec       = History'         Day           Manager
> type ColumnQrtrlyMngrSec = History' (Column PGDate) ColumnManager

--------------------------------------------------
Tables

> $(makeAdaptorAndInstance "pHistory" ''History')   

> mngrSecHistTable :: Table ColumnQrtrlyMngrSec
>                           ColumnQrtrlyMngrSec
> mngrSecHistTable = Table "mngrSecHistTable" 
>                    (pHistory $ History { histDate  = required "quarter"  
>                                        , histValue = manager' "manager" (security' "name" "quant" "value" "sector")  -- TODO: Put column names in type section?
>                                        })

--------------------------------------------------
Queries

> type ColumnManagerNetWorth = Manager' (Column PGText) (Column PGFloat8)
> type ColumnQrtrlyMngrNetWorth   = History' (Column PGDate) ColumnManagerNetWorth

> mngrSecHistQuery :: Query ColumnQrtrlyMngrSec
> mngrSecHistQuery = queryTable mngrSecHistTable

> -- TODO: QueryArr ColumnQrtrlyMngrSec ColumnQrtrlyMngrNetWorth
> mngrNetWorthHist :: Query ColumnQrtrlyMngrNetWorth
> mngrNetWorthHist = 
>   aggregate (pHistory $ History { histDate = groupBy
>                                 , histValue = pManager $ Manager { mngrName = groupBy
>                                                                  , mngrSecurCol = secColNetWorth 
>                                                                  }
>                                 })
>             mngrSecHistQuery
BUG HERE: Move aggeragator in own section?

--------------------------------------------------
Aggregators

> -- TODO: Return PGFloat8 instead of (Column PGFloat8)
> secColNetWorth :: Aggregator ColumnSecurity (Column PGFloat8)
> secColNetWorth = lmap (\sec ->  val sec * quant sec) sum
>   where val   = securityValue . secValue
>         quant = securityQuant . secQuant



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

           (pSecurity $ Security { secName   = pSecurityName $ SecurityName $ required "name"
                                 , secQuant  = securityQuant'  "quant"
                                 , secValue  = securityValue'  "value"
                                 , secSector = securitySector' "sector" 
                                 }) 

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
