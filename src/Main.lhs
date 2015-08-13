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
> import           Control.Arrow (returnA, (<<<))
> import qualified Opaleye.Internal.Unpackspec as U

Key words: BUG, TODO, NOTE

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
>  , mngrSecurCol :: b } deriving Show -- TODO: Rename this to mngrValue? mngrSecurCol is being applied elsewhere without that context
> $(makeAdaptorAndInstance "pManager" ''Manager')
> manager' name sec = pManager $ Manager { mngrName = required name, mngrSecurCol = sec }
> type Manager       = Manager'         String        Security
> type ColumnManager = Manager' (Column PGText) ColumnSecurity

History

> data History' a b = History
>  { histDate  :: a
>  , histValue :: b } deriving Show
> 
> type QrtrlyMngrSec       = History'         Day           Manager
> type ColumnQrtrlyMngrSec = History' (Column PGDate) ColumnManager

--------------------------------------------------
Sql

Tables

> $(makeAdaptorAndInstance "pHistory" ''History')   

> tMngrSecHist :: Table ColumnQrtrlyMngrSec
>                           ColumnQrtrlyMngrSec
> tMngrSecHist = Table "tMngrSecHist" 
>    (pHistory $ History 
>        { histDate  = required "quarter"  
>        , histValue = manager' "mngr_name" (security' "sec_name" "sec_quant" "sec_value" "sec_sector")  -- TODO: Put column names in type section?
>        }
>    )

Queries

> type ColumnManagerNetWorth = Manager' (Column PGText) (Column PGFloat8)
> type ColumnQrtrlyMngrNetWorth = History' (Column PGDate) ColumnManagerNetWorth

> type ColumnQrtrlyNetWorth = History' (Column PGDate) (Column PGFloat8)

> type ColumnManagerPcntNetWorth = Manager' (Column PGText) (Column PGFloat8)
> type ManagerPcntNetWorth = Manager' String Double
> type ColumnQrtrlyMngrPcntNetWorth = History' (Column PGDate) ColumnManagerPcntNetWorth
> type QrtrlyMngrPcntNetWorth = History' Day ManagerPcntNetWorth


> qMngrSecHist :: Query ColumnQrtrlyMngrSec
> qMngrSecHist = queryTable tMngrSecHist

> qMngrNetWorthHist ::  Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrNetWorth
> qMngrNetWorthHist qry = 
>   aggregate (pHistory $ History
>                 { histDate = groupBy
>                 , histValue = pManager $ Manager
>                      { mngrName = groupBy
>                      , mngrSecurCol = aSecColNetWorth  }}) 
>             qry


> qHistNetWorth :: Query ColumnQrtrlyMngrNetWorth -> Query ColumnQrtrlyNetWorth  -- TODO: QueryArr ... 
> qHistNetWorth qry = 
>   aggregate (pHistory $ History 
>                 { histDate = groupBy
>                 , histValue = aHistColNetWorth })
>              qry

> qMngrPcntNetWorthHist :: Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrPcntNetWorth
> qMngrPcntNetWorthHist qry = proc () -> do 
>    (History dateMngr (Manager name netWorthMngr))  <- mngrNetWorthHist -< ()
>    (History dateHist netWorthHist) <- histNetWorth -< ()
> 
>    restrict -< dateHist .== dateMngr
> 
>    returnA -< History dateMngr (Manager name (netWorthMngr / netWorthHist))
>      where mngrNetWorthHist = qMngrNetWorthHist qry
>            histNetWorth = qHistNetWorth mngrNetWorthHist

Aggregators

> aSecColNetWorth :: Aggregator ColumnSecurity (Column PGFloat8)
> aSecColNetWorth = lmap (\sec ->  val sec * quant sec) sum
>   where val   = securityValue . secValue
>         quant = securityQuant . secQuant

> aHistColNetWorth :: Aggregator (Manager' (Column PGText) (Column PGFloat8)) (Column PGFloat8)
> aHistColNetWorth = lmap (\mngr -> mngrSecurCol mngr) sum

--------------------------------------------------
Util functions

> printSql :: Default Unpackspec a a => Query a -> IO ()
> printSql = putStrLn . showSqlForPostgres


--------------------------------------------------
Miscelaneous

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


--------------------------------------------------
Testin & Cabal Repl

Cabal Repl
 import Database.PostgreSQL.Simple.Internal
 conn <- connect ConnectInfo { connectHost="127.0.0.1",connectPort=5432,connectUser="postgres",connectPassword="password",connectDatabase = "managers" }
 a <- runQuery conn secQuery :: IO [Security]
 mapM_ print a
 abc <- runQuery conn $  aggregate secColNetWorth secQuery :: IO [Double]

Testing
 create table tMngrSecHist (quarter date, mngr_name text, sec_name text, sec_quant float8, sec_value float8, sec_sector text);
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','Paul','Amazon', 50, 10.5, 'tech');
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','Paul','Amazon', 10, 5.5, 'tech');
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','George','NEM', 200, 150.12, 'tech');
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','George','Alibaba', 800, 90.12, 'tech');

 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','Amazon', 250, 100.5, 'tech');
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','Amazon', 1000, 150.5, 'tech');
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','Google', 100, 50.12, 'tech');
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','John','Google', 1000, 150.5, 'tech');

 History {histDate = 2014-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = 5.6475170399221e-3}}
 History {histDate = 2014-01-01, histValue = Manager {mngrName = "George", mngrSecurCol = 0.994352482960078}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = 0.545505334650009}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "John", mngrSecurCol = 0.454494665349991}}

