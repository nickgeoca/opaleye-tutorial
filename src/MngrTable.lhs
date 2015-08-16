> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> module MngrTable where
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
> type SecuritySector       = SecuritySector' String
> type ColumnSecuritySector = SecuritySector' (Column PGText)

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

> type Security       = Security'       SecurityName       SecurityQuant       SecurityValue SecuritySector
> type ColumnSecurity = Security' ColumnSecurityName ColumnSecurityQuant ColumnSecurityValue ColumnSecuritySector

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

> qMngrSecHist :: Query ColumnQrtrlyMngrSec
> qMngrSecHist = queryTable tMngrSecHist



