> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> module Queries where
>
> import MngrTable
> 
> import           Prelude hiding (sum)
>
> import           Opaleye (Column, 
>                          Table(Table), required, queryTable,
>                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
>                          (.++), ifThenElse, pgString, pgDouble, aggregate, groupBy,
>                          count, avg, sum, leftJoin, runQuery,
>                          showSqlForPostgres, Unpackspec,
>                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool,
>                          pgDay)
> import qualified Opaleye.Aggregate as A
> import           Opaleye.Aggregate (Aggregator, aggregate)
> import           Data.Profunctor (dimap, lmap, rmap)
> import           Data.Profunctor.Product (p3)
> import           Data.Profunctor.Product.Default (Default, def)
> import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
> import           Data.Time.Calendar
> import           Control.Arrow (returnA, (<<<))
> import qualified Opaleye.Internal.Unpackspec as U

Key words: BUG, TODO, NOTE

--------------------------------------------------
Types

> type ColumnManagerNetWorth = Manager' (Column PGText) (Column PGFloat8)
> type ColumnQrtrlyMngrNetWorth = History' (Column PGDate) ColumnManagerNetWorth

> type ColumnQrtrlyNetWorth = History' (Column PGDate) (Column PGFloat8)

> type ColumnManagerPcntNetWorth = Manager' (Column PGText) (Column PGFloat8)
> type ColumnQrtrlyMngrPcntNetWorth = History' (Column PGDate) ColumnManagerPcntNetWorth
> 
> type ManagerPcntNetWorth = Manager' String Double
> type QrtrlyMngrPcntNetWorth = History' Day ManagerPcntNetWorth

> type MngrSectorPcnt = Manager' (Column PGText)  (ColumnSecuritySector, Column PGFloat8)
> type ColumnQrtrlyMngrSectorPct = History' (Column PGDate) MngrSectorPcnt


This query should report, by manager, quarter, and sector, the
   percentage of the aggregate value of a manager's securities for each
   sector in which a manager owns securities. This query should utilize the
   sector of the underlying for option securities, and should allocate
   issuers without a sector to an unknown sector.

--------------------------------------------------
Sql

Query - qMngrPcntNetWorthHist

> qMngrPcntNetWorthHist :: Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrPcntNetWorth
> qMngrPcntNetWorthHist qry = proc () -> do 
>    (History dateMngr (Manager name netWorthMngr))  <- qMngrNetWorthHist qry -< ()
>    (History dateHist netWorthHist) <- qHistNetWorth $ qMngrNetWorthHist qry -< ()
> 
>    restrict -< dateHist .== dateMngr
> 
>    returnA -< History dateMngr (Manager name (netWorthMngr / netWorthHist))

Query - qHistMngrSectorPcnt

> qHistMngrSectorPcnt :: Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrSectorPct
> qHistMngrSectorPcnt qry = proc () -> do 
>    (History dateMngr (Manager name security))  <- qry -< ()
>    
>    (History dateSector (SecuritySector nameSector, netWorthSector))  <- qHistSectorNetWorth qry -< ()
>    (History dateSector' (Manager nameMngr (SecuritySector nameSector', mngrNetWorthSector)))  <- qHistMngrSectorNetWorth qry -< ()
> 
>    restrict -< dateSector .== dateSector'
> 
>    returnA -< History dateMngr (Manager nameMngr (SecuritySector nameSector, mngrNetWorthSector / netWorthSector))


Queries- miscellaneous


> ERROR HERE!!
> type ColumnHistSectorNetWorth = History' (Column PGDate) (ColumnSecuritySector, Column PGFloat8)
> qHistSectorNetWorth :: Query ColumnQrtrlyMngrSec -> Query ColumnHistSectorNetWorth
> qHistSectorNetWorth qry = 
>   aggregate (pHistory $ History
>                 { histDate = groupBy
>                 , histValue = pManager $ Manager
>                      { mngrName = groupBy
>                      , mngrSecurCol = aSecColNetWorth  }}) 
>            qry

> ERROR HERE!!
> type ColumnHistoryMngrSectorNetWorth 
>      = History' (Column PGDate) 
>                 (Manager' (Column PGText) 
>                           (ColumnSecuritySector, Column PGFloat8)) 
> qHistMngrSectorNetWorth :: Query ColumnQrtrlyMngrSec -> Query ColumnHistoryMngrSectorNetWorth
> qHistMngrSectorNetWorth qry =
>   aggregate (pHistory $ History
>                 { histDate = groupBy
>                 , histValue = pManager $ Manager
>                      { mngrName = groupBy
>                      , mngrSecurCol = aSecColNetWorth  }}) 
>            qry



> qHistNetWorth :: Query ColumnQrtrlyMngrNetWorth -> Query ColumnQrtrlyNetWorth  -- TODO: QueryArr ... 
> qHistNetWorth qry = 
>   aggregate (pHistory $ History 
>                 { histDate = groupBy
>                 , histValue = aHistColNetWorth })
>              qry

> qMngrNetWorthHist ::  Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrNetWorth
> qMngrNetWorthHist qry = 
>   aggregate (pHistory $ History
>                 { histDate = groupBy
>                 , histValue = pManager $ Manager
>                      { mngrName = groupBy
>                      , mngrSecurCol = aSecColNetWorth  }}) 
>             qry


Aggregators

> aSecColNetWorth :: Aggregator ColumnSecurity (Column PGFloat8)
> aSecColNetWorth = lmap (\sec ->  val sec * quant sec) sum
>   where val   = securityValue . secValue
>         quant = securityQuant . secQuant

> aHistColNetWorth :: Aggregator (Manager' (Column PGText) (Column PGFloat8)) (Column PGFloat8)
> aHistColNetWorth = lmap (\mngr -> mngrSecurCol mngr) sum
