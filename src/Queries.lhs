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
>                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, values,
>                          pgDay)
> import qualified Opaleye.Aggregate as A
> import           Opaleye.Aggregate (Aggregator, aggregate)
> import           Data.Profunctor (dimap, lmap, rmap)
> import           Data.Profunctor.Product (p3, p4)
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

> type ColumnMngrSectorPcnt      = Manager' (Column PGText)  (ColumnSecuritySector, Column PGFloat8)
> type ColumnQrtrlyMngrSectorPct = History' (Column PGDate)   ColumnMngrSectorPcnt

> type MngrSectorPcnt = Manager' String (SecuritySector, Double)
> type QrtrlyMngrSectorPct = History' Day MngrSectorPcnt

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
>    (History date  (Manager nameMngr netWorthMngr))  <- qMngrNetWorthHist qry -< ()
>    (History date' netWorthDate) <- qHistNetWorth $ qMngrNetWorthHist qry -< ()
> 
>    restrict -< date .== date'
> 
>    let mngrPcntNetWorth = netWorthMngr / netWorthDate
>    returnA -< History date $ Manager nameMngr mngrPcntNetWorth

Query - qHistMngrSectorPcnt

> qHistMngrSectorPcnt :: Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrSectorPct
> qHistMngrSectorPcnt qry = proc () -> do 
>    (History date  (Manager nameMngr (SecuritySector nameSector, mngrNetWorthSector))) <- qHistMngrSectorNetWorth qry -< ()
>    (History date' (SecuritySector nameSector', netWorthSector))                       <- qHistSectorNetWorth $ qHistMngrSectorNetWorth qry -< ()
> 
>    restrict -< date .== date'
>    restrict -< nameSector .== nameSector'
> 
>    let mngrSectorPcntNetWorth = mngrNetWorthSector / netWorthSector
>    returnA -< History date $ Manager nameMngr (SecuritySector nameSector, mngrSectorPcntNetWorth)
>      -- where fn = proc q -> do   aggregate (p3 (groupBy, groupBy, sum)) q -< ()      -- >    fn -< (date, nameSector, mngrNetWorthSector)
 
Queries- miscellaneous

> type ColumnHistSectorNetWorth = History' (Column PGDate) (ColumnSecuritySector, Column PGFloat8)
> 
> qHistSectorNetWorth :: Query ColumnHistoryMngrSectorNetWorth -> Query ColumnHistSectorNetWorth
> qHistSectorNetWorth qry = proc () -> do
>   (date, sectorName, sectorValue) <- aggregate aSectorSum (alter qry) -< () 
> 
>   returnA -< History date (SecuritySector sectorName, sectorValue)
> 
>   where alter qry = proc () -> do 
>           (History date (Manager _ (SecuritySector sectorName, sectorValue))) <- qry -< ()
>           returnA -<    (date   , sectorName, sectorValue)
>         aSectorSum = p3 (groupBy, groupBy   , sum)

> type ColumnHistoryMngrSectorNetWorth 
>      = History' (Column PGDate) 
>                 (Manager' (Column PGText) 
>                           (ColumnSecuritySector, Column PGFloat8)) 
> 
> qHistMngrSectorNetWorth :: Query ColumnQrtrlyMngrSec -> Query ColumnHistoryMngrSectorNetWorth
> qHistMngrSectorNetWorth qry = proc () -> do
>   (date, mngrName, sectorName, sectorValue) <- aggregate aSectorSum (alter qry) -< () 
>   returnA -< History date (Manager mngrName (SecuritySector sectorName, sectorValue))
> 
>   where alter qry = proc () -> do 
>           (History date (Manager mngrName (Security _
>                                               (SecurityQuant secQuant) 
>                                               (SecurityValue secValue) 
>                                               (SecuritySector secSector)))) <- qry -< ()
>           returnA -<    (date   , mngrName, secSector, (secQuant, secValue))
>         aSectorSum = p4 (groupBy, groupBy , groupBy  , aDotProduct fst snd)

> qHistNetWorth :: Query ColumnQrtrlyMngrNetWorth -> Query ColumnQrtrlyNetWorth  -- TODO: QueryArr ... 
> qHistNetWorth qry = 
>   aggregate (pHistory $ History 
>                 { histDate = groupBy
>                 , histValue = lmap mngrSecurCol sum })
>              qry

> qMngrNetWorthHist ::  Query ColumnQrtrlyMngrSec -> Query ColumnQrtrlyMngrNetWorth
> qMngrNetWorthHist qry = 
>   aggregate (pHistory $ History
>                 { histDate = groupBy
>                 , histValue = pManager $ Manager
>                      { mngrName = groupBy
>                      , mngrSecurCol = aDotProduct value quant }}) 
>             qry
>   where value = securityValue . secValue
>         quant = securityQuant . secQuant

Aggregators

> aDotProduct :: (a -> Column PGFloat8)    -- TODO: Inlcude class constraint: Opaleye.Internal.Column.PGNum a1 
>             -> (a -> Column PGFloat8) 
>             -> Aggregator a (Column PGFloat8)
> aDotProduct f1 f2 = lmap (\x ->  f1 x * f2 x) sum
