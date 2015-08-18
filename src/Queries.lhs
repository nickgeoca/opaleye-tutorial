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
>                          Table(Table), required, queryTable, fromNullable, toNullable, Nullable, matchNullable,
>                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<), 
>                          (.++), ifThenElse, pgString, pgDouble, aggregate, groupBy,
>                          count, avg, sum, leftJoin, runQuery,
>                          showSqlForPostgres, Unpackspec,
>                          PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, values,
>                          pgDay)
> import qualified Opaleye.Aggregate as A
> import           Opaleye.Aggregate (Aggregator, aggregate)
> import           Data.Profunctor (dimap, lmap, rmap)
> import           Data.Profunctor.Product (p2, p3, p4)
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

> type ColNullText = Column (Nullable PGText)

> qHistMngrSectorPcnt :: Query ColumnQrtrlyMngrSec -> Query ColumnSecInfo -> Query ColumnQrtrlyMngrSectorPct
> qHistMngrSectorPcnt qMngr qSecInfo = proc () -> do 
> 
>   (cDate, cMngrName, cSector, cNetWorth) <- qHistMngrSectorVal -< ()
>   (cDate', cSector', cNetWorth')         <- qHistSectorVal     -< ()
>   
>   restrict -< cDate   .== cDate'
>   restrict -< cSector .== cSector'
>     
>   returnA -< History cDate (Manager cMngrName (SecuritySector cSector, cNetWorth / cNetWorth'))
> 
>   where joinFn (m, s) = (securityName.secName.mngrSecurCol.histValue $ m) .== (securityName.fst $ s)
>         qAlter1 qry = proc () -> do (History cDate (Manager cMngrName (Security cSecName cSecQuant cSecValue)), (SecurityName cNullSecName, SecuritySector cNullSector)) <- qry -< ()  
>                                     let cSector = fromNullable (pgString "unknown_sector")   cNullSector
>                                     returnA -< (cDate  , cMngrName, cSector, (cSecQuant, cSecValue))
>         aHistMngrSectorVal =              p4  (groupBy, groupBy  , groupBy, aDotProduct (securityQuant.fst) (securityValue.snd)) 
>         qAlter2 qry = proc () -> do (cDate, _, cSector, netWorth) <- qry -< ()  
>                                     returnA -< (cDate  , cSector, netWorth)
>         aHistSectorVal =                  p3   (groupBy, groupBy, sum) 
>         qJoin              = leftJoin qMngr qSecInfo joinFn :: Query (ColumnQrtrlyMngrSec, (SecurityName' ColNullText, SecuritySector' ColNullText))
>         qHistMngrSectorVal = aggregate aHistMngrSectorVal $ qAlter1 qJoin
>         qHistSectorVal     = aggregate aHistSectorVal     $ qAlter2 qHistMngrSectorVal

Queries- miscellaneous

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
