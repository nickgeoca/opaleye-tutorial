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
> import Prelude hiding (sum)
>
> import Opaleye (Column, Query,
>                 Nullable, fromNullable,
>                 restrict, (.==), ifThenElse, pgString, 
>                 sum, leftJoin, aggregate, groupBy,
>                 PGText, PGDate, PGFloat8)
> import Opaleye.Aggregate (Aggregator)
> import Data.Profunctor (lmap)
> import Data.Profunctor.Product (p3, p4)
> import Data.Time.Calendar
> import Control.Arrow (returnA)

Key words: BUG, TODO, NOTE

--------------------------------------------------
-- Types

Manager

> type ColManagerTotal = Manager' (Column PGText) (Column PGFloat8)
> 
> type ColManagerShare = Manager' (Column PGText) (Column PGFloat8)
> 
> type ManagerShare = Manager' String Double
> 
> type    ManagerSectorShare = Manager'         String   (SecuritySector   ,        Double)
> type ColManagerSectorShare = Manager' (Column PGText)  (ColSecuritySector, Column PGFloat8)

Quarterly
 
> type ColQuarterlyTotal = History' (Column PGDate) (Column PGFloat8)
> 
> type ColQuarterlyManagerTotal = History' (Column PGDate) ColManagerTotal
> 
> type    QuarterlyManagerShare = History'         Day     ManagerShare
> type ColQuarterlyManagerShare = History' (Column PGDate) ColManagerShare
> 
> type    QuarterlyManagerSectorShare = History'         Day        ManagerSectorShare
> type ColQuarterlyManagerSectorShare = History' (Column PGDate) ColManagerSectorShare


--   division

--------------------------------------------------
Sql

Query - qQuarterlyManagerShare

> qQuarterlyManagerShare :: Query ColQuarterlyManagerSecurity -> Query ColQuarterlyManagerShare
> qQuarterlyManagerShare qry = proc () -> do 
>    (History cDate  (Manager cManagerName cQuarterlyManagerTotal))  <- qQuarterlyManagerTotal qry -< ()
>    (History cDate' cQuarterlyTotal) <- qQuarterlyTotal qry -< ()
> 
>    restrict -< cDate .== cDate'
> 
>    let cQuarterlyManagerShare = cQuarterlyManagerTotal / cQuarterlyTotal
>    returnA -< History cDate $ Manager cManagerName cQuarterlyManagerShare

> qQuarterlyTotal :: Query ColQuarterlyManagerSecurity -> Query ColQuarterlyTotal
> qQuarterlyTotal qry = 
>   aggregate (pHistory $ History 
>                 { histDate = groupBy
>                 , histValue = lmap mngrSecurCol sum })
>              qQuarterlyManagerTotal'
>      where qQuarterlyManagerTotal' = qQuarterlyManagerTotal qry

> qQuarterlyManagerTotal ::  Query ColQuarterlyManagerSecurity -> Query ColQuarterlyManagerTotal
> qQuarterlyManagerTotal qry = 
>   aggregate (pHistory $ History
>                 { histDate = groupBy
>                 , histValue = pManager $ Manager
>                      { mngrName = groupBy
>                      , mngrSecurCol = aDotProduct value quantity }}) 
>             qry
>   where value    = securityValue . secValue
>         quantity = securityQuantity . secQuant

Query - qQuarterlyManagerSectorShare

> type ColNullText = Column (Nullable PGText)

> qQuarterlyManagerSectorShare :: Query ColQuarterlyManagerSecurity -> Query ColSecurityInformation -> Query ColQuarterlyManagerSectorShare
> qQuarterlyManagerSectorShare qQMS qSI = 
>   let joinSameSecurity (m, s)      = (securityName.secName.mngrSecurCol.histValue $ m) .== (securityName.fst $ s)
>       qJoinSameSecurity            = leftJoin qQMS qSI joinSameSecurity :: Query (ColQuarterlyManagerSecurity, (SecurityName' ColNullText, SecuritySector' ColNullText))
>       qQuarterlyManagerSectorTotal = aggregate aHistMngrSectorVal $ qAlter1 qJoinSameSecurity
>       qQuarterlySectorTotal        = aggregate aHistSectorVal     $ qAlter2 qQuarterlyManagerSectorTotal
> 
>   in proc () -> do 
>   
>   (cDate, cMngrName, cSector, cNetWorth) <- qQuarterlyManagerSectorTotal -< ()
>   (cDate', cSector', cNetWorth')         <- qQuarterlySectorTotal     -< ()
>   
>   restrict -< cDate   .== cDate'
>   restrict -< cSector .== cSector'
>     
>   returnA -< History cDate (Manager cMngrName (SecuritySector cSector, cNetWorth / cNetWorth'))
> 
>   where qAlter1 qry = proc () -> do (History cDate (Manager cMngrName (Security _ cSecQuant cSecValue)), (_, SecuritySector cNullSector)) <- qry -< ()  
>                                     let cSector = fromNullable (pgString "unknown_sector")   cNullSector
>                                     returnA -< (cDate  , cMngrName, cSector,             (cSecQuant             ,cSecValue))
>         aHistMngrSectorVal =                p4 (groupBy, groupBy  , groupBy, aDotProduct (securityQuantity.fst) (securityValue.snd)) 
> 
>         qAlter2 qry = proc () -> do (cDate, _, cSector, netWorth) <- qry -< ()  
>                                     returnA -< (cDate  , cSector, netWorth)
>         aHistSectorVal =                    p3 (groupBy, groupBy, sum) 
> 

Aggregators

> aDotProduct :: (a -> Column PGFloat8)    -- TODO: Inlcude class constraint: Opaleye.Internal.Column.PGNum a1 
>             -> (a -> Column PGFloat8) 
>             -> Aggregator a (Column PGFloat8)
> aDotProduct f1 f2 = lmap (\x ->  f1 x * f2 x) sum
