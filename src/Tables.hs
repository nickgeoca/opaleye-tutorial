{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
 
module Tables where

import Types

import Prelude hiding (sum)

import Opaleye (Column, Table(Table), required, queryTable,
                Query, PGText, PGDate)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product (p2)
import Data.Time.Calendar

-- Key words: BUG, TODO, NOTE

--------------------------------------------------
-- Types

type    ManagerSecurity = Manager'         String     Security
type ColManagerSecurity = Manager' (Column PGText) ColSecurity
 
type    QuarterlyManagerSecurity = History'         Day        ManagerSecurity
type ColQuarterlyManagerSecurity = History' (Column PGDate) ColManagerSecurity

type    SecurityInformation = (   SecurityName,    SecuritySector)
type ColSecurityInformation = (ColSecurityName, ColSecuritySector)

--------------------------------------------------
-- Sql

-- Tables

$(makeAdaptorAndInstance "pHistory" ''History')   

tQuarterlyManagerSecurity :: Table ColQuarterlyManagerSecurity
                                   ColQuarterlyManagerSecurity
tQuarterlyManagerSecurity = Table "tQuarterlyManagerSecurity" 
     (pHistory $ History 
       { histDate  = required "quarter"  
       , histValue = manager' "mngr_name" (security' "sec_name" "sec_quant" "sec_value")  -- TODO: Put column names in type section?
       }
   )

tSecurityInformation :: Table ColSecurityInformation
                              ColSecurityInformation
tSecurityInformation = Table "tSecurityInformation" $ p2 ( pSecurityName   $ SecurityName   $ required "sec_name"
                                                         , pSecuritySector $ SecuritySector $ required "sec_sector")

--------------------------------------------------
-- Queries
 
tqQuarterlyManagerSecurity :: Query ColQuarterlyManagerSecurity
tqQuarterlyManagerSecurity = queryTable tQuarterlyManagerSecurity

tqSecurityInformation :: Query ColSecurityInformation
tqSecurityInformation = queryTable tSecurityInformation
