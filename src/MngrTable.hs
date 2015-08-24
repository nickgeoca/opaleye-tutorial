{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
 
module MngrTable where

import Prelude hiding (sum)

import Opaleye (Column, Table(Table), required, queryTable,
                Query, PGText, PGDate, PGFloat8)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor (dimap, lmap, rmap)
import Data.Profunctor.Product (p2)
import Data.Time.Calendar

-- Key words: BUG, TODO, NOTE

--------------------------------------------------
-- Types

-- Security

data SecurityName' a = SecurityName a deriving (Show, Eq)
securityName :: SecurityName' a -> a
securityName (SecurityName x) = x
$(makeAdaptorAndInstance "pSecurityName" ''SecurityName')
securityName' s = pSecurityName $ SecurityName $ required s
type    SecurityName = SecurityName'         String
type ColSecurityName = SecurityName' (Column PGText)

data SecuritySector' a = SecuritySector a deriving (Show, Eq)
securitySector :: SecuritySector' a -> a
securitySector (SecuritySector x) = x
$(makeAdaptorAndInstance "pSecuritySector" ''SecuritySector')
securitySector' s = pSecuritySector $ SecuritySector $ required s
type    SecuritySector = SecuritySector'         String
type ColSecuritySector = SecuritySector' (Column PGText)

data SecurityQuantity' a = SecurityQuantity a deriving (Show, Eq)
securityQuantity :: SecurityQuantity' a -> a
securityQuantity (SecurityQuantity x) = x
$(makeAdaptorAndInstance "pSecurityQuantity" ''SecurityQuantity')
securityQuantity' s = pSecurityQuantity $ SecurityQuantity $ required s
type    SecurityQuantity = SecurityQuantity'         Double
type ColSecurityQuantity = SecurityQuantity' (Column PGFloat8)

data SecurityValue' a = SecurityValue a deriving (Show, Eq)
securityValue :: SecurityValue' a -> a
securityValue (SecurityValue x) = x
$(makeAdaptorAndInstance "pSecurityValue" ''SecurityValue')
securityValue' s = pSecurityValue $ SecurityValue $ required s
type    SecurityValue = SecurityValue'         Double
type ColSecurityValue = SecurityValue' (Column PGFloat8)


data Security' a b c = Security
  { secName   :: a
  , secQuant  :: b
  , secValue  :: c 
  }  deriving (Show, Eq)
$(makeAdaptorAndInstance "pSecurity" ''Security')
security' n q v = pSecurity $ Security { secName   = securityName' n
                                       , secQuant  = securityQuantity' q
                                       , secValue  = securityValue' v }

type    Security = Security'    SecurityName    SecurityQuantity    SecurityValue
type ColSecurity = Security' ColSecurityName ColSecurityQuantity ColSecurityValue

-- Manager

data Manager' a b = Manager 
  { mngrName     :: a
  , mngrSecurCol :: b
  } deriving (Show, Eq) -- TODO: Rename this to mngrValue? mngrSecurCol is being applied elsewhere without that context
$(makeAdaptorAndInstance "pManager" ''Manager')
manager' name sec = pManager $ Manager { mngrName = required name, mngrSecurCol = sec }
 
type    ManagerSecurity = Manager'         String     Security
type ColManagerSecurity = Manager' (Column PGText) ColSecurity

-- History

data History' a b = History
  { histDate  :: a
  , histValue :: b } deriving (Show, Eq)
 
type    QuarterlyManagerSecurity = History'         Day        ManagerSecurity
type ColQuarterlyManagerSecurity = History' (Column PGDate) ColManagerSecurity

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

-- Queries

qQuarterlyManagerSecurity :: Query ColQuarterlyManagerSecurity
qQuarterlyManagerSecurity = queryTable tQuarterlyManagerSecurity

--------------------------------------------------

---- Types

type    SecurityInformation = (   SecurityName,    SecuritySector)
type ColSecurityInformation = (ColSecurityName, ColSecuritySector)

---- Sql

-- Tables

tSecurityInformation :: Table ColSecurityInformation
                              ColSecurityInformation
tSecurityInformation = Table "tSecurityInformation" $ p2 ( pSecurityName   $ SecurityName   $ required "sec_name"
                                                         , pSecuritySector $ SecuritySector $ required "sec_sector")

-- Queries

qSecurityInformation :: Query ColSecurityInformation
qSecurityInformation = queryTable tSecurityInformation

