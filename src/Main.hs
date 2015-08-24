{-# LANGUAGE FlexibleContexts #-}

import MngrTable
import Queries

import Prelude hiding (sum)
import Opaleye (Query, runQuery, showSqlForPostgres, Unpackspec)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple.Internal
import Data.Time.Calendar
import Control.Applicative

-- Key words: BUG, TODO, NOTE

--------------------------------------------------
-- main 
-- settings - Change dbConnection to suite needs

main :: IO ()
main = do testProblems

dbConnection :: IO Connection
dbConnection = connect ConnectInfo { connectHost     = "127.0.0.1"
                                   , connectPort     = 5432
                                   , connectUser     = "postgres"
                                   , connectPassword = "password"
                                   , connectDatabase = "managers" 
                                   }

--------------------------------------------------
-- Testing

testProblems :: IO ()
testProblems = do 
   conn <- dbConnection
   let printResult s bool = print $ s ++ if bool 
                                         then "Pass" 
                                         else "Fail"
 
   result1 <- runQuery conn $ qQuarterlyManagerShare qQuarterlyManagerSecurity :: IO [QuarterlyManagerShare]
   printResult "Test 1: " $
               areListEq result1 ([ History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "Paul", mngrSecurCol = 0.546718288204319}}
                                  , History {histDate = readDay "2014-01-01", histValue = Manager {mngrName = "Paul", mngrSecurCol = 5.6475170399221e-3}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "John", mngrSecurCol = 0.453281711795681}}
                                  , History {histDate = readDay "2014-01-01", histValue = Manager {mngrName = "George", mngrSecurCol = 0.994352482960078}}
                                  ] :: [QuarterlyManagerShare])
    
   result2 <- runQuery conn $ qQuarterlyManagerSectorShare qQuarterlyManagerSecurity qSecurityInformation :: IO [QuarterlyManagerSectorShare]
   printResult "Test 2: " $ 
               areListEq result2 ([ History {histDate = readDay "2014-01-01", histValue = Manager {mngrName = "George", mngrSecurCol = (SecuritySector "mining",1.0)}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "mining",0.25)}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "John", mngrSecurCol = (SecuritySector "mining",0.75)}}
                                  , History {histDate = readDay "2014-01-01", histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "tech",7.98062634157081e-3)}}
                                  , History {histDate = readDay "2014-01-01", histValue = Manager {mngrName = "George", mngrSecurCol = (SecuritySector "tech",0.992019373658429)}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "tech",0.545505334650009)}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "John", mngrSecurCol = (SecuritySector "tech",0.454494665349991)}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "John", mngrSecurCol = (SecuritySector "unknown_sector",0.2)}}
                                  , History {histDate = readDay "2015-01-01", histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "unknown_sector",0.8)}}
                                  ] :: [QuarterlyManagerSectorShare])

--------------------------------------------------
-- Table data to test against

{-
 create table tQuarterlyManagerSecurity (quarter date, mngr_name text, sec_name text, sec_quant float8, sec_value float8);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2014-01-01','Paul','Amazon', 50, 10.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2014-01-01','Paul','Amazon', 10, 5.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2014-01-01','George','NEM', 200, 150.12);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2014-01-01','George','Alibaba', 800, 90.12);

 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','Paul','Amazon', 250, 100.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','Paul','Amazon', 1000, 150.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','Paul','Google', 100, 50.12);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','John','Google', 1000, 150.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','John','NEM', 200, 50.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','John','RGLD', 100, 50.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','Paul','RGLD', 100, 50.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','John','***', 100, 50.5);
 INSERT INTO tQuarterlyManagerSecurity VALUES ('2015-01-01','Paul','***', 400, 50.5);

 create table tSecurityInformation ( sec_name text, sec_sector text);
 INSERT INTO tSecurityInformation VALUES ('Amazon', 'tech');
 INSERT INTO tSecurityInformation VALUES ('Google', 'tech');
 INSERT INTO tSecurityInformation VALUES ('Alibaba','tech');
 INSERT INTO tSecurityInformation VALUES ('NEM', 'mining');
 INSERT INTO tSecurityInformation VALUES ('RGLD', 'mining');
-}

--------------------------------------------------
-- Util functions

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

readDay :: String -> Day
readDay x = read x

areListEq :: Eq a => [a] -> [a] -> Bool
areListEq ls1 ls2 =    (length ls1 == length ls2)   
                    && (and $ (elem <$> ls1) <*> (pure ls2))  -- Check lists both ways to rule out repeated elements
                    && (and $ (elem <$> ls2) <*> (pure ls1))  -- ""

