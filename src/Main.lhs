> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TemplateHaskell #-}
> 
> import MngrTable
> import Queries
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
> import Database.PostgreSQL.Simple.Internal

Key words: BUG, TODO, NOTE

> main = print "hi"
> 

--------------------------------------------------
Testing

 create table tMngrSecHist (quarter date, mngr_name text, sec_name text, sec_quant float8, sec_value float8);
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','Paul','Amazon', 50, 10.5);
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','Paul','Amazon', 10, 5.5);
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','George','NEM', 200, 150.12);
 INSERT INTO tMngrSecHist VALUES ('2014-01-01','George','Alibaba', 800, 90.12);

 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','Amazon', 250, 100.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','Amazon', 1000, 150.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','Google', 100, 50.12);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','John','Google', 1000, 150.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','John','NEM', 200, 50.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','John','RGLD', 100, 50.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','RGLD', 100, 50.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','John','***', 100, 50.5);
 INSERT INTO tMngrSecHist VALUES ('2015-01-01','Paul','***', 400, 50.5);

 create table tSecInfo ( sec_name text, sec_sector text);
 INSERT INTO tSecInfo VALUES ('Amazon', 'tech');
 INSERT INTO tSecInfo VALUES ('Google', 'tech');
 INSERT INTO tSecInfo VALUES ('Alibaba','tech');
 INSERT INTO tSecInfo VALUES ('NEM', 'mining');
 INSERT INTO tSecInfo VALUES ('RGLD', 'mining');

 P1 Results
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = 0.546718288204319}}
 History {histDate = 2014-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = 5.6475170399221e-3}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "John", mngrSecurCol = 0.453281711795681}}
 History {histDate = 2014-01-01, histValue = Manager {mngrName = "George", mngrSecurCol = 0.994352482960078}}

 P2 Results
 History {histDate = 2014-01-01, histValue = Manager {mngrName = "George", mngrSecurCol = (SecuritySector "mining",1.0)}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "mining",0.25)}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "John", mngrSecurCol = (SecuritySector "mining",0.75)}}
 History {histDate = 2014-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "tech",7.98062634157081e-3)}}
 History {histDate = 2014-01-01, histValue = Manager {mngrName = "George", mngrSecurCol = (SecuritySector "tech",0.992019373658429)}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "tech",0.545505334650009)}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "John", mngrSecurCol = (SecuritySector "tech",0.454494665349991)}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "John", mngrSecurCol = (SecuritySector "unknown_sector",0.2)}}
 History {histDate = 2015-01-01, histValue = Manager {mngrName = "Paul", mngrSecurCol = (SecuritySector "unknown_sector",0.8)}}



> testProblem1 = do conn <- connect ConnectInfo { connectHost="127.0.0.1",connectPort=5432,connectUser="postgres",connectPassword="password",connectDatabase = "managers" }
>                   a <- runQuery conn (qMngrPcntNetWorthHist qMngrSecHist) :: IO [QrtrlyMngrPcntNetWorth]
>                   mapM_ print a

> testProblem2 = do conn <- connect ConnectInfo { connectHost="127.0.0.1",connectPort=5432,connectUser="postgres",connectPassword="password",connectDatabase = "managers" }
>                   a <- runQuery conn (qHistMngrSectorPcnt qMngrSecHist qSecInfo) :: IO [QrtrlyMngrSectorPct]
>                   mapM_ print a


--------------------------------------------------
Util functions

> printSql :: Default Unpackspec a a => Query a -> IO ()
> printSql = putStrLn . showSqlForPostgres


--------------------------------------------------
Cabal Repl
  
 a <- runQuery conn secQuery :: IO [Security]
 mapM_ print a
 abc <- runQuery conn $  aggregate secColNetWorth secQuery :: IO [Double]

