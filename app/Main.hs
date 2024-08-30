{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Main where

--------------------------------------
-- Imports 

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Marshall as Marshall
-- import qualified Orville.PostgreSQL.AutoMigration as AM
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Raw.Connection as Connection
import qualified Data.ByteString.Char8 as BB
import qualified Data.Text as Text
import Prelude

---------------------------------------
-- Function to execute raw sql query

executeRawSql :: 
  Connection.Connection -> 
  String -> 
  [SqlValue.SqlValue] ->
  Marshall.AnnotatedSqlMarshaller writeEntity readEntity ->
  IO [readEntity]
executeRawSql conn sql params marshaller = do
  let query = BB.pack sql
  let errorDetailLevel = O.defaultErrorDetailLevel
  let params' = map SqlValue.toPgValue params
  libPqResult <- Connection.executeRaw conn query params'
  resulti <- Marshall.marshallResultFromSql errorDetailLevel marshaller libPqResult
  case resulti of
    Left _ -> pure []
    Right result -> pure result

-----------------------------------------
-- Sample table object and implementation

data MessageObj = MessageObj { jobId :: Text.Text } deriving (Show)

data SchemaItem where
  -- |
  --    Constructs a 'SchemaItem' from a 'Orville.TableDefinition'.
  --
  -- @since 1.0.0.0
  SchemaTable :: forall key writeEntity readEntity . O.TableDefinition key writeEntity readEntity -> SchemaItem
  -- |
  --    Constructs a 'SchemaItem' that will drop the specified table if it is
  --    found in the database.
  --
  -- @since 1.0.0.0
  SchemaDropTable ::
    O.TableIdentifier ->
    SchemaItem


messageTable :: O.TableDefinition (O.HasKey Text.Text) MessageObj MessageObj 
messageTable = O.mkTableDefinition "message_table" (O.primaryKey jobIdField) messageTableMarshaller

messageTableMarshaller :: O.SqlMarshaller MessageObj MessageObj
messageTableMarshaller = 
  MessageObj
    <$> O.marshallField jobId jobIdField

jobIdField :: O.FieldDefinition O.NotNull Text.Text
jobIdField =
  O.fieldOfType O.unboundedText "job_id"

---------------------------------------
-- Sample query to test sql func.

sampleQuery :: Connection.Connection -> IO ()
sampleQuery conn = do
  result <- executeRawSql 
	      conn
	      ("SELECT * FROM message_table WHERE job_id = $1")
 	      [ Marshall.fieldValueToSqlValue jobIdField "123456" ]
  	      (O.tableMarshaller messageTable)
  print result

newtype DealOfferId = DealOfferId {unDealOfferId :: Text.Text}
--
schemaItems = [SchemaTable messageTable]

allTableDefinitions :: 
  forall key writeEntity readEntity . 
  [SchemaItem] -> 
  [O.TableDefinition key writeEntity readEntity]
allTableDefinitions [] = []
allTableDefinitions (x:xs) =
  case x of
    SchemaTable tableDef -> tableDef : (allTableDefinitions xs)
    _ -> allTableDefinitions xs
--------------------------------------
-- main func.
--
-- data Employee = Employee { empId :: Int, empName :: String } deriving (Show)
-- data Functions = Functions { funcId :: Int, funcName :: String } deriving (Show)
-- data Department = Department { deptId :: Int, deptName :: String } deriving (Show)
--
--
-- innerJoin :: [Employee] -> [Department] -> [Functions] -> [(Employee, Functions, Department)]
-- innerJoin employees departments functions =
--     [ (emp, func, dept) | emp <- employees, dept <- departments, func <- functions, (empId emp == deptId dept) && (empId emp == funcId func) ]
--
--
-- employees :: [Employee]
-- employees = [Employee 1 "Alice", Employee 1 "Sparsh", Employee 2 "Bob", Employee 3 "Charlie"]
--
-- functions :: [Functions]
-- functions = [Functions 4 "func40", Functions 3 "func30", Functions 2 "func20", Functions 2 "func21", Functions 3 "func31", Functions 1 "func10"]
--
-- departments :: [Department]
-- departments = [Department 1 "HR", Department 3 "IT", Department 2 "Finance"]
--

main :: IO ()
main = do 
  -- let x = innerJoin employees departments functions
  -- putStrLn $ show x
  -- pure ()
   
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "dbname=orville_test user=sparsh password=password"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }
  Connection.withPoolConnection pool $ \connection -> do
    sampleQuery connection














