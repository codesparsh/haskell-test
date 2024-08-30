{-# LANGUAGE OverloadedStrings #-}

module Database.Utils where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Raw.Connection as Connection
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BB
import Prelude

data MessageObj = MessageObj { jobId :: Text.Text } deriving (Show)

messageTable :: O.TableDefinition (O.HasKey Text.Text) MessageObj MessageObj 
messageTable = O.mkTableDefinition "message_table" (O.primaryKey jobIdField) messageTableMarshaller

messageTableMarshaller :: O.SqlMarshaller MessageObj MessageObj
messageTableMarshaller = 
  MessageObj
    <$> O.marshallField jobId jobIdField

jobIdField :: O.FieldDefinition O.NotNull Text.Text
jobIdField =
  O.fieldOfType O.unboundedText "job_id"

executeRawSql :: 
  Connection.Connection -> 
  String -> 
  [SqlValue.SqlValue] -> 
  O.TableDefinition key writeEntity readEntity -> 
  IO [readEntity]
executeRawSql conn sql params tableDefinition = do
  let query = BB.pack sql
  let errorDetailLevel = O.defaultErrorDetailLevel
  let params' = map SqlValue.toPgValue params
  let marshall = O.tableMarshaller tableDefinition 
  libPqResult <- Connection.executeRaw conn query params'
  resulti <- Marshall.marshallResultFromSql errorDetailLevel marshall libPqResult
  case resulti of
    Left _ -> pure []
    Right result -> pure result

sampleQuery :: Connection.Connection -> IO ()
sampleQuery conn = do
  result <- executeRawSql 
	      conn
	      ("SELECT * FROM message_table WHERE job_id = $1")
 	      [ Marshall.fieldValueToSqlValue jobIdField "123456" ]
	      messageTable
  print result
