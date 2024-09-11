library(DBI)
library(odbc)
library(httr)

##### Connecting to snowflake
source("login_credentials.R")

user <- username
password <-  password
client_id <- client_id
client_secret <- client_secret
warehouse <- warehouse
role <- role
database <- database
snowflake_account <- snowflake_account
snowflake_host <- snowflake_host
oauth_url <- oauth_url


payload <- list(
  'client_id' = client_id,
  'client_secret' = client_secret,
  'username' = user,
  'password' = password,
  'grant_type' = 'password',
  'scope' = 'SESSION:ROLE-ANY')

response <- POST(oauth_url, body = payload, encode="form")

print((content(response, encoding="json")))

db_connection <- DBI::dbConnect(odbc::odbc(),
                                Driver="SnowflakeDSIIDriver",
                                Server=snowflake_host,
                                Database=database,
                                UID=user,
                                ROLE=role,
                                authenticator="OAUTH",
                                token = toString(content(response, encoding="json")["access_token"]),
                                WAREHOUSE=warehouse)  

query_result <- dbGetQuery(db_connection, "select current_version(), current_user()")
print(query_result)

################################################################################################################################
### Connecting using ODBC

install.packages(c("DBI", "dplyr","dbplyr","odbc"))
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
myconn <- DBI::dbConnect(odbc::odbc(), "Test1", Driver=Driver, uid=username, pwd=password)

mydata <- DBI::dbGetQuery(myconn,"Select*
from PGSUDH_DL_PROD.PGSUDH_CDM.LIMS_LABWARE_RESULT
limit 10")


