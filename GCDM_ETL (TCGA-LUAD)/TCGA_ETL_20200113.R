library(SqlRender)
library(DatabaseConnector)
library(xlsx)
library(readxl)
library(tibble)
library(tidyr)
library(stringr)

getwd()
setwd("D:/Users/ssj/Desktop/GCDM_ETL (TCGA-Lung)")
source('scripts/scripts_20200113.R', encoding='UTF-8')

## Setup
## DB options are the same for DatabaseConnector

dbms = "sql server"
server = "server_ip"
schema = "GCDM_TCGA.dbo"
user="user_id"
password="user_pw"

ddlFile = "sql/CDMG-DDL-SQLSERVER (TCGA_20200113).sql"
variantDir = "data/variant" # The Dir _variant.xlsx files exists
variantFile = "data/TCGA_TargetGene_variant.xlsx"
clinicalFile = "data/TCGA_Clinical_Data.xlsx" 
HGNC_file = "data/HGNC.xlsx"
conceptID = "data/Vocabulary_20200113.xlsx"

conn = connectGCDM(dbms, server, schema, user, password)

dropTableGCDM(conn, schema)

createTableGCDM(conn, schema, ddlFile) 

insertXLSXIntoGCDM(conn, schema, variantDir, clinicalFile)

