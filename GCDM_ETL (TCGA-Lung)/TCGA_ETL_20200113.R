library(SqlRender)
library(DatabaseConnector)
library(xlsx)
library(readxl)
library(tibble)
library(tidyr)
library(stringr)

getwd()
setwd("D:/Users/ssj/Desktop/TCGA_ETL_20200113")
source('scripts/scripts_20200113.R', encoding='UTF-8')

## Setup
## DB options are the same for DatabaseConnector

dbms = "sql server"
server = "server"
schema = "GCDM_TCGA.dbo"
user="id"
password="pw"

ddlFile = "sql/CDMG-DDL-SQLSERVER (TCGA_20200113).sql"
variantDir = "data/variant" # The Dir _variant.xlsx files exists
variantFile = "data/TCGA_TargetGene_variant.xlsx"
clinicalFile = "data/TCGA_Clinical_Data.xlsx" 
#TotalvariantFile = "data/TCGA_TargetGene_variant.xlsx"
HGNC_file = "data/HGNC.xlsx"
conceptID = "data/Vocabulary_20200113.xlsx"

conn = connectGCDM(dbms, server, schema, user, password)

dropTableGCDM(conn, schema)

createTableGCDM(conn, schema, ddlFile) 

insertXLSXIntoGCDM(conn, schema, variantDir, clinicalFile)

