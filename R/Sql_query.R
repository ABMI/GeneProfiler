#' SQL query setting
#'
#' This function get sql and extract data.
#' @param sql sql query
#' @param input input data when connecting DB
#' @keywords genomic
#' @export
#' @example sql_query(sql)

sql_query <- function(sql,input){
    sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
    sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
    return(DatabaseConnector::querySql(connection, sql))
}
# sql <- SqlRender::renderSql(sql, schema="SSJ_GCDM_AJOU_v3", Cohort_table="cohort")$sql
# sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
# cohort_forPathogeny <- DatabaseConnector::querySql(connection, sql)
