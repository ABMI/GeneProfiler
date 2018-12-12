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
