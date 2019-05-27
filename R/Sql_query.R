#' SQL query setting
#'
#' This function get sql and extract data.
#' @param sql sql query
#' @param input input data when connecting DB
#' @keywords genomic
#' @export
#' @example sql_query(sql)

sql_query <- function(sql,input){
    if(input$Cohort_definition_id == '')
    {
        sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table, Cohort_definition_id='\'%\'')$sql
    }
    else{
        sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table, Cohort_definition_id=input$Cohort_definition_id)$sql
    }
    sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
    return(DatabaseConnector::querySql(connection, sql))
}
