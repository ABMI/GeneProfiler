#' Connect database server
#'
#' This function connect to DB server
#' @keywords Genomic
#' @param server The type of DBMS running on the server. Valid values are
#' "oracle" for Oracle
#' "postgresql" for PostgreSQL
#' "sql server" for Microsoft SQL Server
#' @param ip The name of the server.
#' @param id The user name used to access the server.
#' @param pw The password for that user.
#' @param schema The name of the schema to connect to.
#' @export
#' @example Connect_DB(server_name, ip, db_id, pwd, schema_name))

Connect_DB <- function(server,ip,id,pw,schema){
    #Reconnect other DB
    try(DatabaseConnector::disconnect(connection),silent = T)
    connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms=server,
                                                                     server=ip,
                                                                     user=id,
                                                                     password=pw,
                                                                     schema=schema)

    #fix
    tryCatch({
        connection <<- DatabaseConnector::connect(connectionDetails)
        showModal(modalDialog(title="DB connection status","Your DB is connected!",footer = modalButton("OK")))
    },error = function(e){
        showModal(modalDialog(title="DB connection status","Your DB is failed to connect! ",footer = modalButton("OK")))
    })
}

