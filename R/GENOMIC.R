#' Start GENOMIC
#'
#' build and run genomic
#' @keywords genomic
#' @export
#' @example genomic()
genomic <- function(){
    check.packages("shiny")
    app_path <- paste0(.libPaths()[1],"/genomic")
    shiny::runApp(app_path)
}
