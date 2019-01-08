#' Check package installed
#'
#' This function check package successfully installed. If not, package install.
#' @keywords genomic
#' @param pkg package name
#' @export
#' @example check.packages("dplyr")

check.packages <- function(pkg){
    if (!(pkg %in% installed.packages()[,"Package"])){
        install.packages(pkg)
        tryCatch(
            library(pkg, character.only = T)
        ,error=function(e){
            if(pkg == "GenVisR")
                install_github("https://github.com/griffithlab/GenVisR.git")
        })
    }
    else{
        library(pkg, character.only = T)
    }
}
