#' Draw pie plot using table
#'
#' This function help to draw pie chart by assigning slices and labels.
#' @keywords Genomic
#' @param table Table for visualizing.
#' @export
#' @example Draw_pieplot(table_name)

Draw_pieplot<-function(table){
    slices <<- table$Freq
    lbls <- as.character(table$Var1)
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels
    lbls <<- paste(lbls,"%",sep="") # add % to labels
    return(0)
}
