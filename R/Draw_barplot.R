#' Draw bar plot using table
#'
#' This function draw bar chart with table
#' @keywords Genomic
#' @param table Table for visualizing.
#' @export
#' @example Draw_barplot(table_name)

Draw_barplot <- function(input_table,title_name = NULL, input_select = NULL){
    for (i in 1:dim(input_table)[1]){
        input_table$Proportion[i] <- input_table$Freq[i]/sum(input_table$Freq)
    }
    input_table <- input_table[order(-input_table$Proportion),]
    if(is.element("Total",input_select) || is.null(input_select)){
        bpTable <- input_table
    }else{
        bpTable <- input_table[input_table$Var1%in%input_select,]
    }
    bp <- barplot(bpTable$Proportion, xaxt="n", main = title_name,
                  xlab="Sequence Alteration", ylab = 'Fraction of Mutation Type', ylim=c(0:1), angle = 45)
    text( x=bp, y=-0.1, bpTable$Var1, xpd=TRUE, srt=45)

    return(bp)
}
