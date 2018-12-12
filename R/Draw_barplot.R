#' Draw bar plot using table
#'
#' This function draw bar chart with table
#' @keywords Genomic
#' @param table Table for visualizing.
#' @export
#' @example Draw_barplot(table_name)

Draw_barplot <- function(table){
    for (i in 1:dim(table)[1]){
        table$Proportion[i] <- table$Freq[i]/sum(table$Freq)
    }
    table <- table[order(-table$Proportion),]
    bp <- barplot(table$Proportion, xaxt="n",
            xlab="Sequence Alteration", ylab = 'Fraction of Mutation Type', ylim=c(0:1), angle = 45)
    text( x=bp, y=-0.1, table$Var1, xpd=TRUE, srt=45)
    # bp <- ggplot(table, aes(x=table$Var1, y=table$Proportion)) +
    #     geom_bar(stat="identity") +
    #     theme(axis.text.x = element_text(angle=90))

    return(bp)
}
