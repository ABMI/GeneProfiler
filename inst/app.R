
check.packages("xlsx")

check.packages("shinydashboard")
check.packages("SqlRender")
check.packages("DatabaseConnector")
check.packages("dplyr")
check.packages("GenVisR")
check.packages("ggrepel")


# require(shiny)
# library(xlsx)
# library(shinydashboard)
# library(SqlRender)
# library(DatabaseConnector)
# library(dplyr)
# library(GenVisR)
# library(ggrepel)

# library(devtools)
# install_github("https://github.com/griffithlab/GenVisR.git")

############################################

shiny::shinyApp(

  # UI Definition

  ui <- dashboardPage(

    #1
    dashboardHeader(title='GeneProfiler'),

    #2
    dashboardSidebar(sidebarMenu(menuItem('DB connection', tabName='db'),
                                 menuItem('Overall Profile', tabName='WaterFall'),
                                 menuItem('Mutation Type', tabName='MutationType'),
                                 menuItem('Pathogeny', tabName='PathogenyPlot'),
                                 menuItem('Gene', tabName='GenePlot'),
                                 menuItem('Variant', tabName='VariantTable'),
                                 menuItem('Search Yours', tabName='Search')
                                 )),

    #3
    dashboardBody(

      tabItems(

        #3-1 DB Connection
        tabItem(tabName='db',
                fluidRow(
                  titlePanel('Database Connection'),

                  sidebarPanel(
                    #input text to db information
                    textInput("ip","IP", "128.1.99.58")
                    ,textInput("user","USER", 'ssj')
                    ,passwordInput("pw","PASSWORD", 'ssj1225')
                    ,textInput("schema","GCDM Database", 'SSJ_GCDM_AJOU_v3')
                    ,textInput("Cohort_table","Cohort Table", 'cohort')
                    ,actionButton("db_load","Load DB")
                    ,width=10),

                  mainPanel(
                    textOutput(outputId = 'DB_Connect')
                    ))),


        # 3-2 WaterFall
        tabItem(tabName = "WaterFall",
                fluidRow(
                  titlePanel("Overall Mutation Profile"),

                  sidebarPanel(
                    actionButton(inputId = 'Show_WF', label = 'Go!'), width=2,
                    #height means percentage of box
                    tags$head(tags$style("#WF_Plot{height:70vh !important;}"))
                  ),

                  mainPanel(
                    plotOutput(outputId = "WF_Plot"), width=12)
                  )),


        # 3-3 Mutation Type
        tabItem(tabName = "MutationType",
                fluidRow(
                  titlePanel("Mutation Type"),

                  sidebarPanel(
                    actionButton(inputId = 'Show_MutationType', label = 'Go!'), width=2),

                  mainPanel(
                    plotOutput(outputId = "MutationType_Plot"), width=12)
                )),


        # 3-4 Pathogeny Plot
        tabItem(tabName = "PathogenyPlot",
                fluidRow(
                  titlePanel("Pathogeny Plot"),

                  sidebarPanel(
                    actionButton(inputId = 'Show_Pathogeny', label = 'Go!'), width=2),

                  mainPanel(
                    plotOutput(outputId = "Pathogeny_Plot"), width=12)
                )),



        # 3-5 Pathogeny & Drug Response Genes
        tabItem(tabName = "GenePlot",
                fluidRow(
                  titlePanel("Gene Plot"),

                  sidebarPanel(
                    actionButton(inputId = 'Show_GenePlot', label = 'Go!'), width=2),

                  mainPanel(
                    plotOutput(outputId = "GenePlot"), width=12)
                )),


        # 3-6 Variant
        tabItem(tabName = "VariantTable",
                fluidRow(
                  titlePanel("Variant Table"),

                  sidebarPanel(
                    actionButton(inputId = 'Show_VariantTable', label = 'Go!'), width=2),

                  mainPanel(
                    tableOutput(outputId = "VariantTable"), width=12)
                )),

        # 3-7 Search Yours
        tabItem(tabName = "Search",
                fluidRow(
                  titlePanel("Explore Your Data!"),

                  sidebarPanel(

                    textInput('CstmGene', 'Gene Symbol', value = 'EGFR'),

                    textInput('CstmHGVSp', 'HVGSp', value = 'p.Leu858Arg'),

                    # selectInput('CstmGene', 'Gene Symbol', multiple = TRUE,
                    #             choices = c("ATM", "ATR", "BRAF", "BRCA1", "BRCA2", "EGFR", "KRAS",
                    #                         "MET", "MTOR", "NRAS", "PIK3CA", "PTEN", "TP53")),
                    #
                    # selectInput('CstmSeqAlt', 'Sequence Alteration', multiple = TRUE,
                    #             choices = c("Amplification", "DEL", "Deletion", "INS", "MIXED", "MNP", "SNP", "TRA")),
                    #
                    # selectInput('CstmVarFtr', 'Variant Feature', multiple = TRUE,
                    #             choices = c("Frameshift", "Inframe", "Intron", "Missense",
                    #                         "Nonsense", "Other", "Splice", "Synonymous", "UTR")),
                    #
                    # selectInput('CstmOrigin', 'Variant Origin', multiple = TRUE,
                    #             choices = c("germline", "somatic", "unknown")),
                    #
                    # selectInput('CstmPG', 'Variant Pathogeny', multiple = TRUE,
                    #             choices = c("Benign", "Benign/Likely benign", "Conflict pathogenicity",
                    #                         "Drug response", "Likely benign", "Pathogenic/Likely pathogenic",
                    #                         "Unknown significance")),

                    actionButton(inputId = 'Show_Custom', label = 'Go!'), width=5),

                  mainPanel(
                    plotOutput(outputId = "Custom_Result_Plot"),
                    tableOutput(outputId = "Custom_Result_Table"), width=12)
                ))

      ))), # End of dashboardPage



############################################################



  server <- function(input, output, session) {


    ##### 3-1 DB Connection
    Connect.DB <- eventReactive(input$db_load, {

        #Reconnect other DB
        Connect_DB("sql server", input$ip, input$user, input$pw, input$schema)

    #     try(disconnect(connection),silent = T)
    #   connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
    #                                                                   server=input$ip,
    #                                                                   user=input$user,
    #                                                                   password=input$pw,
    #                                                                   schema=input$schema)
    #
    #   #fix
    #   tryCatch({
    #       connection <<- DatabaseConnector::connect(connectionDetails)
    #       showModal(modalDialog(title="DB connection status","Your DB is connected!",footer = modalButton("OK")))
    #   },error = function(e){
    #       showModal(modalDialog(title="DB connection status","Your DB is failed to connect! ",footer = modalButton("OK")))
    # })
      # print('Your DB is connected!')})
    })
    output$DB_Connect <- renderText({Connect.DB()})


    #cohort_personID <- cohort_personID()




    ##### 3-2 WaterFall Plot
    draw.WF <- eventReactive(input$Show_WF, {

      # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
      #                                                                  server=input$ip,
      #                                                                  user=input$user,
      #                                                                  password=input$pw,
      #                                                                  schema=input$schema)
      #
      # connection <<- DatabaseConnector::connect(connectionDetails)

      sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
              WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
              LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
      # cohort_variant <- sql_query(sql)
      # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
      # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
      # cohort_variant <- DatabaseConnector::querySql(connection, sql)
      cohort_variant <- sql_query(sql,input)
      # View(cohort_variant)

      cohort_variant$VARIANT_FEATURE[cohort_variant$SEQUENCE_ALTERATION == 'Amplification'] <- 'Amplification'

      cohort_variant$VARIANT_FEATURE[cohort_variant$SEQUENCE_ALTERATION == 'Deletion'] <- 'Deletion'

      cohort_variant <- cohort_variant[complete.cases(cohort_variant[ , c("VARIANT_FEATURE")]), ]

      cohort_variant <- cohort_variant[cohort_variant$VARIANT_FEATURE!='Intron', ]

      cohort_variant <- cohort_variant[cohort_variant$VARIANT_FEATURE!='Synonymous', ]

      cohort_variant <- cohort_variant[cohort_variant$VARIANT_FEATURE!='Other', ]

      cohort_variant <- cohort_variant[complete.cases(cohort_variant[ , c("TARGET_GENE_SOURCE_VALUE")]), ]


      # Create a data frame of random elements to plot
      inputData <- data.frame("sample" = cohort_variant$PERSON_ID,
                              "gene" = cohort_variant$TARGET_GENE_SOURCE_VALUE,
                              "variant_class" = cohort_variant$VARIANT_FEATURE)
      colnames(inputData) <- c('sample', 'gene', 'variant_class')

      table <- as.data.frame(table(inputData$variant_class))
      table <- table[order(-table$Freq), ]

      # choose the most deleterious to plot with y being defined as the most deleterious
      most_deleterious <- as.character(table$Var1)
      GenVisR::waterfall(inputData, fileType="Custom", variant_class_order = most_deleterious, plotMutBurden = FALSE,
                mainXlabel = TRUE, maxGenes=50, mainGrid = TRUE, mainLabelSize = 1,
                plot_proportions = TRUE, section_heights = c(0, 400, 60))

    }) # End of draw.WF

      output$WF_Plot <- renderPlot({draw.WF()})



      ##### 3-3 Mutation Type

      draw.MutationType <- eventReactive(input$Show_MutationType, {

        # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        #                                                                  server=input$ip,
        #                                                                  user=input$user,
        #                                                                  password=input$pw,
        #                                                                  schema=input$schema)
        #
        # connection <<- DatabaseConnector::connect(connectionDetails)

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
        # cohort_forMT <- sql_query(sql)
        # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
        # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
        # cohort_forMT <- DatabaseConnector::querySql(connection, sql)
        cohort_forMT <- sql_query(sql,input)
        par(mfrow=c(1,2))

        Tbl4BarPlot <- as.data.frame(table(cohort_forMT$SEQUENCE_ALTERATION))
        Draw_barplot(Tbl4BarPlot)
        # for (i in 1:dim(Tbl4BarPlot)[1]){
        #   Tbl4BarPlot$Proportion[i] <- Tbl4BarPlot$Freq[i]/sum(Tbl4BarPlot$Freq)
        # }
        # Tbl4BarPlot <- Tbl4BarPlot[order(-Tbl4BarPlot$Proportion),]
        # barplot(Tbl4BarPlot$Proportion, names.arg = Tbl4BarPlot$Var1,
        #         xlab="Sequence Alteration", ylab = 'Fraction of Mutation Type', ylim=c(0:1))

        Tbl4BarPlot <- as.data.frame(table(cohort_forMT$VARIANT_FEATURE))
        Draw_barplot(Tbl4BarPlot)
        # for (i in 1:dim(Tbl4BarPlot)[1]){
        #   Tbl4BarPlot$Proportion[i] <- Tbl4BarPlot$Freq[i]/sum(Tbl4BarPlot$Freq)
        # }
        # Tbl4BarPlot <- Tbl4BarPlot[order(-Tbl4BarPlot$Proportion),]
        # barplot(Tbl4BarPlot$Proportion, names.arg = Tbl4BarPlot$Var1,
        #         xlab="Sequence Alteration", ylab = 'Fraction of Mutation Type', ylim=c(0:1))

      }) # End of draw.MutationType

      output$MutationType_Plot <- renderPlot({draw.MutationType()})





      ##### 3-4 Pathogeny Plot

      draw.PathogenyPlot <- eventReactive(input$Show_Pathogeny, {

        # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        #                                                                  server=input$ip,
        #                                                                  user=input$user,
        #                                                                  password=input$pw,
        #                                                                  schema=input$schema)
        #
        # connection <<- DatabaseConnector::connect(connectionDetails)

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
                       A.hgvs_p, A.sequence_alteration, A.variant_feature,
                       C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                      WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
        # cohort_forPathogeny <- sql_query(sql)
        # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
        # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
        # cohort_forPathogeny <<- DatabaseConnector::querySql(connection, sql)
        cohort_forPathogeny <- sql_query(sql,input)

        par(mfrow=c(1,2))
        par(mar=c(0,0,1,0))
        par(oma=c(0,2,1,2))


        Tbl4PiePlot <- as.data.frame(table(cohort_forPathogeny$VARIANT_ORIGIN))
        Draw_pieplot(Tbl4PiePlot)
        # origin <- ggplot(Tbl4PiePlot, aes(x = "", y = Freq, fill=Var1))+
        #     geom_bar(stat="identity",width=1)+
        #     coord_polar("y",start = 0) +
        #     ggtitle("Proportion of Variant Origin")+
        #     theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
        #     geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5))
        pie(slices, lbls , radius=0.6,
            main="Proportion of Variant Origin", clockwise = TRUE)


        Tbl4PiePlot <- as.data.frame(table(cohort_forPathogeny$VARIANT_PATHOGENY))
        # slices <- Tbl4PiePlot$Freq
        # lbls <- as.character(Tbl4PiePlot$Var1)
        # pct <- round(slices/sum(slices)*100)
        # lbls <- paste(lbls, pct) # add percents to labels
        # lbls <- paste(lbls,"%",sep="") # ad % to labels
        Draw_pieplot(Tbl4PiePlot)
        # pathogeny <- ggplot(Tbl4PiePlot, aes(x = "", y = Freq, fill=Var1))+
        #     geom_bar(stat="identity",width=1)+
        #     coord_polar("y",start = 0) +
        #     ggtitle("Proportion of Variant Pathogeny")+
        #     theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
        #     geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5))
        pie(slices, lbls , radius=0.6,
            main="Proportion of Variant Pathogeny", clockwise = TRUE )


      }) # End of draw.PathogenyPlot

      output$Pathogeny_Plot <- renderPlot({draw.PathogenyPlot()})




      ##### 3-5 Pathogeny & Drug Response Genes

      draw.GenePlot <- eventReactive(input$Show_GenePlot, {

        # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        #                                                                  server=input$ip,
        #                                                                  user=input$user,
        #                                                                  password=input$pw,
        #                                                                  schema=input$schema)
        #
        # connection <<- DatabaseConnector::connect(connectionDetails)

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
        A.hgvs_p, A.sequence_alteration, A.variant_feature,
        C.variant_origin, C.variant_pathogeny
        FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
        LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
        # cohort_forPathogeny <- sql_query(sql)
        # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
        # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
        # cohort_forPathogeny <<- DatabaseConnector::querySql(connection, sql)
        cohort_forPathogeny <- sql_query(sql,input)
        # View(cohort_forPathogeny)

        cohort_forGenes <<- cohort_forPathogeny[cohort_forPathogeny$VARIANT_PATHOGENY %in%
                                                 c('Pathogenic/Likely pathogenic', 'Drug response') &
                                                 cohort_forPathogeny$VARIANT_ORIGIN %in% 'somatic', ]
        Tbl4Gene <- as.data.frame(table(cohort_forGenes$TARGET_GENE_SOURCE_VALUE))
        Tbl4Gene <- Tbl4Gene[order(-Tbl4Gene$Freq),]

        par(mar=c(0,0,1,0))
        par(oma=c(0,0,2,0))

        Draw_pieplot(Tbl4Gene)
        # slices <- Tbl4Gene$Freq
        # lbls <- as.character(Tbl4Gene$Var1)
        # pct <- round(slices/sum(slices)*100)
        # lbls <- paste(lbls, pct) # add percents to labels
        # lbls <- paste(lbls,"%",sep="") # ad % to labels
        ggplot(Tbl4Gene, aes(x = "", y = Freq, fill=Var1))+
            geom_bar(stat="identity",width=1)+
            coord_polar("y",start = 0) +
            ggtitle("Proportion of Pathogeny & Drug Response Genes")+
            theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
            geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5))
        # pie(slices, labels = lbls, radius=0.8,
        #     main="Proportion of Pathogeny & Drug Response Genes", clockwise = TRUE )
      }) # End of draw.GenePlot

      output$GenePlot <- renderPlot({draw.GenePlot()})

      # par(mar=c(0,0,1,0))
      # par(oma=c(0,0,1,0))
      #
      # slices <- c(10, 12, 6, 8)
      # lbls <- c('A', 'B', 'C', 'D')
      # pie(slices, label=lbls, main='Pie')





      ##### 3-6 Variant

      draw.VariantTable <- eventReactive(input$Show_VariantTable, {

        # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        #                                                                  server=input$ip,
        #                                                                  user=input$user,
        #                                                                  password=input$pw,
        #                                                                  schema=input$schema)
        #
        # connection <<- DatabaseConnector::connect(connectionDetails)
        sql <- "SELECT B.target_gene_source_value, A.variant_exon_number,
        A.hgvs_p, A.sequence_alteration, A.variant_feature,
        C.variant_origin, C.variant_pathogeny
        FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
        LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
        # cohort_forGenes <- sql_query(sql)
        # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
        # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
        # cohort_forPathogeny <<- DatabaseConnector::querySql(connection, sql)
        cohort_forPathogeny <- sql_query(sql,input)

        cohort_forGenes <<- cohort_forPathogeny[cohort_forPathogeny$VARIANT_PATHOGENY %in%
                                                  c('Pathogenic/Likely pathogenic', 'Drug response') &
                                                  cohort_forPathogeny$VARIANT_ORIGIN %in% 'somatic', ]


        colnames(cohort_forGenes) <- c('Gene', 'Exon', 'HGVSp',
                                       'Sequence Alteration', 'Variant Feature', 'Origin', 'Pathogeny')
        cohort_forGenes <- cohort_forGenes[order(cohort_forGenes$Gene),]
        cohort_forGenes


      }) # End of draw.VariantTable

      output$VariantTable <- renderTable({draw.VariantTable()})



      ##### 3-7 Search

      draw.Custom_Result_Plot <- eventReactive(input$Show_Custom, {

        # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        #                                                                  server=input$ip,
        #                                                                  user=input$user,
        #                                                                  password=input$pw,
        #                                                                  schema=input$schema)
        #
        # connection <<- DatabaseConnector::connect(connectionDetails)

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_c, A.hgvs_p,
                A.sequence_alteration, A.variant_feature, C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
        # CustomizedTbl <- sql_query(sql)
        # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
        # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
        # CustomizedTbl <<- DatabaseConnector::querySql(connection, sql)
        CustomizedTbl <- sql_query(sql,input)

        total = length(unique(CustomizedTbl$PERSON_ID))

        CustomizedTbl <- CustomizedTbl[CustomizedTbl$TARGET_GENE_SOURCE_VALUE %in% input$CstmGene
                                        & CustomizedTbl$HGVS_P %in% input$CstmHGVSp, ]

        cohort = length(unique(CustomizedTbl$PERSON_ID))
        not_cohort = total-cohort

        cohort_label = paste0(input$CstmGene, '_', input$CstmHGVSp)

        Tbl4Cstm <- data.frame('Var1'=c(cohort_label, 'Others'), 'Freq'=c(cohort, not_cohort))

        graphics.off()
        par(mar=c(0,0,1,0))
        par(oma=c(0,0,1,0))

        Draw_pieplot(Tbl4Cstm)
        # slices <- Tbl4Cstm$Freq
        # lbls <- as.character(Tbl4Cstm$Var1)
        # pct <- round(slices/sum(slices)*100)
        # lbls <- paste(lbls, pct) # add percents to labels
        # lbls <- paste(lbls,"%",sep="") # ad % to labels
        pie(slices, labels = lbls, clockwise = TRUE, radius=1,
            main="Proportion of Patients You Choose",
            col=(c('orange', 'ivory')))


      }) # End of draw.Custom_Result_Plot



      # Custom_Result_Table
      draw.Custom_Result_Table <- eventReactive(input$Show_Custom, {

        # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        #                                                                  server=input$ip,
        #                                                                  user=input$user,
        #                                                                  password=input$pw,
        #                                                                  schema=input$schema)
        #
        # # connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
        # #                                                                  server='128.1.99.58',
        # #                                                                  user='ssj',
        # #                                                                  password='ssj1225',
        # #                                                                  schema='SSJ_GCDM_AJOU_v3')
        #
        # connection <<- DatabaseConnector::connect(connectionDetails)

        #schema='SSJ_GCDM_AJOU_v3'
        #Cohort_table = 'cohort'

        sql <- "SELECT B.target_gene_source_value, A.hgvs_c, A.hgvs_p,
                A.sequence_alteration, A.variant_feature, C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
        # CustomizedTbl <- sql_query(sql)
        # sql <- SqlRender::renderSql(sql, schema=input$schema, Cohort_table=input$Cohort_table)$sql
        # #sql <- SqlRender::renderSql(sql, schema=schema, Cohort_table=Cohort_table)$sql
        # sql <- SqlRender::translateSql(sql, targetDialect=connectionDetails$dbms)$sql
        # CustomizedTbl <- DatabaseConnector::querySql(connection, sql)
        CustomizedTbl <- sql_query(sql,input)

        #dim(CustomizedTbl)
        #CstmGene = 'EGFR'
        #CstGene = NA
        #CstmHGVSp = 'p.Leu858Arg'
        #CstmHGVSp = NA

        CustomizedTbl <- CustomizedTbl[CustomizedTbl$TARGET_GENE_SOURCE_VALUE %in% input$CstmGene
                                        & CustomizedTbl$HGVS_P %in% input$CstmHGVSp, ]

        colnames(CustomizedTbl) <- c('Gene', 'HGVSc', 'HGVSp',
                                     'Structural Change', 'Functional Change', 'Origin', 'Pathogeny')
        CustomizedTbl

      }) # End of draw.Custom_Result_Table


      output$Custom_Result_Plot <- renderPlot({draw.Custom_Result_Plot()})
      output$Custom_Result_Table <- renderTable({draw.Custom_Result_Table()})

      onSessionEnded(function(){
          message("Disconnect server.")
          DatabaseConnector::disconnect(connection)
          message("Genomic closed.")
          })
  } # End of server
)


