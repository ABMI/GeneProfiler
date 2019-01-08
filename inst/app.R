
check.packages("xlsx")

check.packages("shinydashboard")
check.packages("SqlRender")
check.packages("DatabaseConnector")
check.packages("dplyr")
check.packages("GenVisR")
check.packages("ggrepel")
check.packages("gridExtra")

############################################

shiny::shinyApp(

  # UI Definition

  ui <- dashboardPage(

    #1
    dashboardHeader(title='GeneProfiler'),

    #2
    dashboardSidebar(sidebarMenu(menuItem('DB connection', tabName='db'),
                                 menuItem('Overall Profile', tabName='WaterFall'),
                                 menuItem('Variant Type', tabName='VariantType'),
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
            tabItem(tabName = "VariantType",
                    fluidRow(
                        titlePanel("Variant Type"),

                        sidebarPanel(
                            selectInput("str_selector", "Structual Type : ", "Total", selected = "Total", multiple = T),
                            actionButton(inputId = 'Show_VariantStructualType', label = 'Go!'),
                            selectInput("func_selector", "Functional Type : ", "Total", selected = "Total", multiple = T),
                            actionButton(inputId = 'Show_VariantFunctionalType', label = 'Go!')
                        ),

                        mainPanel(
                            column(width = 6, plotOutput(outputId = "VariantStructualType_Plot")),
                            column(width = 6, plotOutput(outputId = "VariantFunctionalType_Plot")),
                            column(width = 6, tableOutput(outputId = "VariantStructual_Tbl")),
                            column(width = 6, tableOutput(outputId = "VariantFunctional_Tbl")),
                            width=12)
                    )),


            # 3-4 Pathogeny Plot
            tabItem(tabName = "PathogenyPlot",
                    fluidRow(
                        titlePanel("Pathogeny Plot"),

                        sidebarPanel(
                            actionButton(inputId = 'Show_Origin', label = 'View Variant Origin'),
                            actionButton(inputId = 'Show_Pathogeny', label = 'View Variant Pathogeny'),
                            width=8),

                        mainPanel(
                            column(width = 6, plotOutput(outputId = "Origin_Plot")),
                            column(width = 6, plotOutput(outputId = "Pathogeny_Plot")),
                            column(width = 6, tableOutput(outputId = "Origin_Table")),
                            column(width = 6, tableOutput(outputId = "Pathogeny_Table")),
                            width=12)
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
    })
    output$DB_Connect <- renderText({Connect.DB()})

    ##### 3-2 WaterFall Plot
    draw.WF <- eventReactive(input$Show_WF, {

      sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
              WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
              LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
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


      ##### 3-3-1 Variant Structual Type

      draw.VariantStructualType <- eventReactive(input$Show_VariantStructualType, {

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
        cohort_forMT <- sql_query(sql,input)

        Select_Structual <- table(cohort_forMT$SEQUENCE_ALTERATION)

        Tbl4BarPlot <- as.data.frame(Select_Structual)
        Draw_barplot(Tbl4BarPlot,"Structual Variant Types",input$str_selector)
        observe({
            updateSelectInput(session, "str_selector", choices = c("Total", as.character(Tbl4BarPlot$Var1)),selected = input$str_selector)
        })
        })
      draw.VariantStructualTbl <- eventReactive(input$Show_VariantStructualType, {

          sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
          cohort_forMT <- sql_query(sql,input)
          Select_Structual <- as.data.frame(table(cohort_forMT$SEQUENCE_ALTERATION))
          if(is.element("Total",input$str_selector)){
              showTbl <- Select_Structual
          }else{
              showTbl <- Select_Structual[Select_Structual$Var1%in%c(input$str_selector),]
          }
          showTbl
      })# End of draw.VariantStructual

      ##### 3-3-2 Variant Functional Type

      draw.VariantFunctionalType <- eventReactive(input$Show_VariantFunctionalType, {

          sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
          cohort_forMT <- sql_query(sql,input)

          Select_Functional <- table(cohort_forMT$VARIANT_FEATURE)

          Tbl4BarPlot <- as.data.frame(Select_Functional)
          Draw_barplot(Tbl4BarPlot,"Functional Variant Types",input$func_selector)

          observe({
              updateSelectInput(session, "func_selector", choices = c("Total", as.character(Tbl4BarPlot$Var1)),selected = input$func_selector)
          })
      })

      draw.VariantFunctionalTbl <- eventReactive(input$Show_VariantFunctionalType, {

          sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
          FROM (SELECT * FROM @schema.dbo.variant_occurrence
          WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
          LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id"
          cohort_forMT <- sql_query(sql,input)
          Select_Functional <- as.data.frame(table(cohort_forMT$VARIANT_FEATURE))
          if(is.element("Total",input$func_selector)){
              showTbl <- Select_Functional
          }else{
              showTbl <- Select_Functional[Select_Functional$Var1%in%c(input$func_selector),]
          }
          showTbl
      })# End of draw.VariantFunctional

      output$VariantStructualType_Plot <- renderPlot({draw.VariantStructualType()})
      output$VariantFunctionalType_Plot <- renderPlot({draw.VariantFunctionalType()})

      output$VariantStructual_Tbl <- renderTable({draw.VariantStructualTbl()})
      output$VariantFunctional_Tbl <- renderTable({draw.VariantFunctionalTbl()})



      ##### 3-4 Pathogeny Plot

      draw.OriginPlot <- eventReactive(input$Show_Origin, {

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
                       A.hgvs_p, A.sequence_alteration, A.variant_feature,
                       C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                      WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
        cohort_forPathogeny <- sql_query(sql,input)

        par(mar=c(2,1,1,0))
        par(oma=c(0,1,2,1))

        Tbl4PiePlot <- as.data.frame(table(cohort_forPathogeny$VARIANT_ORIGIN))
        Draw_pieplot(Tbl4PiePlot)
        pie(slices, lbls , radius=0.6,
            main="Variant Origin", clockwise = TRUE)

      })

      draw.OriginTable <- eventReactive(input$Show_Origin, {
          sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
                       A.hgvs_p, A.sequence_alteration, A.variant_feature,
                       C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                      WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
          cohort_forPathogeny <- sql_query(sql,input)

          Tblorigin <- as.data.frame(table(cohort_forPathogeny$VARIANT_ORIGIN))
          Tblorigin
      }) # End of draw.Origin

      draw.PathogenyPlot <- eventReactive(input$Show_Pathogeny, {

          sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
          A.hgvs_p, A.sequence_alteration, A.variant_feature,
          C.variant_origin, C.variant_pathogeny
          FROM (SELECT * FROM @schema.dbo.variant_occurrence
          WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
          LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
          LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
          cohort_forPathogeny <- sql_query(sql,input)

          par(mar=c(2,1,1,0))
          par(oma=c(0,1,2,1))

          Tbl4PiePlot <- as.data.frame(table(cohort_forPathogeny$VARIANT_PATHOGENY))
          Draw_pieplot(Tbl4PiePlot)
          pie(slices, lbls , radius=0.6,
              main="Variant Pathogeny", clockwise = TRUE )
      })

      draw.PathogenyTable <- eventReactive(input$Show_Pathogeny, {
          sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
                       A.hgvs_p, A.sequence_alteration, A.variant_feature,
                       C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                      WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
          cohort_forPathogeny <- sql_query(sql,input)

          Tblpathogeny <- as.data.frame(table(cohort_forPathogeny$VARIANT_PATHOGENY))
          Tblpathogeny
      }) # End of draw.Pathogeny

      output$Pathogeny_Plot <- renderPlot({draw.PathogenyPlot()})
      output$Origin_Plot <- renderPlot({draw.OriginPlot()})

      output$Pathogeny_Table <- renderTable({draw.PathogenyTable()})
      output$Origin_Table <- renderTable({draw.OriginTable()})


      ##### 3-5 Pathogeny & Drug Response Genes

      draw.GenePlot <- eventReactive(input$Show_GenePlot, {

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.variant_exon_number,
        A.hgvs_p, A.sequence_alteration, A.variant_feature,
        C.variant_origin, C.variant_pathogeny
        FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
        LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"

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

        ggplot(Tbl4Gene, aes(x = "", y = Freq, fill=Var1))+
            geom_bar(stat="identity",width=1)+
            coord_polar("y",start = 0) +
            ggtitle("Proportion of Pathogeny & Drug Response Genes")+
            theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
            geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5))

      }) # End of draw.GenePlot

      output$GenePlot <- renderPlot({draw.GenePlot()})

      ##### 3-6 Variant

      draw.VariantTable <- eventReactive(input$Show_VariantTable, {

        sql <- "SELECT B.target_gene_source_value, A.variant_exon_number,
        A.hgvs_p, A.sequence_alteration, A.variant_feature,
        C.variant_origin, C.variant_pathogeny
        FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
        LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
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

        sql <- "SELECT A.person_id, B.target_gene_source_value, A.hgvs_c, A.hgvs_p,
                A.sequence_alteration, A.variant_feature, C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
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

        pie(slices, labels = lbls, clockwise = TRUE, radius=1,
            main="Proportion of Patients You Choose",
            col=(c('orange', 'ivory')))


      }) # End of draw.Custom_Result_Plot



      # Custom_Result_Table
      draw.Custom_Result_Table <- eventReactive(input$Show_Custom, {

        sql <- "SELECT B.target_gene_source_value, A.hgvs_c, A.hgvs_p,
                A.sequence_alteration, A.variant_feature, C.variant_origin, C.variant_pathogeny
                FROM (SELECT * FROM @schema.dbo.variant_occurrence
                WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id"
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

