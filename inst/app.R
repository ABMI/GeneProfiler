
check.packages("xlsx")
check.packages("data.table")
check.packages("shinyjs")
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

  ui = dashboardPage(

    #1
    dashboardHeader(title='GeneProfiler'),

    #2
    dashboardSidebar(sidebarMenu(menuItem('DB connection', tabName='db'),
                                 menuItem('Overall Profile', tabName='WaterFall'),
                                 menuItem('Variant Type', tabName='VariantType'),
                                 menuItem('Pathogeny', tabName='PathogenyPlot'),
                                 menuItem('Gene', tabName='GenePlot'),
                                 menuItem('Variant', tabName='VariantTable'),
                                 menuItem('Search Yours', tabName='Search'),
                                 menuItem('Query', tabName='Query'),
                                 menuItem('Graph', tabName='Graph')
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
                            textInput("ip","IP")
                            ,textInput("user","USER")
                            ,passwordInput("pw","PASSWORD")
                            ,textInput("schema","GCDM Database", 'SSJ_GCDM_AJOU_v1')
                            ,textInput("Cohort_table","Cohort Table", 'cohort')
                            ,textInput("Cohort_definition_id","Cohort Definition Id", '')
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
                            actionButton(inputId = 'Show_WF', label = 'Go!'),
                            width =2
                        ),

                        mainPanel(
                            plotOutput(outputId = "WF_Plot", height = "700px"),
                            width=12)

                    ),fluidRow(
                        downloadButton(outputId = 'download_plot', label = "Download Plot")
                    )),


            # 3-3 Variant Type
            tabItem(tabName = "VariantType",
                    fluidRow(
                        titlePanel("Variant Type"),

                        sidebarPanel(
                            box(column(checkboxGroupInput("str_selector","Structual Type : ", selected = "Total"),
                            actionButton(inputId = 'Show_VariantStructualType', label = 'Go!'),width = 6)),
                            box(column(checkboxGroupInput("func_selector", "Functional Type : ", selected = "Total"),
                            actionButton(inputId = 'Show_VariantFunctionalType', label = 'Go!'),width =6)),
                            width = 12
                        ),

                        mainPanel(
                            box(column(plotOutput(outputId = "VariantStructualType_Plot"),width = 12),
                                column(tableOutput(outputId = "VariantStructual_Tbl"),width = 12),width = 6),
                            box(column(width = 12, plotOutput(outputId = "VariantFunctionalType_Plot")),
                                column(width = 12, tableOutput(outputId = "VariantFunctional_Tbl")),width = 6),
                            width=12)
                    ),fluidRow(
                        downloadButton(outputId = 'download_structual_plot', label = "Download Variant Structual Plot"),
                        downloadButton(outputId = 'download_structual_tbl', label = "Download Variant Structual Table"),
                        downloadButton(outputId = 'download_functional_plot', label = "Download Variant Functional Plot"),
                        downloadButton(outputId = 'download_functional_tbl', label = "Download Variant Functional Table")
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
                            box(column(width = 12, plotOutput(outputId = "Origin_Plot")),
                                column(width = 12, tableOutput(outputId = "Origin_Table")),width = 6),
                            box(column(width = 12, plotOutput(outputId = "Pathogeny_Plot")),
                                column(width = 12, tableOutput(outputId = "Pathogeny_Table")),width = 6),
                            width=12)
                    ),fluidRow(
                        downloadButton(outputId = 'download_origin_plot', label = "Download Variant Origin Plot"),
                        downloadButton(outputId = 'download_origin_tbl', label = "Download Variant Origin Table"),
                        downloadButton(outputId = 'download_pathogeny_plot', label = "Download Variant Pathogeny Plot"),
                        downloadButton(outputId = 'download_pathogeny_tbl', label = "Download Variant Pathogeny Table")
                    )),



            # 3-5 Pathogeny & Drug Response Genes
            tabItem(tabName = "GenePlot",
                    fluidRow(
                        titlePanel("Gene Plot"),

                        sidebarPanel(
                            actionButton(inputId = 'Show_GenePlot', label = 'Go!'), width=2),

                        mainPanel(
                            plotOutput(outputId = "GenePlot"), width=12)
                    ),fluidRow(
                        downloadButton(outputId = 'download_gene_plot', label = "Download Gene Plot")
                    )),


            # 3-6 Variant
            tabItem(tabName = "VariantTable",
                    fluidRow(
                        titlePanel("Variant Table"),

                        sidebarPanel(
                            actionButton(inputId = 'Show_VariantTable', label = 'Go!'), width=2),

                        mainPanel(
                            tableOutput(outputId = "VariantTable"), width=12)
                    ),fluidRow(
                        downloadButton(outputId = 'download_variant_tbl', label = "Download Variant Table")
                    )),

            # 3-7 Search Yours
            tabItem(tabName = "Search",
                    fluidRow(
                        titlePanel("Explore Your Data!"),

                        sidebarPanel(

                            column(12,selectInput(inputId = 'CstmGene', 'Gene Symbol', choices = NULL , multiple = T),
                            actionButton(inputId = 'Add_Gene', label = 'Add!'),
                            uiOutput(outputId = "Custom_HVGSp")),
                            column(12,selectInput(inputId = 'CstmHGVSp', 'HVGSp', choices = NULL , multiple = T),
                            actionButton(inputId = 'Add_HVGSp', label = 'Add!'),
                            uiOutput(outputId = "Custom_UI")),


                            actionButton(inputId = 'Show_Custom', label = 'Go!')
                            ,width = 12),

                        mainPanel(
                            tableOutput(outputId = "Custom_Result_Table"),width =12)
                    ),fluidRow(
                        downloadButton(outputId = 'download_custom_tbl', label = "Download Custom Table")
                    )),

            # 3-8 Search Query
            tabItem(tabName = "Query",
                    fluidRow(
                        titlePanel("Explore Your Graph!"),

                        sidebarPanel(
                            textAreaInput(inputId = "Query_Field",label = "Target Query", height = '400px',value = readSql("extdata/Co_work.sql"))
                            # textOutput(outputId = "Query_Field", label = "Target Query")
                            ,actionButton(inputId = 'Show_Query', label = 'GO!')
                            ,width = 12),

                        mainPanel(
                            # plotOutput(outputId = "Custom_Result_Plot"),
                            tableOutput(outputId = "Custom_Query_Table"),width =12)
                    ),fluidRow(
                        # downloadButton(outputId = 'download_custom_plot', label = "Download Custom Plot"),
                        downloadButton(outputId = 'download_query_table', label = "Download query table")
                    )),

            # 3-9 Search Graph
            tabItem(tabName = "Graph",
                    fluidRow(
                        titlePanel("Explore Your Graph!"),

                        sidebarPanel(
                            fileInput(inputId = "file1", "Choose File",
                                      accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                            radioButtons(inputId = 'fileExt', label = 'File extension', choices = c("csv", "xlsx"), selected = "csv")
                            ,actionButton(inputId = 'Show_Graph', label = 'Go!')
                            ,width = 12),

                        mainPanel(
                            # plotOutput(outputId = "Custom_Result_Plot"),
                            plotOutput(outputId = "Custom_Result_Graph"),width =12)
                    ),fluidRow(
                        # downloadButton(outputId = 'download_custom_plot', label = "Download Custom Plot"),
                        downloadButton(outputId = 'download_custom_plot', label = "Download Custom Plot")
                    ))

        ))), # End of dashboardPage
############################################################



  server = function(input, output, session) {

    ##### 3-1 DB Connection
    Connect.DB <- eventReactive(input$db_load, {

        #Reconnect other DB
        Connect_DB("sql server", input$ip, input$user, input$pw, input$schema)
    })
    Update.UI <- eventReactive(input$db_load, {
        sql <- readSql("extdata/Sequence_alteration.sql")
        cohort_forMT <- sql_query(sql,input)
        Select_Structual <- table(cohort_forMT$SEQUENCE_ALTERATION)
        Tbl4StructualBar <- as.data.frame(Select_Structual)

        sql <- readSql("extdata/Variant_feature.sql")
        cohort_forMT <- sql_query(sql,input)
        Select_Functional <- table(cohort_forMT$VARIANT_FEATURE)
        Tbl4FunctionalBar <- as.data.frame(Select_Functional)

        updateCheckboxGroupInput(session, "str_selector", choices = c("Total", as.character(Tbl4StructualBar$Var1)),selected = input$str_selector)
        updateCheckboxGroupInput(session, "func_selector", choices = c("Total", as.character(Tbl4FunctionalBar$Var1)),selected = input$func_selector)

        sql <- readSql("extdata/Gene_setting.sql")
        CustomizedTbl <- sql_query(sql,input)

        updateSelectInput(session, "CstmGene", choices = c(as.character(CustomizedTbl$TARGET_GENE1_SYMBOL)))
        # updateSelectInput(session, "CstmHGVSp", choices = c(as.character(CustomizedTbl$HGVS_P)), selected = 'p.Leu858Arg')
        removeModal()
        showModal(modalDialog(title="Load data","Initializing Complete.",footer = modalButton("OK")))
    })

    output$DB_Connect <- renderText({Connect.DB()
        Update.UI()
        })



    ##### 3-2 WaterFall Plot
    draw.WF <- eventReactive(input$Show_WF, {

      sql <- readSql("extdata/Waterfall.sql")
      cohort_variant <- sql_query(sql,input)
      # View(cohort_variant)

      cohort_variant$VARIANT_FEATURE[cohort_variant$SEQUENCE_ALTERATION == 'Amplification'] <- 'Amplification'

      cohort_variant$VARIANT_FEATURE[cohort_variant$SEQUENCE_ALTERATION == 'Deletion'] <- 'Deletion'

      cohort_variant <- cohort_variant[complete.cases(cohort_variant[ , c("VARIANT_FEATURE")]), ]

      cohort_variant <- cohort_variant[cohort_variant$VARIANT_FEATURE!='Intron', ]

      cohort_variant <- cohort_variant[cohort_variant$VARIANT_FEATURE!='Synonymous', ]

      cohort_variant <- cohort_variant[cohort_variant$VARIANT_FEATURE!='Other', ]

      cohort_variant <- cohort_variant[complete.cases(cohort_variant[ , c("TARGET_GENE1_SYMBOL")]), ]

      # Create a data frame of random elements to plot
      inputData <- data.frame("sample" = cohort_variant$PERSON_ID,
                              "gene" = cohort_variant$TARGET_GENE1_SYMBOL,
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
    output$download_plot <- downloadHandler(
        filename = "WF_PLOT.pdf", content = function(file){
            pdf(file,width = 12, height =10)
            GenVisR::waterfall(inputData, fileType="Custom", variant_class_order = most_deleterious, plotMutBurden = FALSE,
                               mainXlabel = TRUE, maxGenes=50, mainGrid = TRUE, mainLabelSize = 1,
                               plot_proportions = TRUE, section_heights = c(0, 400, 60))
            dev.off()
        }
    )

      output$WF_Plot <- renderPlot({draw.WF()})


      ##### 3-3-1 Variant Structual Type

      draw.VariantStructualType <- eventReactive(input$Show_VariantStructualType, {

        sql <- readSql("extdata/Waterfall.sql")
        cohort_forMT <- sql_query(sql,input)

        Select_Structual <- table(cohort_forMT$SEQUENCE_ALTERATION)

        Tbl4StructualBar <- as.data.frame(Select_Structual)
        Draw_barplot(Tbl4StructualBar,"Structual Variant Types",input$str_selector)
        # observe({
        #     updateCheckboxGroupInput(session, "str_selector", choices = c("Total", as.character(Tbl4StructualBar$Var1)),selected = input$str_selector)
        # })
        })
      draw.VariantStructualTbl <- eventReactive(input$Show_VariantStructualType, {

          sql <- readSql("extdata/Waterfall.sql")
          cohort_forMT <- sql_query(sql,input)
          Select_Structual <- as.data.frame(table(cohort_forMT$SEQUENCE_ALTERATION))
          if(is.element("Total",input$str_selector)){
              showStructualTbl <- Select_Structual
          }else{
              showStructualTbl <- Select_Structual[Select_Structual$Var1%in%c(input$str_selector),]
          }
          showStructualTbl
      })# End of draw.VariantStructual

      ##### 3-3-2 Variant Functional Type

      draw.VariantFunctionalType <- eventReactive(input$Show_VariantFunctionalType, {

          sql <- readSql("extdata/Waterfall.sql")
          cohort_forMT <- sql_query(sql,input)

          Select_Functional <- table(cohort_forMT$VARIANT_FEATURE)

          Tbl4FunctionalBar <- as.data.frame(Select_Functional)
          Draw_barplot(Tbl4FunctionalBar,"Functional Variant Types",input$func_selector)

          # observe({
          #     updateCheckboxGroupInput(session, "func_selector", choices = c("Total", as.character(Tbl4FunctionalBar$Var1)),selected = input$func_selector)
          # })
      })

      draw.VariantFunctionalTbl <- eventReactive(input$Show_VariantFunctionalType, {

          sql <- readSql("extdata/Waterfall.sql")
          cohort_forMT <- sql_query(sql,input)
          Select_Functional <- as.data.frame(table(cohort_forMT$VARIANT_FEATURE))
          if(is.element("Total",input$func_selector)){
              showFunctionalTbl <- Select_Functional
          }else{
              showFunctionalTbl <- Select_Functional[Select_Functional$Var1%in%c(input$func_selector),]
          }
          showFunctionalTbl
      })# End of draw.VariantFunctional

      output$download_structual_plot <- downloadHandler(
          filename <- "variant_structual.pdf" ,
          content = function(file){
              pdf(file, width = 12, height = 6)
              print(Draw_barplot(Tbl4StructualBar,"Structual Variant Types",input$str_selector))
              dev.off()
          }
      )
      output$download_structual_tbl <- downloadHandler(
          filename = "variant_structual.csv",
          content = function(file) {
              write.csv(draw.VariantStructualTbl(), file)
          }
      )

      output$download_functional_plot <- downloadHandler(
          filename <- "variant_function.pdf" ,
          content = function(file){
              pdf(file, width = 12, height = 6)
              print(Draw_barplot(Tbl4FunctionalBar,"Functional Variant Types",input$func_selector))
              dev.off()
          }
      )
      output$download_functional_tbl <- downloadHandler(
          filename = "variant_function.csv",
          content = function(file) {
              write.csv(draw.VariantFunctionalTbl(), file)
          }
      )

      output$VariantStructualType_Plot <- renderPlot({draw.VariantStructualType()})
      output$VariantFunctionalType_Plot <- renderPlot({draw.VariantFunctionalType()})

      output$VariantStructual_Tbl <- renderTable({draw.VariantStructualTbl()})
      output$VariantFunctional_Tbl <- renderTable({draw.VariantFunctionalTbl()})



      ##### 3-4 Pathogeny Plot

      draw.OriginPlot <- eventReactive(input$Show_Origin, {

        sql <- readSql("extdata/PathogenyPlot.sql")

        cohort_forPathogeny <- sql_query(sql,input)

        par(mar=c(2,1,1,0))
        par(oma=c(0,1,2,1))

        Tbl4OriginPie <- as.data.frame(table(cohort_forPathogeny$VALUE_AS_STRING))
        Draw_pieplot(Tbl4OriginPie)
        ggplot(Tbl4OriginPie, aes(x = "", y = Freq, fill=Var1))+
            geom_bar(stat="identity",width=1)+
            coord_polar("y",start = 0) +
            ggtitle("Variant Origin")+
            geom_col(color = 'black', width = 100)+
            theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
            geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5))

      })

      draw.OriginTable <- eventReactive(input$Show_Origin, {
          sql <- readSql("extdata/PathogenyPlot.sql")
          cohort_forPathogeny <- sql_query(sql,input)

          Tblorigin <- as.data.frame(table(cohort_forPathogeny$VALUE_AS_STRING))
          Tblorigin
      }) # End of draw.Origin

      draw.PathogenyPlot <- eventReactive(input$Show_Pathogeny, {

          sql <- readSql("extdata/PathogenyPlot.sql")
          cohort_forPathogeny <- sql_query(sql,input)

          par(mar=c(2,1,1,0))
          par(oma=c(0,1,2,1))

          Tbl4PathogenyPie <- as.data.frame(table(cohort_forPathogeny$VALUE_AS_STRING))
          Draw_pieplot(Tbl4PathogenyPie)
          ggplot(Tbl4PathogenyPie, aes(x = "", y = Freq, fill=Var1))+
              geom_bar(stat="identity",width=1)+
              coord_polar("y",start = 0) +
              ggtitle("Variant Pathogeny")+
              geom_col(color = 'black', width = 100)+
              theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
              geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.6))
      })

      draw.PathogenyTable <- eventReactive(input$Show_Pathogeny, {
          sql <- readSql("extdata/PathogenyPlot.sql")
          cohort_forPathogeny <- sql_query(sql,input)

          Tblpathogeny <- as.data.frame(table(cohort_forPathogeny$VALUE_AS_STRING))
          Tblpathogeny
      }) # End of draw.Pathogeny

      output$download_origin_plot <- downloadHandler(
          filename <- "VALUE_AS_STRING.pdf" ,
          content = function(file){
              pdf(file, width = 12, height = 6)
              Draw_pieplot(Tbl4OriginPie)
              print(ggplot(Tbl4OriginPie, aes(x = "", y = Freq, fill=Var1))+
                  geom_bar(stat="identity",width=1)+
                  coord_polar("y",start = 0) +
                  ggtitle("Variant Origin")+
                  geom_col(color = 'black', width = 100)+
                  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
                  geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5)))
              dev.off()
          }
          ,contentType = "pdf"
      )
      output$download_origin_tbl <- downloadHandler(
          filename = "VALUE_AS_STRING.csv",
          content = function(file) {
              write.csv(draw.OriginTable(), file)
          }
      )

      output$download_pathogeny_plot <- downloadHandler(
          filename <- "VALUE_AS_STRING.pdf" ,
          content = function(file){
              pdf(file, width = 12, height = 6)
              Draw_pieplot(Tbl4PathogenyPie)
              print(ggplot(Tbl4PathogenyPie, aes(x = "", y = Freq, fill=Var1))+
                  geom_bar(stat="identity",width=1)+
                  coord_polar("y",start = 0) +
                  ggtitle("Variant Pathogeny")+
                  geom_col(color = 'black', width = 100)+
                  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
                  geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.6)))
              dev.off()
          }
      )
      output$download_pathogeny_tbl <- downloadHandler(
          filename = "VALUE_AS_STRING.csv",
          content = function(file) {
              write.csv(draw.PathogenyTable(), file)
          }
      )

      output$Pathogeny_Plot <- renderPlot({draw.PathogenyPlot()})
      output$Origin_Plot <- renderPlot({draw.OriginPlot()})

      output$Pathogeny_Table <- renderTable({draw.PathogenyTable()})
      output$Origin_Table <- renderTable({draw.OriginTable()})

      ##### 3-5 Pathogeny & Drug Response Genes

      draw.GenePlot <- eventReactive(input$Show_GenePlot, {

        sql <- readSql("extdata/PathogenyPlot.sql")

        cohort_forPathogeny <- sql_query(sql,input)
        # View(cohort_forPathogeny)
        cohort_forGenes <- cohort_forPathogeny[cohort_forPathogeny$VALUE_AS_STRING %in%
                                                 c('Pathogenic/Likely pathogenic', 'Drug response') &
                                                 cohort_forPathogeny$VALUE_AS_STRING %in% 'Pathogenic/Likely pathogenic', ]
        Tbl4Gene <- as.data.frame(table(cohort_forGenes$TARGET_GENE1_SYMBOL))
        Tbl4Gene <- Tbl4Gene[order(-Tbl4Gene$Freq),]

        par(mar=c(0,0,1,0))
        par(oma=c(0,0,2,0))

        Draw_pieplot(Tbl4Gene)

        ggplot(Tbl4Gene, aes(x = "", y = Freq, fill=Var1))+
            geom_bar(stat="identity",width=1)+
            coord_polar("y",start = 0) +
            geom_col(color = 'black', width = 100)+
            ggtitle("Proportion of Pathogeny & Drug Response Genes")+
            theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
            geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5))

      }) # End of draw.GenePlot
      output$download_gene_plot <- downloadHandler(
          filename <- "variant_gene.pdf" ,
          content = function(file){
              pdf(file, width = 12, height = 6)
              Draw_pieplot(Tbl4Gene)
              print(ggplot(Tbl4Gene, aes(x = "", y = Freq, fill=Var1))+
                  geom_bar(stat="identity",width=1)+
                  coord_polar("y",start = 0) +
                  geom_col(color = 'black', width = 100)+
                  ggtitle("Proportion of Pathogeny & Drug Response Genes")+
                  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
                  geom_text_repel(aes(y= Freq, label = lbls), position = position_stack(vjust = 0.5)))
              dev.off()
          }
      )

      output$GenePlot <- renderPlot({draw.GenePlot()})

      ##### 3-6 Variant

      draw.VariantTable <- eventReactive(input$Show_VariantTable, {

        sql <- readSql("extdata/PathogenyPlot.sql")
        cohort_forPathogeny <- sql_query(sql,input)

        cohort_forGenes <- cohort_forPathogeny[cohort_forPathogeny$VALUE_AS_STRING %in%
                                                  c('Pathogenic/Likely pathogenic', 'Drug response') &
                                                  cohort_forPathogeny$VALUE_AS_STRING %in% 'Pathogenic/Likely pathogenic', ]


        colnames(cohort_forGenes) <- c('Person Id', 'Gene', 'Exon', 'HGVSp',
                                       'Sequence Alteration', 'Variant Feature', 'Origin')
        cohort_forGenes <- cohort_forGenes[order(cohort_forGenes$Gene),]
        cohort_forGenes


      }) # End of draw.VariantTable

      output$download_variant_tbl <- downloadHandler(
          filename <- "variant.csv" ,
          content = function(file){
              write.csv(draw.VariantTable(), file)
          }
      )

      output$VariantTable <- renderTable({draw.VariantTable()}, digits = 0)

      ##### 3-7 Search

      # Custom_Result_Table
      draw.hgvs_p <- eventReactive(input$Add_Gene, {
          if(length(input$CstmGene)>1){
              updateSelectInput(session, "CstmHGVSp", choices = "" , selected = "")
          }else{
              sql <- paste0("SELECT distinct(B.target_gene1_symbol), B.hgvs_p
                    FROM (SELECT person_id, specimen_id FROM specimen
                            WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
                            LEFT OUTER JOIN @schema.dbo.variant_occurrence B ON A.specimen_id = B.specimen_id
                            LEFT OUTER JOIN @schema.dbo.variant_annotation C ON B.variant_occurrence_id = C.variant_occurrence_id
                    WHERE B.target_gene1_symbol = '",input$CstmGene,"'")
              CustomizedTbl <- sql_query(sql,input)

              updateSelectInput(session, "CstmHGVSp", choices = unique(CustomizedTbl$HGVS_P))

          }
      })

      load.genelist <- eventReactive(input$Add_HVGSp, {
          genelist <- paste0(input$CstmGene , input$CstmHGVSp, collapse = "\n")
      })

      draw.Custom_Result_Table <- eventReactive(input$Show_Custom, {
          if(length(input$CstmGene)>1){
              sql <- readSql("extdata/Multi_gene.sql")

              CustomizedTbl <- unique(sql_query(sql,input))

              total <- length(unique(CustomizedTbl$PERSON_ID))
              resultTbl <- data.frame(input$CstmGene,unlist(lapply(input$CstmGene, function(x){length(unique(CustomizedTbl[CustomizedTbl$TARGET_GENE1_SYMBOL%in%x,]$PERSON_ID))}) ))
              colnames(resultTbl) <- c('Gene', 'Count')
              resultTbl$Count <- (resultTbl$Count / total) * 100
              resultTbl
          }else{
              sql <- readSql("extdata/Single_gene.sql")

              CustomizedTbl <- unique(sql_query(sql,input))
              total <- length(unique(CustomizedTbl$PERSON_ID))
              getcount <- function(gene, hgvsp){
                  unlist(lapply(hgvsp, function(x){length(unique(CustomizedTbl[CustomizedTbl$TARGET_GENE1_SYMBOL%in%gene & CustomizedTbl$HGVS_P%in%x,]$PERSON_ID))}) )
              }
              resultTbl <- data.frame(input$CstmGene, input$CstmHGVSp,  getcount(input$CstmGene, input$CstmHGVSp))
              colnames(resultTbl) <- c('Gene', 'HGVSp','Fraction')
              resultTbl$Fraction <- (resultTbl$Fraction / total)* 100
              resultTbl
          }


      }) # End of draw.Custom_Result_Table
      output$Custom_HVGSp <- renderUI(draw.hgvs_p())
      output$Custom_UI <- renderText({load.genelist()})
      output$Custom_Result_Table <- renderTable(draw.Custom_Result_Table(),digits = 2)

      output$download_custom_tbl <- downloadHandler(
          filename <- "custom.csv" ,
          content = function(file){
              write.csv(draw.Custom_Result_Table(), file)
          }
      )

      #### 3-8 Query
      draw.target <- eventReactive(input$Show_Query, {
          targetTbl <- sql_query(input$Query_Field, input)
          targetTbl
      })

      output$download_query_table <- downloadHandler(
          filename <- "queryTable.csv" ,
          content = function(file){
              write.csv(draw.target(), file, row.names = F)
          }
      )
      output$Custom_Query_Table <- renderTable(draw.target())

      ##### 3-9 Graph
      draw.Graph <- eventReactive(input$Show_Graph, {
          if(input$fileExt == 'csv'){
              tbls <- read.csv(file = input$file1$datapath,header = T)
              tblss <- melt(tbls, id.vars = "NAME")
          }else if(input$fileExt == 'xlsx'){
              tbl <- read.xlsx(input$file1$datapath,sheetName = 'Sheet1')
              tblss<-melt(tbl, id.vars = "NAME")
          }

          ggplot(tblss, aes(NAME,value, fill = variable)) +
              geom_bar(stat="identity", position="dodge") +
              geom_text(aes(label=value), vjust=-0.3, size= 2.5, position = position_dodge(0.75)) +
              ylab("Frequency of actionable mutation (%)") +
              labs(x = NULL, fill = "Institution")+
              coord_flip()

      }) # End of draw.VariantTable

      output$download_custom_plot <- downloadHandler(
          filename <- "Custom_plot.pdf" ,
          content = function(file){
              pdf(file, width = 12, height = 6)
              print(ggplot(tblss, aes(variable,value, fill = NA.)) +
                  geom_bar(stat="identity", position="dodge") +
                  coord_flip())
              dev.off()
          }
      )

      output$Custom_Result_Graph <- renderPlot({draw.Graph()})

          onSessionEnded(function(){
          message("Disconnect server.")
          DatabaseConnector::disconnect(connection)
          message("Genomic closed.")
          })
  } # End of server
)

