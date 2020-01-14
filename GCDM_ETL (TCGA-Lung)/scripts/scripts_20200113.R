
# < TCGA into GCDM ETL Process >  - 20200113 SSJ
# 
# - DB Connection
# - DROP GCDM Tables
# - Create GCDM Tables
# - Insert XLSX data into GCDM
#   (0) genomic_test
#   (1) target_gene
#   (2) variant_occurrence
#   (3) variant_annotation 

Sys.setlocale(category = "LC_ALL", locale = "us")


##################################################################
# Function Definition < DB Connection >
##################################################################

connectGCDM <- function(dbms, server, schema, user, password)
{
  tryCatch({
    connectionDetails<-createConnectionDetails(dbms=dbms,
                                               server=server,
                                               schema=schema,
                                               user=user,
                                               password=password)
    connection<-connect(connectionDetails)}
    , error = function(e) {
      stop(e)
    }
  )
  
  print("DB connected!")
  
  return (list(connection, connectionDetails))
}



###################################################################
# Function Definition < DROP GCDM TABLEs >
###################################################################

dropTableGCDM <- function (conn, schema)
{
  connection = conn[[1]]
  connectionDetails = conn[[2]]
  
  sql <- "IF OBJECT_ID ('@schema.person', 'U') IS NOT NULL DROP TABLE @schema.person;"
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.specimen', 'U') 
                IS NOT NULL DROP TABLE @schema.specimen;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.condition_occurrence', 'U') 
                IS NOT NULL DROP TABLE @schema.condition_occurrence;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.procedure_occurrence', 'U') 
                IS NOT NULL DROP TABLE @schema.procedure_occurrence;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.genomic_test', 'U') 
                IS NOT NULL DROP TABLE @schema.genomic_test;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.target_gene', 'U') 
                IS NOT NULL DROP TABLE @schema.target_gene;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.variant_occurrence', 'U') 
                IS NOT NULL DROP TABLE @schema.variant_occurrence;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.variant_annotation', 'U') 
                IS NOT NULL DROP TABLE @schema.variant_annotation;")
  
  sql <- paste0(sql, "IF OBJECT_ID ('@schema.cohort', 'U') 
                IS NOT NULL DROP TABLE @schema.cohort;")
  
  
  sql <- renderSql(sql, schema=connectionDetails$schema)$sql
  sql <- translateSql(sql, targetDialect=connectionDetails$dbms)$sql
  executeSql(connection, sql)
  
  print("DROP GCDM Tables is completed!")
}




#####################################################################
# Function Definition < CREATE GCDM TABLEs >
#####################################################################

createTableGCDM <- function (conn, schema, ddlFile)
{
  connection = conn[[1]]
  connectionDetails = conn[[2]]
  
  sql <- suppressWarnings(readSql(ddlFile))
  sql <- renderSql(sql, schema=connectionDetails$schema)$sql
  sql <- translateSql(sql, targetDialect=connectionDetails$dbms)$sql
  executeSql(connection, sql)
  
  print("CREATE GCDM TABLEs succeded!")
}

 


#####################################################################
# Insert XLSX data into GCDM
#####################################################################

insertXLSXIntoGCDM <- function (conn, schema, variantDir, clinicalFile) {
  
  connection = conn[[1]]
  connectionDetails = conn[[2]]
  
  ### Load Clinical Information
  if (!file.exists(clinicalFile)) { 
    print(paste0(clinicalFile, " doesn't exists.")) 
    stop()
  }
  
  clinicalData = read.xlsx(clinicalFile, 1, stringsAsFactors=F, encoding='UTF-8')
  
  TotalvariantFile = read.xlsx(variantFile, sheetIndex = 1)
  
  #dim(clinicalData)       # 1144, 22
  #dim(TotalvariantFile)   # 2904, 23 
  #View(clinicalData)
  #View(TotalvariantFile)
  
  #length(TargetGenePatientList) # 1060
  TargetGenePatientList <- unique(TotalvariantFile$case_id)  
  
  TargetGeneClinicalData <- clinicalData[clinicalData$Sample.ID %in% TargetGenePatientList, ]
  #dim(TargetGeneClinicalData)  # 1060, 22
  #View(TargetGeneClinicalData)
    
  # View(TargetGeneClinicalData)
  # View(TotalvariantFile)
  
  TargetGeneClinicalData$Person_id <- c(1:length(TargetGeneClinicalData$Sample.ID))
  # dim(TargetGeneClinicalData) # 1060, 23
  
  
  
  ########################################################################################
  ########################################################################################
  # 1. Inserting data into "person" table 
  
  # Sex: Female(8532), Male(8507)
  TargetGeneClinicalData$sex_concept_id <- 0
  TargetGeneClinicalData$sex_concept_id[TargetGeneClinicalData$Sex=='Female'] <- 8532
  TargetGeneClinicalData$sex_concept_id[TargetGeneClinicalData$Sex=='Male'] <- 8507
  TargetGeneClinicalData$sex_concept_id[TargetGeneClinicalData$Sex=='unknown'] <- 0
  unique(TargetGeneClinicalData$sex_concept_id)
  

  person <- data.frame(
    
    person_id <- TargetGeneClinicalData$Person_id,
    
    gender_concept_id = TargetGeneClinicalData$sex_concept_id,
    
    year_of_birth = 0 , #NA	
    month_of_birth = 0 , #NA	
    day_of_birth = 0 , #NA	
    birth_datetime = '1900-01-01' , #NA
    
    race_concept_id = 0,
    ethnicity_concept_id = 0,
    location_id = 0,
    provider_id = 0,
    care_site_id = 0,
    
    person_source_value = TargetGeneClinicalData$Sample.ID,
    gender_source_value = TargetGeneClinicalData$Sex,			
    gender_source_concept_id = 0,	
    
    race_source_value = '0' , #NA			
    race_source_concept_id = 0,		
    ethnicity_source_value = 0,		
    ethnicity_source_concept_id = 0
  )
  # dim(person) # 1060, 18
  # View(person)
  
  print("Inserting data into person table")
  colnames(person)[1] <- 'person_id'
  
  #View(person)
  insertTable(connection, paste0(schema,".","person"), person,
              dropTableIfExists = F,
              createTable = F,
              tempTable = F,
              oracleTempSchema = NULL)
  
  
  print("Inserted.")
  
  ############################################ END 1. Inserting into "person" table
  ########################################################################################
  
  
  
  
  
  
  ########################################################################################
  ########################################################################################
  # 2. Inserting data into "specimen" table 
  
  conceptTbl <- read.xlsx(conceptID, sheetIndex = 2)
  #dim(conceptTbl) #58, 4
  #View(conceptTbl)
  #head(conceptTbl, 12)
  #View(TargetGeneClinicalData)
  

  specimen <- data.frame(
    
    person_id = TargetGeneClinicalData$Person_id,
    
    specimen_concept_id	= 0,			
    specimen_type_concept_id = 581378,  # EHR Detail
    
    specimen_date = '1900-01-01',
    specimen_datetime = '1900-01-01 00:00:00.0000000',
    quantity = 1,
    
    unit_concept_id	= 0,
    
    anatomic_site_concept_id = 4111459,	 # Lung
    
    disease_status_concept_id = 4066212, # Malignant
    
    specimen_source_id = NA, 
    specimen_source_value = NA, # FFPE
    
    unit_source_value = NA,			
    
    anatomic_site_source_value = 'Lung',	
    
    disease_status_source_value	= 'Malignant'
  )
  # dim(specimen) # 1060, 14
  # View(specimen)
  
  print("Inserting data into specimen table")
  
  insertTable(connection, tableName = paste0(schema,".", "specimen"), data = specimen,
              dropTableIfExists = FALSE,
              createTable = FALSE,
              tempTable=FALSE,
              oracleTempSchema = NULL)
  
  print("Inserted.")
  
  ############################################ END 2. Inserting into "specimen" table
  ########################################################################################
  
  
  
  
  ########################################################################################
  ########################################################################################
  # 3. Inserting data into "condition_occurrence" table 
  
  ### condition_occurrence 
  # View(clinicalData)
  # View(conceptTbl)
  
  # ## condition_concept_id
  # TargetGeneClinicalData$condition_concept_id <- 0
  # 
  # for (i in clinicalData$path_dx){
  #   id <- conceptTbl$ID[conceptTbl$source_value == i]
  #   clinicalData$condition_concept_id[clinicalData$path_dx == i] <- as.character(id) }
  # # View(clinicalData[, c("path_dx", "condition_concept_id")])
  # 
  # ## condition_source_concept_id
  # TargetGeneClinicalData$condition_source_concept_id <- 0
  # 
  # for (i in clinicalData$path_dx){
  #   id <- conceptTbl$CODE[conceptTbl$source_value == i]
  #   clinicalData$condition_source_concept_id[clinicalData$path_dx == i] <- as.character(id)}
  # ########################################################################
  
  
  condition_occurrence <- data.frame(
    
    person_id = TargetGeneClinicalData$Person_id,
    
    condition_concept_id = 4112738, # Adenocarcinoma of lung
    
    condition_start_date = '1900-01-01',
    condition_start_datetime = '1900-01-01 00:00:00.0000000',
    condition_end_date = '1900-01-01',
    condition_end_datetime = '1900-01-01 00:00:00.0000000',
    
    condition_type_concept_id = "44786627", # "primary"
    stop_reason = NA,
    provider_id = NA,
    visit_occurrence_id = NA,
    visit_detail_id = NA,
    
    condition_source_value = 'Adenocarcinoma of lung',
    condition_source_concept_id = NA,
    
    condition_status_source_value = NA,
    condition_status_concept_id	= NA
  )
  
  # dim(condition_occurrence) # 1060, 15
  # View(condition_occurrence)
  
  print("Inserting data into condition_occurrence table")
  
  insertTable(connection, tableName = paste0(schema,".condition_occurrence"), 
              data = condition_occurrence,
              dropTableIfExists = FALSE,
              createTable = FALSE,
              tempTable=FALSE,
              oracleTempSchema = NULL)
  
  print("Inserted.") 
  
  ############################################ END 3. Inserting into "condition_occurrence" table
  ########################################################################################
  
  
  
  ########################################################################################
  ########################################################################################
  # 4. Inserting data into "procedure_occurrence" table 
  
  # ### procedure_occurrence
  # # doctor_order
  # clinicalData$doc_order_concept_id <- 0
  # for (i in 1:length(unique(clinicalData$doc_order))) { # i=1
  #   clinicalData$doc_order_concept_id[clinicalData$doc_order == unique(clinicalData$doc_order)[i]] <- i
  # }
  
  ########################################################################
  
  
  procedure_occurrence <- data.frame(
    
    person_id	= TargetGeneClinicalData$Person_id,
    procedure_concept_id	= 46257601,
    # Genomic Sequencing Procedures and Other Molecular Multianalyte Assays
    
    procedure_date	= '1900-01-01',
    procedure_datetime	= '1900-01-01 00:00:00.0000000',
    
    procedure_type_concept_id	= 38000275, # EHR order list entry
    modifier_concept_id	= NA,
    
    quantity	= 1,
    
    provider_id	= NA,
    
    visit_occurrence_id	= NA,
    visit_detail_id	= 0,
    
    procedure_source_value	= "Genomic Sequencing Procedures and Other Molecular Multianalyte Assays",
    procedure_source_concept_id	= 0,
    
    modifier_source_value	= NA
  )
  # dim(procedure_occurrence) # 1060, 13
  # View(procedure_occurrence)
  
  print("Inserting data into procedure_occurrence table")
  
  insertTable(connection, paste0(schema,".","procedure_occurrence"), procedure_occurrence,
              dropTableIfExists = FALSE,
              createTable = FALSE,
              tempTable = FALSE,
              oracleTempSchema = NULL)
  
  print("Inserted.")
  
  ############################################ END 4. Inserting into "procedure_occurrence" table
  ########################################################################################
  
  
  ########################################################################################
  ########################################################################################
  # 5. Inserting data into "genomic_test" table 
  
  print("Loading genomic_test data")
  
  genomic_test <- data.frame(
    
    care_site_id = 00001, # TCGA
    
    genomic_test_name = 'NGS panel level1',
    genomic_test_version = 'ver2018.10',
    
    reference_genome = "GRCh37 (hg19)",
    
    sequencing_device = "Illumina_MiSeq",
    library_preparation = 'SureSelectXT_ReagentKit_HSQ96',
    target_capture = 'Amplicon/Probe_capture',
    read_type = 'Paired-ends',
    read_length = 150,
    
    quality_control_tools = 'FastQC_v0.10.1/Cutadapt_v1.10/Picard_v1.98/Qualimap_v2.2.1',
    total_reads = NA,
    mean_target_coverage = NA,
    per_target_base_cover_100x = NA,
    
    alignment_tools = 'BWA_v0.7.15/Bedtools_v2.17/samtools_v1.3.1/GATK_v2.3.9Lite',
    variant_calling_tools = 'Vardict_2017.1.17/Lumpy_v0.2.13/CNVkit_vv0.8.4',
    chromosome_corrdinate = NA,
    annotation_tools = 'SnpEff_v4.3',
    annotation_databases = 'gnomAD_r.2.0.1/dbNSFP_2.9/ClinVar_20180225/COSMIC_v79/dbSNP_v149'
  )
  # dim(genomic_test) # 1, 18
  # View(genomic_test)
  
  print("Inserting data into genomic_test table")
  
  insertTable(connection, paste0(schema,".","genomic_test"), genomic_test,
              dropTableIfExists = FALSE,
              createTable = FALSE,
              tempTable = FALSE,
              oracleTempSchema = NULL)
  
  print("Inserted.")
  
  ############################################ END 5. Inserting into "genomic_test" table
  ########################################################################################
  
  
  
  ########################################################################################
  ########################################################################################
  # 6. Inserting data into "target_gene" table 
  
  print("Loading target_gene data")
  
  HGNC_Table <- read.xlsx(HGNC_file, sheetIndex = 1)
  # head(HGNC_Table)
  # HGNC_Table$ID <- 1:length(HGNC_Table$Approved_Symbol)
  # dim(HGNC_Table) # 41183, 3
  # head(HGNC_Table)
  
  ########################################################################
  
  ### target_gene 
  # Target Gene List
  Targeted_Genes <- c("AKT1", "ALK", "APC", "AR", "ATM", "ATR", "BRAF", "BRCA1", "BRCA2", "CD274", "CDH1", 
                      "CDK4", "CDK6", "CDKN2A", "EGFR", "ERBB2", "ERBB3", "ERBB4", "ESR1", "EWSR1", "FGFR1", 
                      "FGFR2", "FGFR3", "HRAS", "IDH1", "IDH2", "JAK2", "KIT", "KRAS", "MET", "MTOR", "MYC", "MYCN", 
                      "MYD88", "NOTCH1", "NRAS", "NTRK1", "PDGFRA", "PDGFRB", "PIK3CA", "PTEN", "RB1", "RET", "ROS1", 
                      "TERT", "TMPRSS2", "TOP1", "TOP2A", "TP53")
  # length(Targeted_Genes) # 49
  
  tbl <- data.frame('genomic_test_id' = 1, 'gene_symbol' = Targeted_Genes)
  # head(tbl)
  
  # Target Gene
  target_merge <- merge(tbl, HGNC_Table, by.x='gene_symbol', by.y='Approved_Symbol', all.x=TRUE)
  # head(target_merge)
  # dim(target_merge) # 49, 4
  
  ########################################################################
  
  target_gene <- data.frame(
    genomic_test_id = target_merge$genomic_test_id,
    hgnc_id = target_merge$HGNC_ID,
    hgnc_symbol = target_merge$gene_symbol
  )
  
  # dim(target_gene) # 49, 3
  # head(target_gene)
  
  print("Inserting data into target_gene table")
  
  insertTable(connection, paste0(schema,".","target_gene"), target_gene,
              dropTableIfExists = FALSE,
              createTable = FALSE,
              tempTable = FALSE,
              oracleTempSchema = NULL)
  
  print("Inserted.")
  
  ############################################ END 6. Inserting into "target_gene" table
  ########################################################################################
  
  
  
  
  
  ########################################################################################
  ########################################################################################
  # 7. Inserting data into "variant_occurrence" table 
  
  ### Load Variation Information
  varFiles = list.files(variantDir, pattern="variant.xlsx")
  # length(varFiles)  # 1060
  
  # variantFile = read.xlsx(variantFile, sheetIndex = 1)
  # # dim(variantFile) # 2904, 23
  
  #i=1
  variant_occurrence <- data.frame()
  for (i in 1:length(varFiles)) {
    
    pathology_id <- substr(varFiles[i], 1, nchar(varFiles[i])-13)
    
    variant_of_case_id <- TotalvariantFile[TotalvariantFile$case_id==pathology_id, ]
    variant_of_case_id_merged <- merge(x=variant_of_case_id, target_merge[,c('HGNC_ID', 'gene_symbol')], 
                                       by='gene_symbol', all.x=TRUE)
    
    
    variant_occurrence_sub <- data.frame(
      
      procedure_occurrence_id = TargetGeneClinicalData$Person_id[TargetGeneClinicalData$Sample.ID==pathology_id],
      specimen_id = TargetGeneClinicalData$Person_id[TargetGeneClinicalData$Sample.ID==pathology_id],
      
      reference_specimen_id = NA,
      
      target_gene1_id = variant_of_case_id_merged$HGNC_ID,
      target_gene1_symbol = variant_of_case_id_merged$gene_symbol,
      
      target_gene2_id = NA,
      target_gene2_symbol = NA,
      
      reference_sequence = NA,
      rs_id = NA,
      
      reference_allele = variant_of_case_id_merged$reference_allele,
      alternate_allele = variant_of_case_id_merged$variant_allele,
      
      hgvs_c = NA,
      hgvs_p = variant_of_case_id_merged$amino_acid_change,
      
      variant_read_depth = as.numeric(variant_of_case_id_merged$reference_read_count_tumor),
      #total_read_depth = as.numeric(variant_of_case_id_merged$variant_read_count_tumor),
      
      variant_exon_number = NA,
      
      copy_number = NA,
      cnv_locus = NA,
      
      fusion_breakpoint = NA,
      fusion_supporting_reads = NA,
      
      sequence_alteration = 'SNP',
      variant_feature	= variant_of_case_id_merged$mutation_type,
      
      genotype = tolower(variant_of_case_id_merged$mutation_status)
    )
    
    variant_occurrence <- rbind(variant_occurrence, variant_occurrence_sub)

    print(paste0(i,"/", length(varFiles), ": ", varFiles[i]))
    
  }
  #dim(variant_occurrence) # 2904, 22
  
    
  ############ Data Processing Before Insert #############
  
  # Variables:
  #foo <- c("ARGHISLEULEULYS","METHISARGARGMET")
  #foo <- c("Asp1184Thr", "Gly13Arg")
  
  HGVS_p <- as.character(variant_occurrence$hgvs_p)
  #length(HGVS_p) # 2904
  
  # Code maps:
  code1 <- c("A",   "R",   "N",   "D",   "B", 
             "C",   "E",   "Q",   "Z",   "G",   
             "H",   "I",   "L",   "K",   "M",   
             "F",   "P",   "S",   "T",   "W",   
             "Y",   "V",   "X")
  
  code3 <- c("Ala", "Arg", "Asn", "Asp", "Asx", 
             "Cys", "Glu", "Gln", "Glx", "Gly", 
             "His", "Ile", "Leu", "Lys", "Met", 
             "Phe", "Pro", "Ser", "Thr", "Trp", 
             "Tyr", "Val", "Xaa")
  
  # For each code replace 3letter code by 1letter code:
  for (i in 1:length(code3))  {
    HGVS_p <- gsub(code1[i], code3[i], HGVS_p, ignore.case=FALSE)
  }
  
  # length(three_letter) # 2904
  
  foo <- c("E256Q", "S310F")
  
  for (i in 1:length(code3))  {
    foo <- gsub(code1[i], code3[i], foo, ignore.case=FALSE)
  }
  
  wrong <- c("Glylu", "Glyln", "Prohe")
  right <- c("Glu",   "Gln",   "Phe")
  
  for (i in 1:length(wrong))  {
    foo <- gsub(wrong[i], right[i], foo, ignore.case=FALSE)
  }
  
  for (i in 1:length(wrong))  {
    HGVS_p <- gsub(wrong[i], right[i], HGVS_p, ignore.case=FALSE)
  }
  
  
  #head(variant_occurrence)
  
  for (i in 1:length(HGVS_p)){
    HGVS_p[i] <- paste0("p.", HGVS_p[i])
  }
  
  variant_occurrence$hgvs_p <- HGVS_p
  
  ########################################################################
  
  #unique(variant_occurrence$variant_feature)
  
  feature <- variant_occurrence$variant_feature
  
  from <- c("Missense_Mutation", "Nonsense_Mutation", "Splice_Site", "Splice_Region",
            "In_Frame_Ins", "In_Frame_Del", "Frame_Shift_Ins", "Frame_Shift_Del")
  to   <- c("Missense", "Nonsense", "Splice_site", "Splice_region", 
            "Inframe_Ins", "Inframe_Del", "Frameshift_Ins", "Frameshift_Del")
  
  for (i in 1:length(from))  {
    feature <- gsub(from[i], to[i], feature, ignore.case=FALSE)
  }
  
  variant_occurrence$variant_feature <- feature
  
  
  ########################################################################
    
    
    insertTable(connection, paste0(schema,".","variant_occurrence"), variant_occurrence,
                dropTableIfExists = FALSE,
                createTable = FALSE,
                tempTable = FALSE,
                oracleTempSchema = NULL)
    
    ############################################ END 7. Inserting into "variant_occurrence" table
    ########################################################################################
  
  
    
    ########################################################################################
    ########################################################################################
    # 8. Inserting data into "variant_annotation" table
    
    ## variant_occurrence_id ###########
    
    #View(variant_occurrence)
    
    #i=1
    last_n = 0
    variant_annotation <- data.frame()
    for (i in 1:length(varFiles)) {
      pathology_id <- substr(varFiles[i], 1, nchar(varFiles[i])-13)
      
      variant_of_case_id <- TotalvariantFile[TotalvariantFile$case_id==pathology_id, ]
      variant_of_case_id_merged <- merge(x=variant_of_case_id, target_merge[,c('HGNC_ID', 'gene_symbol')], 
                                         by='gene_symbol', all.x=TRUE)
      
      annoFields <- c('functional_impact_score') 
      #              'variant_function_impact', 'lof', 'SIFT', 'MutationTaster', 'PolyPhen2', 'clinvar_sig_id')
      #length(annoFields) # 7
      
      annoData <- variant_of_case_id_merged[, annoFields]
      annoData[is.na(annoData)] <- "."
      
      last_n <- dim(variant_annotation)[1]
      
      annoData_df = as.data.frame(cbind(variant_occurrence_id = (last_n + 1) : (last_n + length(annoData)), annoData))
      
      annoData_gather = gather(annoData_df, 'annotation_fields', 'value_as_string', -variant_occurrence_id)
      
      variant_annotation_sub <- data.frame(variant_occurrence_id = as.character(annoData_df$variant_occurrence_id),
                                       #annoData$variant_occurrence_id,
                                       annotation_field = 'functional_impact_score',
                                       value_as_string = NA,
                                       value_as_number = as.character(annoData_df$annoData))
      
      variant_annotation <- rbind(variant_annotation, variant_annotation_sub)
      
      print(paste0(i,"/", length(varFiles), ": ", varFiles[i]))
      
    }
    
    #dim(variant_annotation) # 2904
    
    # View(variant_annotation)
    
    
    insertTable(connection, paste0(schema,".","variant_annotation"), variant_annotation,
                dropTableIfExists = FALSE,
                createTable = FALSE,
                tempTable = FALSE,
                oracleTempSchema = NULL)
    
    
    ############################################ END 8. Inserting into "variant_annotation" table
    ########################################################################################
    
    
  print("ETL Completed!")  
  
} ## END for Function "insertXLSXIntoGCDM"


