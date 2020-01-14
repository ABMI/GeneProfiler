#################################### 
# Divide Mutation by each casd_id  #
# 2018-05-18 SSJ                   #
####################################

View(clinicalData)
clinicalDatafordivide <- clinicalData
dim(clinicalDatafordivide)  # 2904, 23

uniqList <- unique(clinicalDatafordivide$case_id)

for (i in uniqList) {
    df <- clinicalDatafordivide[which(clinicalDatafordivide$case_id == i),  ]
    dir <- paste0('C:/Users/ssj/Desktop/4. OHDSI/1. OHDSI Genomic WG/3. TCGA_ETL/TCGA_ETL_20180529/data/variant/', 
                  i, '_variant.xlsx')
    write.xlsx(df, dir)    
}
