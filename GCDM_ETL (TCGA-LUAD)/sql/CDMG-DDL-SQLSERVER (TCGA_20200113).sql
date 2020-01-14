IF OBJECT_ID ('@schema.person', 'U') IS NULL 
CREATE TABLE @schema.person (	
	person_id						        INTEGER					NOT NULL	primary key,
	gender_concept_id					    INTEGER	  				NOT NULL ,
	
	year_of_birth						    INTEGER	  				NOT NULL ,
	month_of_birth						    INTEGER	  				NULL,
	day_of_birth					        INTEGER	  				NULL,
	birth_datetime						    DATETIME2				NULL,
	
	race_concept_id							INTEGER					NOT NULL,	--FK
	ethnicity_concept_id				    INTEGER	  				NOT NULL,	--FK
	location_id							    INTEGER					NULL,		--FK
	provider_id						        INTEGER					NULL,		--FK
	care_site_id					        INTEGER					NULL,		--FK
	
	person_source_value					NVARCHAR(50)					NULL,
	gender_source_value					NVARCHAR(50)				NULL,
	gender_source_concept_id				INTEGER					NULL,
	
	race_source_value						NVARCHAR(50)				NULL,
	race_source_concept_id					INTEGER					NULL,
	ethnicity_source_value					NVARCHAR(50)				NULL,
	ethnicity_source_concept_id				INTEGER					NULL
);


IF OBJECT_ID ('@schema.specimen', 'U') IS NULL 
CREATE TABLE @schema.specimen (
	specimen_id								INTEGER	identity(1,1)	NOT NULL	primary key,
	
	person_id							    INTEGER					NOT NULL,	--FK
	specimen_concept_id						INTEGER					NOT NULL,	--FK
	specimen_type_concept_id				INTEGER					NOT NULL,	--FK
	
	specimen_date						    DATE					NOT NULL,
	specimen_datetime						DATETIME2				NULL,
	quantity							    FLOAT					NULL,
	
	unit_concept_id							INTEGER					NULL,		--FK
	anatomic_site_concept_id				INTEGER					NULL,		--FK
	disease_status_concept_id				INTEGER					NULL,		--FK
	
	specimen_source_id						NVARCHAR(50)				NULL,	
	specimen_source_value					NVARCHAR(50)				NULL,
	unit_source_value						NVARCHAR(50)				NULL,
	anatomic_site_source_value				NVARCHAR(50)				NULL,
	disease_status_source_value				NVARCHAR(50)		    	NULL  
); 


IF OBJECT_ID ('@schema.condition_occurrence', 'U') IS NULL 
CREATE TABLE @schema.condition_occurrence (
    condition_occurrence_id					INTEGER	identity(1,1)	NOT NULL	primary key,
    person_id								INTEGER					NOT NULL ,	--FK
    condition_concept_id					INTEGER					NOT NULL ,  --FK
    
	condition_start_date					DATE					NOT NULL , 
    condition_start_datetime				DATETIME2				NULL,
	condition_end_date						DATE					NOT NULL, 
    condition_end_datetime					DATETIME2				NULL,

	condition_type_concept_id				INTEGER					NULL,  --FK
    stop_reason								VARCHAR(MAX)			NULL,  
    provider_id								INTEGER					NULL,  --FK
    visit_occurrence_id						INTEGER					NULL,  --FK
    visit_detail_id							INTEGER					NULL,  --FK

	condition_source_value					VARCHAR(MAX)			NULL,
	condition_source_concept_id				INTEGER					NULL,  --FK
	
	condition_status_source_value			VARCHAR(MAX)			NULL,
	condition_status_concept_id				INTEGER					NULL
);
 

IF OBJECT_ID ('@schema.procedure_occurrence', 'U') IS NULL 
CREATE TABLE @schema.procedure_occurrence (
	procedure_occurrence_id					INTEGER	identity(1,1)	NOT NULL	primary key,
	
	person_id						        INTEGER					NOT NULL,	--FK
	procedure_concept_id					INTEGER					NOT NULL,	--FK

	procedure_date							DATE					NOT NULL,
	procedure_datetime						DATETIME2				NULL,

	procedure_type_concept_id				INTEGER					NOT NULL,	--FK	
	modifier_concept_id						INTEGER					NULL,		--FK

	quantity						        INTEGER					NULL,
	
	provider_id								INTEGER					NULL,		--FK
	visit_occurrence_id						INTEGER					NULL,		--FK
	visit_detail_id							INTEGER					NULL,		--FK
		
	procedure_source_value					VARCHAR(MAX)			NULL,
	procedure_source_concept_id				INTEGER					NULL,		--FK
	modifier_source_value					VARCHAR(50)				NULL
);


IF OBJECT_ID ('@schema.genomic_test', 'U') IS NULL 
CREATE TABLE @schema.genomic_test (
	genomic_test_id						    INTEGER	identity(1,1)	NOT NULL	primary key,	
	care_site_id							INTEGER					NOT NULL,
	
	genomic_test_name						VARCHAR(MAX)			NULL,
	genomic_test_version					VARCHAR(MAX)			NULL,	

	reference_genome						VARCHAR(MAX)			NULL,

	sequencing_device						VARCHAR(MAX)			NULL,
	library_preparation						VARCHAR(MAX)			NULL,
	target_capture							VARCHAR(MAX)			NULL,
	read_type								VARCHAR(MAX)			NULL,
	read_length								INTEGER					NULL,

	quality_control_tools					VARCHAR(MAX)			NULL,
	total_reads								INTEGER					NULL,
	mean_target_coverage					FLOAT					NULL,
	per_target_base_cover_100x				FLOAT					NULL,

	alignment_tools							VARCHAR(MAX)			NULL,
	variant_calling_tools					VARCHAR(MAX)			NULL,
	chromosome_corrdinate					VARCHAR(MAX)			NULL,
	annotation_tools						VARCHAR(MAX)			NULL,
	annotation_databases					VARCHAR(MAX)			NULL
);



IF OBJECT_ID ('@schema.target_gene', 'U') IS NULL 
CREATE TABLE @schema.target_gene (
	target_gene_id							INTEGER	identity(1,1)	NOT NULL	primary key,	
	genomic_test_id				     	    INTEGER					NOT NULL,	
	
	hgnc_id									VARCHAR(MAX)			NOT NULL,
	hgnc_symbol								VARCHAR(MAX)			NOT NULL
);



IF OBJECT_ID ('@schema.variant_occurrence', 'U') IS NULL 
CREATE TABLE @schema.variant_occurrence (						
	variant_occurrence_id					INTEGER	identity(1,1)	NOT NULL	primary key,		
	
	procedure_occurrence_id					INTEGER					NOT NULL,	--FK
	
	specimen_id								INTEGER					NOT NULL,	--FK
	reference_specimen_id					INTEGER					NULL,    	--FK
	
	target_gene1_id							VARCHAR(MAX)			NULL,	    --FK
	target_gene1_symbol						VARCHAR(MAX)			NULL,		--FK
	target_gene2_id							VARCHAR(MAX)			NULL,	    --FK
	target_gene2_symbol						VARCHAR(MAX)			NULL,		--FK	

	reference_sequence						VARCHAR(50)				NULL,
	rs_id									VARCHAR(50)				NULL,
	
	reference_allele						VARCHAR(MAX)			NULL,
	alternate_allele						VARCHAR(MAX)			NULL,

	hgvs_c									VARCHAR(MAX)			NULL,
	hgvs_p									VARCHAR(MAX)			NULL,

	variant_read_depth						INTEGER					NULL,
--	total_read_depth						INTEGER					NULL,	
	variant_exon_number						INTEGER					NULL,

	copy_number								FLOAT					NULL,
	cnv_locus								VARCHAR(MAX)			NULL,

	fusion_breakpoint						VARCHAR(MAX)			NULL,
	fusion_supporting_reads					INTEGER					NULL,

	sequence_alteration						VARCHAR(MAX)			NULL,
	variant_feature							VARCHAR(MAX)			NULL,	
	
	genetic_origin							VARCHAR(MAX)			NULL,
	genotype								VARCHAR(MAX)			NULL	
);


IF OBJECT_ID ('@schema.variant_annotation', 'U') IS NULL 
CREATE TABLE @schema.variant_annotation (
	variant_annotation_id					INTEGER	identity(1,1)	NOT NULL	primary key,
	variant_occurrence_id					INTEGER					NOT NULL,

	annotation_field						VARCHAR(MAX)			NOT NULL,

	value_as_string							VARCHAR(MAX)			NULL,
	value_as_number							FLOAT					NULL
);


IF OBJECT_ID ('@schema.cohort', 'U') IS NULL 
CREATE TABLE @schema.cohort (
	cohort_definition_id					INTEGER					NOT NULL,
	subject_id								INTEGER					NOT NULL,

	cohort_start_date						date					NOT NULL,
	cohort_end_date							date					NOT NULL
);