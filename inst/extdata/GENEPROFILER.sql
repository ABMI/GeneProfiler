SET NOCOUNT ON
GO
IF ( OBJECT_ID('tempdb..#TEMP') IS NOT NULL )
    DROP TABLE #TEMP
GO
CREATE TABLE #TEMP ( Target_name varCHAR(30) NOT NULL,
					Input_institution_name float NULL)
GO				
INSERT #TEMP (Target_name,
			Input_institution_name)
			Values('EGFR_p.Leu858Arg', 0),
			('EGFR_p.Thr790Met',0),
			('EGFR_p.Leu861Gln',0),
			('EGFR_p.Gly719X',0),
			('EGFR_p.Ser768Ile',0),
			('KRAS_p.Gly12X',0),
			('KRAS_p.Gly13X',0),
			('KRAS_p.Gln61X',0),
			('PIK3CA_p.His1047X',0),
			('BRAF_p.Val600Glu',0),
			('NRAS_p.Gln61Lys',0)
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'EGFR' and A.hgvs_p = 'p.Leu858Arg')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'EGFR_p.Leu858Arg'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'EGFR' and A.hgvs_p = 'p.Thr790Met')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'EGFR_p.Thr790Met'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'EGFR' and A.hgvs_p = 'p.Leu861Gln')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'EGFR_p.Leu861Gln'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'EGFR' and A.hgvs_p like 'p.Gly719%')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'EGFR_p.Gly719X'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'EGFR' and A.hgvs_p = 'p.Ser768Ile')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'EGFR_p.Ser768Ile'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1  (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'KRAS' and A.hgvs_p like 'p.Gly12%')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'KRAS_p.Gly12X'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'KRAS' and A.hgvs_p like 'p.Gly13%')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'KRAS_p.Gly13X'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'KRAS' and A.hgvs_p like 'p.Gln61%')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'KRAS_p.Gln61X'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'PIK3CA' and A.hgvs_p like 'p.His1047%')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'PIK3CA_p.His1047X'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'BRAF' and A.hgvs_p = 'p.Val600Glu')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'BRAF_p.Val600Glu'
GO
UPDATE #TEMP SET Input_institution_name = (SELECT TOP 1 (count(A.person_id)) as input_institution_name
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
WHERE (B.target_gene_source_value = 'NRAS' and A.hgvs_p = 'p.Gln61Lys')
GROUP BY B.target_gene_source_value, A.hgvs_p WITH ROLLUP
ORDER BY B.target_gene_source_value+'_'+A.hgvs_p)
WHERE Target_name = 'NRAS_p.Gln61Lys'
GO
UPDATE #TEMP SET Input_institution_name = ROUND(Input_institution_name * 100 / CONVERT(float, (SELECT count(distinct(A.person_id))
FROM (SELECT * FROM dbo.variant_occurrence
WHERE person_id IN (SELECT subject_id FROM dbo.cohort)) A
LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id)),2)
GO
SELECT * FROM #TEMP