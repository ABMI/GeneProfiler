SELECT target_gene_source_value+'_'+hgvs_p as name,
		ROUND(COUNT(distinct(person_id))/CONVERT(float,(SELECT COUNT(person_id)
                                          FROM [@schema].[dbo].[condition_occurrence]
                                          WHERE person_id IN (SELECT [person_id]
                                          FROM [@schema].[dbo].[specimen]
                                          WHERE [anatomic_site_source_value] = 'Lung')
                                          AND [condition_source_value] IN ('Invasive adenocarcinoma', 'Minimally invasive adenocarcinoma', 'Squamous cell carcinoma'))),4)*100 as fraction
                                          FROM (SELECT person_id, B.target_gene_source_value, hgvs_p
                                          FROM [@schema].[dbo].[variant_occurrence] AS A
                                          LEFT OUTER JOIN dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                                          LEFT OUTER JOIN dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id
                                          WHERE ((target_gene_source_value = 'EGFR' AND hgvs_p = 'p.Leu858Arg') OR
                                          (target_gene_source_value = 'EGFR' AND hgvs_p = 'p.Thr790Met') OR
                                          (target_gene_source_value = 'EGFR' AND hgvs_p = 'p.Leu861Gln') OR
                                          (target_gene_source_value = 'EGFR' AND hgvs_p like 'p.Gly719%') OR
                                          (target_gene_source_value = 'EGFR' AND hgvs_p = 'p.Ser768Ile') OR
                                          (target_gene_source_value = 'KRAS' AND hgvs_p like 'p.Gly12%') OR
                                          (target_gene_source_value = 'KRAS' AND hgvs_p like 'p.Gly13%') OR
                                          (target_gene_source_value = 'KRAS' AND hgvs_p like 'p.Gln61%') OR
                                          (target_gene_source_value = 'PIK3CA' AND hgvs_p like 'p.His1047%') OR
                                          (target_gene_source_value = 'BRAF' AND hgvs_p = 'p.Val600Glu') OR
                                          (target_gene_source_value = 'NRAS' AND hgvs_p = 'p.Gln61Lys')) AND
                                          A.person_id IN (SELECT person_id
                                          FROM [@schema].[dbo].[condition_occurrence]
                                          WHERE person_id IN (SELECT [person_id]
                                          FROM [@schema].[dbo].[specimen]
                                          WHERE [anatomic_site_source_value] = 'Lung')
                                          AND [condition_source_value] IN ('Invasive adenocarcinoma', 'Minimally invasive adenocarcinoma', 'Squamous cell carcinoma'))) AS T
                                          GROUP BY target_gene_source_value+'_'+hgvs_p