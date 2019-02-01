SELECT A.person_id, B.target_gene_source_value
                    FROM (SELECT * FROM @schema.dbo.variant_occurrence
                            WHERE person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table)) A
                            LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id
                            LEFT OUTER JOIN @schema.dbo.[variant_annotation] C ON A.variant_occurrence_id = C.variant_occurrence_id