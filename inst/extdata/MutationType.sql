SELECT A.person_id, B.target_gene_source_value, A.hgvs_p, A.sequence_alteration, A.variant_feature
              FROM (SELECT * FROM @schema.dbo.variant_occurrence
        WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table)) A
        LEFT OUTER JOIN @schema.dbo.[target_gene] B ON A.target_gene_id = B.target_gene_concept_id