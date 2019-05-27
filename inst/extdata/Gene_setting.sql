		SELECT B.target_gene1_symbol
        FROM (SELECT person_id, specimen_id FROM @schema.dbo.specimen
        WHERE person_id IN (SELECT distinct subject_id FROM @schema.dbo.@Cohort_table WHERE cohort_definition_id like @Cohort_definition_id)) A
              LEFT OUTER JOIN @schema.dbo.variant_occurrence B ON A.specimen_id = B.specimen_id