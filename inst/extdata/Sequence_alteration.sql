SELECT B.sequence_alteration
FROM (SELECT person_id, specimen_id FROM specimen
WHERE specimen_id IN (SELECT specimen_id FROM variant_occurrence) AND person_id IN (SELECT subject_id FROM @schema.dbo.@Cohort_table WHERE cohort_definition_id like @Cohort_definition_id)) A
LEFT OUTER JOIN @schema.dbo.variant_occurrence B ON A.specimen_id = B.specimen_id