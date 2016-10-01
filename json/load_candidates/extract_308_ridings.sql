
--	Extract ridings from the candidate data.
--	Doesn't have area and population data, unfortunately.

WITH
        riding AS (
        SELECT DISTINCT ON (riding_id)
              riding_id        AS fptp_id
            , riding_id / 1000 AS prov_id
            , riding_name      AS name
            , pop
        FROM election_raw.poll_results
            JOIN election_raw.riding_pop USING (riding_id)
        ORDER BY riding_id
    ),
        prov AS (
        SELECT
            *
            , CASE
              WHEN prov_id = 10
                  THEN 'NL'
              WHEN prov_id = 11
                  THEN 'PE'
              WHEN prov_id = 12
                  THEN 'NS'
              WHEN prov_id = 13
                  THEN 'NB'
              WHEN prov_id = 24
                  THEN 'QC'
              WHEN prov_id = 35
                  THEN 'ON'
              WHEN prov_id = 46
                  THEN 'MB'
              WHEN prov_id = 47
                  THEN 'SK'
              WHEN prov_id = 48
                  THEN 'AB'
              WHEN prov_id = 59
                  THEN 'BC'
              WHEN prov_id = 60
                  THEN 'YT'
              WHEN prov_id = 61
                  THEN 'NT'
              WHEN prov_id = 62
                  THEN 'NU'
              END AS prov
        FROM riding
    )

SELECT json_agg(
    json_build_object(
        'fptp_id', fptp_id,
        'prov', prov,
        'name', name,
        'pop', pop,
        'area', 0,
        'dm', 1
    )
)
FROM prov