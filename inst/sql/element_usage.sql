CREATE TEMPORARY TABLE _tmp_period_of_interest AS (
SELECT rb.company_name, 
rb.cik, 
rb.sic,
rb.form_type, 
rb.filing_date,
rb.taxonomy,
CASE
     WHEN rb.creation_software LIKE '%XBRL Document Created with Wdesk from Workiva%'::TEXT THEN 'Workiva'::TEXT
     ELSE rb.creation_software
END AS creation_software,
rb.accession_number,
rb.authority_html_url AS sec_url,
rb.filer_status,
rb.document_period_end_date AS period_end,
rb.report_id
 FROM _mv_research_base_2014_ongoing rb
 --2016
WHERE 
(filing_date >= '01-01-2016' AND filing_date < '01-01-2017') AND rb.form_type NOT IN ('485BPOS', '485APOS', '497'));

 -- CREATING A LIST OF THE REPORTS THAT HAVE THESE LINE ITEMS
 
CREATE TEMPORARY TABLE _tmp_aspect_counts
AS
(SELECT  dp.report_id, a.name AS element_name
FROM data_point dp
JOIN aspect a ON a.aspect_id=dp.aspect_id
JOIN _tmp_period_of_interest rb ON rb.report_id=dp.report_id);

 -- CREATING A LIST OF THE REPORTS THAT HAVE THESE AXES

CREATE TEMPORARY TABLE _tmp_axes_counts 
AS
(SELECT avss.report_id, a.name AS element_name
FROM aspect a
JOIN aspect_value_selection avs ON avs.aspect_id=a.aspect_id
JOIN aspect_value_selection_set avss ON avs.aspect_value_selection_id=avss.aspect_value_selection_id
JOIN _tmp_period_of_interest rb ON rb.report_id=avss.report_id);

 -- CREATING A LIST OF THE REPORTS THAT HAVE THESE MEMBERS
CREATE TEMPORARY TABLE _tmp_member_counts 
AS
(SELECT avss.report_id, a.name AS element_name
FROM aspect a
JOIN aspect_value_selection avs ON avs.aspect_value_id=a.aspect_id
JOIN aspect_value_selection_set avss ON avs.aspect_value_selection_id=avss.aspect_value_selection_id
JOIN _tmp_period_of_interest rb ON rb.report_id=avss.report_id);


CREATE TEMPORARY TABLE _tmp_all AS
(
SELECT * FROM  _tmp_aspect_counts
UNION
SELECT * FROM  _tmp_axes_counts
UNION
SELECT * FROM  _tmp_member_counts
);

SELECT element_name, count(DISTINCT report_id)
FROM _tmp_all
GROUP BY element_name
ORDER BY count DESC;