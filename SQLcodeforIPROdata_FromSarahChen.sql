SELECT 
       CASE 
              WHEN CatgID=1 AND NumID=1 THEN 'HEDIS-7 Day Follow-Up'
              WHEN CatgID=1 AND NumID=2 THEN 'HEDIS-30 Day Follow-Up'
              WHEN CatgID=2 AND NumID=1 THEN 'PA-7 Day Follow-Up'
              WHEN CatgID=2 AND NumID=2 THEN 'PA-30 Day Follow-Up'
              WHEN CatgID=3 AND NumID=3 THEN 'Readmission within 30 Days'
       END AS Measure_Name,
       Provider_Master_BK_DC,
       Provider_Master_Label_DC,

       Frequency= CASE 
       WHEN FrequencyID=2 THEN 'Monthly'
       WHEN FrequencyID=4 THEN 'Semi-Annual'
       WHEN FrequencyID=5 THEN 'ANNUAL'
       END,

       RptDateBeg,
       RptDateEnd,
       AgeCat,
       Num=SUM(RptNum),
       Denom=SUM(RptDenom),
       Rate=SUM(RptNum)*1.0/SUM(RptDenom)
FROM SSRS_SP.dbo.IPRO_Rate_Provider


GROUP BY  
 CASE 
              WHEN CatgID=1 AND NumID=1 THEN 'HEDIS-7 Day Follow-Up'
              WHEN CatgID=1 AND NumID=2 THEN 'HEDIS-30 Day Follow-Up'
              WHEN CatgID=2 AND NumID=1 THEN 'PA-7 Day Follow-Up'
              WHEN CatgID=2 AND NumID=2 THEN 'PA-30 Day Follow-Up'
              WHEN CatgID=3 AND NumID=3 THEN 'Readmission within 30 Days'
       END ,
       Provider_Master_BK_DC,
       Provider_Master_Label_DC,

       CASE 
       WHEN   FrequencyID=2 THEN 'Monthly'
       WHEN FrequencyID=4 THEN 'Semi-Annual'
       WHEN FrequencyID=5 THEN 'ANNUAL'
       END,
                   FrequencyID,
       RptDateBeg,
       RptDateEnd,
       AgeCat
ORDER BY 
CASE 
       WHEN CatgID=1 AND NumID=1 THEN 'HEDIS-7 Day Follow-Up'
       WHEN CatgID=1 AND NumID=2 THEN 'HEDIS-30 Day Follow-Up'
       WHEN CatgID=2 AND NumID=1 THEN 'PA-7 Day Follow-Up'
       WHEN CatgID=2 AND NumID=2 THEN 'PA-30 Day Follow-Up'
       WHEN CatgID=3 AND NumID=3 THEN 'Readmission within 30 Days'
END 
,      FrequencyID,
       RptDateBeg,
       RptDateEnd,
       AgeCat
