CREATE TRIGGER clean_match_prediction AFTER INSERT ON DE_match_prediction 
BEGIN 
DELETE
FROM
 DE_match_prediction
WHERE
  'Date' IS NULL
 OR HomeTeam IS NULL
 OR AwayTeam IS NULL
 OR PredictedOutcome IS NULL
 OR PredictedOutcome = 'NA'
; END