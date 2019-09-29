 use flightdata;
SELECT 
    OP_CARRIER, AIRLINE, DEP_DEL15, COUNT(*) AS NUM_FLIGHT
FROM
    flights
        INNER JOIN
    airline ON flights.OP_CARRIER = airline.IATA_CODE
WHERE
    DEP_DEL15 = 1
GROUP BY OP_CARRIER
ORDER BY NUM_FLIGHT DESC;

SELECT 
    COUNT(*)
FROM
    flights;

-- Performing some CTEs with the flight dataset    
WITH flight_data AS(
	SELECT DAY_OF_MONTH, MONTH, OP_CARRIER,TAIL_NUM, ORIGIN, AIRPORT, DEST, DEP_DELAY, ARR_DELAY, CANCELLED, DISTANCE
    FROM
		flights
		INNER JOIN
        airports ON flights.ORIGIN = airports.IATA_CODE
WHERE ORIGIN BETWEEN 'JFK' AND 'LAX' AND MONTH = 1 AND CANCELLED = 1
LIMIT  100
)
SELECT 
	DAY_OF_MONTH, OP_CARRIER, AIRLINE, ORIGIN, AIRPORT, DEST, DEP_DELAY, ARR_DELAY, CANCELLED, DISTANCE
FROM
	airline
    INNER JOIN
    flight_data ON airline.IATA_CODE = flight_data.OP_CARRIER;
    
-- Exporting the all outbound flight from California 
(SELECT 
  'YEAR',
    'MONTH',
    'DAY_OF_MONTH',
    'OP_CARRIER',
    'TAIL_NUM',
    'ORIGIN',
    'DEST',
    'DEP_TIME',
    'DEP_DELAY',
    'DEP_DEL15',
    'ARR_TIME',
    'ARR_DELAY',
    'ARR_DEL15',
    'FLIGHTS',
    'DISTANCE')
UNION
SELECT 
    YEAR,
    MONTH,
    DAY_OF_MONTH,
    OP_CARRIER,
    TAIL_NUM,
    ORIGIN,
    DEST,
    DEP_TIME,
    DEP_DELAY,
    DEP_DEL15,
    ARR_TIME,
    ARR_DELAY,
    ARR_DEL15,
    FLIGHTS,
    DISTANCE
FROM
    flights
WHERE 
ORIGIN IN ('LAX','SFO','SAN','OAK', 'SJC', 'SNA', 'TIJ','SMF','ONT', 'BUR', 'FAT','SBA')
INTO OUTFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/caflight15.csv' 
FIELDS ENCLOSED BY '"' 
TERMINATED BY ',' 
ESCAPED BY '"' 
LINES TERMINATED BY '\r\n';