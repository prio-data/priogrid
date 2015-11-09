DROP TABLE IF EXISTS dev.geoepr2priogrid;

CREATE TABLE dev.geoepr2priogrid AS
SELECT * FROM dev.geoepr;

COPY dev.geoepr2priogrid TO '/home/andreas/readwrite/Andreas/geoepr2priogrid.csv' CSV HEADER DELIMITER ',';