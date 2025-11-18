SELECT * FROM OLA_4.bilbasen;

INSERT INTO seller (seller_cvr, seller_name, seller_address, location)
SELECT DISTINCT
    seller_cvr,
    seller_name,
    seller_address,
    location
FROM bilbasen
WHERE seller_cvr IS NOT NULL;

INSERT INTO car (carid, makemodel, link, seller_cvr)
SELECT DISTINCT
    CAST(carid AS SIGNED) AS carid,
    makemodel,
    link,
    seller_cvr
    FROM bilbasen
	WHERE seller_cvr IS NOT NULL;

INSERT INTO car_observation (carid, `Sys.time..`, price, details, properties, description)
SELECT
    CAST(carid AS SIGNED) AS carid,
    `Sys.time..`,
    price,
    details,
    properties,
    description
FROM bilbasen
WHERE seller_cvr IS NOT NULL;


INSERT INTO car_latest_observation (carid, `Sys.time..`)
SELECT o.carid, o.`Sys.time..`
FROM car_observation o
JOIN (
    SELECT carid, MAX(`Sys.time..`) AS max_time
    FROM car_observation
    GROUP BY carid
) m
  ON o.carid = m.carid
 AND o.`Sys.time..` = m.max_time;
