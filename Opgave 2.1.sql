-- Opret en database til projektet
CREATE DATABASE OLA_4;

USE OLA_4;

-- 1) Forhandler (invariant entitet)
CREATE TABLE seller (
    seller_cvr     VARCHAR(8)  PRIMARY KEY,    
    seller_name    VARCHAR(34) NOT NULL,
    seller_address VARCHAR(38),
    location       VARCHAR(35)
);

-- 2) Bil / annonce (invariant entitet)
CREATE TABLE car (
    carid      BIGINT       PRIMARY KEY,        
    makemodel  VARCHAR(34)  NOT NULL,
    link       VARCHAR(75)  NOT NULL,
    seller_cvr VARCHAR(8)   NOT NULL,
    CONSTRAINT fk_car_seller
        FOREIGN KEY (seller_cvr)
        REFERENCES seller(seller_cvr)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
);

-- 3) Observationer over tid (tidsserie)
CREATE TABLE car_observation (
    carid       BIGINT      NOT NULL,
    `Sys.time..` DATETIME(6) NOT NULL, 
    price       VARCHAR(22) NOT NULL,
    details     VARCHAR(47),
    properties  VARCHAR(255),   
    description TEXT,
    PRIMARY KEY (carid, `Sys.time..`),
    CONSTRAINT fk_observation_car
        FOREIGN KEY (carid)
        REFERENCES car(carid)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

-- 4) Seneste observation per bil (link til sidst forekommende observation)
CREATE TABLE car_latest_observation (
    carid       BIGINT      NOT NULL,
    `Sys.time..` DATETIME(6) NOT NULL,
    PRIMARY KEY (carid),
    CONSTRAINT fk_latest_car
        FOREIGN KEY (carid)
        REFERENCES car(carid)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT fk_latest_observation
        FOREIGN KEY (carid, `Sys.time..`)
        REFERENCES car_observation(carid, `Sys.time..`)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
);
