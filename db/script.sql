
-- ============================
-- 1) CREATION DES TABLES
-- ============================

-- Table AIRPORTS
DROP TABLE IF EXISTS airports CASCADE;
CREATE TABLE airports (
    faa   TEXT PRIMARY KEY,
    name  TEXT NOT NULL,
    lat   NUMERIC(9,6),
    lon   NUMERIC(9,6),
    alt   INTEGER,
    tz    NUMERIC,
    dst   TEXT,
    tzone TEXT,
    CONSTRAINT chk_airports_faa_format CHECK ( faa ~ '^[A-Z0-9]{3,4}$' )
);


-- Table AIRLINES
DROP TABLE IF EXISTS airlines CASCADE;
CREATE TABLE airlines (
    carrier TEXT PRIMARY KEY,
    name    TEXT NOT NULL
);


-- Table PLANES
DROP TABLE IF EXISTS planes CASCADE;
CREATE TABLE planes (
    tailnum TEXT PRIMARY KEY,
    year    INTEGER,
    type    TEXT,
    manufacturer TEXT,
    model   TEXT,
    engines INTEGER,
    seats   INTEGER,
    speed   INTEGER,
    engine  TEXT
);


-- Table WEATHER
DROP TABLE IF EXISTS weather CASCADE;
CREATE TABLE weather (
    origin   TEXT NOT NULL,
    year     INTEGER NOT NULL,
    month    INTEGER NOT NULL,
    day      INTEGER NOT NULL,
    hour     INTEGER NOT NULL,
    temp     NUMERIC,
    dewp     NUMERIC,
    humid    NUMERIC,
    wind_dir INTEGER,
    wind_speed NUMERIC,
    wind_gust NUMERIC,
    precip   NUMERIC,
    pressure NUMERIC,
    visib    NUMERIC,
    time_hour TIMESTAMP,
    PRIMARY KEY (year, month, day, hour, origin)
);


-- Table FLIGHTS
DROP TABLE IF EXISTS flights CASCADE;
CREATE TABLE flights (
    fid             BIGSERIAL PRIMARY KEY,
    year            INTEGER NOT NULL,
    month           INTEGER NOT NULL,
    day             INTEGER NOT NULL,
    dep_time        INTEGER,
    sched_dep_time  INTEGER,
    dep_delay       NUMERIC,
    arr_time        INTEGER,
    sched_arr_time  INTEGER,
    arr_delay       NUMERIC,
    hour            INTEGER,
    minute          INTEGER,
    carrier         TEXT NOT NULL,
    tailnum         TEXT,
    flight          INTEGER NOT NULL,
    origin          TEXT NOT NULL,
    dest            TEXT NOT NULL,
    air_time        NUMERIC,
    distance        NUMERIC,
    time_hour       TIMESTAMP,
    CONSTRAINT uq_flight_date_carrier UNIQUE (flight, year, month, day, hour, carrier)
);


-- ============================
-- 2) CONTRAINTES FK (NOT VALID pour injection sécurisée)
-- ============================

ALTER TABLE weather
  ADD CONSTRAINT fk_weather_origin FOREIGN KEY (origin) REFERENCES airports(faa) NOT VALID;

ALTER TABLE flights
  ADD CONSTRAINT fk_flights_origin  FOREIGN KEY (origin)  REFERENCES airports(faa) NOT VALID,
  ADD CONSTRAINT fk_flights_dest    FOREIGN KEY (dest)    REFERENCES airports(faa) NOT VALID,
  ADD CONSTRAINT fk_flights_carrier FOREIGN KEY (carrier) REFERENCES airlines(carrier) NOT VALID,
  ADD CONSTRAINT fk_flights_tailnum FOREIGN KEY (tailnum) REFERENCES planes(tailnum) NOT VALID;


-- ============================
-- 3) INDEXES
-- ============================

CREATE INDEX idx_flights_time_hour ON flights(time_hour);
CREATE INDEX idx_flights_origin    ON flights(origin);
CREATE INDEX idx_flights_dest      ON flights(dest);
CREATE INDEX idx_planes_manu       ON planes(manufacturer);



-- ============================
-- 4) VALIDATION DES CONTRAINTES APRES IMPORT
-- ============================
ALTER TABLE flights VALIDATE CONSTRAINT fk_flights_origin;
ALTER TABLE flights VALIDATE CONSTRAINT fk_flights_dest;
ALTER TABLE flights VALIDATE CONSTRAINT fk_flights_carrier;
ALTER TABLE flights VALIDATE CONSTRAINT fk_flights_tailnum;
ALTER TABLE weather VALIDATE CONSTRAINT fk_weather_origin;
