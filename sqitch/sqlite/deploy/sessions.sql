-- Deploy primer:sessions to sqlite

BEGIN;

CREATE TABLE sessions (
    uuid         TEXT NOT NULL PRIMARY KEY,
    gitversion   TEXT NOT NULL,
    app          TEXT NOT NULL,
    name         TEXT NOT NULL,
    lastmodified DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);

COMMIT;
