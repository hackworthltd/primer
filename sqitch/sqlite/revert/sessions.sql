-- Revert primer:sessions from sqlite

BEGIN;

DROP TABLE sessions;

COMMIT;
