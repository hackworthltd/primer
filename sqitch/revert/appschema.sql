-- Revert primer:appschema from pg

BEGIN;

DROP SCHEMA primer;

COMMIT;
