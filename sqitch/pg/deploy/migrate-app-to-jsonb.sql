-- Deploy primer:migrate-app-to-jsonb to pg
-- requires: sessions
-- requires: appschema

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

ALTER TABLE ONLY sessions
    ALTER COLUMN app TYPE jsonb USING convert_from(app, 'UTF8')::jsonb;

COMMIT;
