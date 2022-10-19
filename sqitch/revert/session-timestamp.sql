-- Revert primer:session-timestamp from pg

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

ALTER TABLE ONLY sessions
    DROP COLUMN lastmodified;

COMMIT;
