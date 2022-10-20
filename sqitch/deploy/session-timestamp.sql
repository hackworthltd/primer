-- Deploy primer:session-timestamp to pg
-- requires: sessions

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

ALTER TABLE ONLY sessions
    ADD COLUMN lastmodified timestamp with time zone NOT NULL DEFAULT now();

COMMIT;
