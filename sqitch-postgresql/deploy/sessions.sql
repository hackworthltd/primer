-- Deploy primer:sessions to pg
-- requires: appschema

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

CREATE TABLE sessions (
    uuid uuid NOT NULL,
    gitversion text NOT NULL,
    app bytea NOT NULL,
    name text NOT NULL
);

ALTER TABLE ONLY sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (uuid);

COMMIT;
