-- Revert primer:sessions from pg

BEGIN;

SET client_min_messages = warning;

DROP TABLE primer.sessions;

COMMIT;
