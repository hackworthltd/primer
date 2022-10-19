-- Verify primer:session-timestamp on pg

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

SELECT uuid, gitversion, app, name, lastmodified
  FROM sessions
WHERE FALSE;

ROLLBACK;
