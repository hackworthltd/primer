-- Verify primer:sessions on sqlite

BEGIN;

SELECT uuid, gitversion, app, name, lastmodified
    FROM sessions
WHERE 0;

ROLLBACK;
