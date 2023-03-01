-- Verify primer:migrate-app-to-jsonb on pg

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

-- Will fail if app isn't of type jsonb.
SELECT jsonb_pretty(app) FROM sessions LIMIT 1;

ROLLBACK;
