-- Verify primer:appschema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('primer', 'usage');

ROLLBACK;
