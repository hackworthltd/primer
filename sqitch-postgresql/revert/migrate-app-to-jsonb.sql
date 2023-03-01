-- Revert primer:migrate-app-to-jsonb from pg

BEGIN;

SET client_min_messages = warning;
SET search_path TO primer;

-- Note: the migration introduced by this schema change (i.e., the
-- schema change that the ALTER below reverts) isn't strictly
-- invertible, because there's no way to recover the original JSON's
-- formatting. However, how the JSON is formatted is irrelevant to the
-- `App` type in our Haskell code, so the fact that we can't recover
-- the original programs' formatting upon reverting is no cause for
-- concern.
ALTER TABLE ONLY sessions
    ALTER COLUMN app TYPE bytea USING convert_to(jsonb_pretty(app), 'UTF8')::bytea;


COMMIT;
