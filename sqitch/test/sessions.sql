-- Test sessions table.
SET client_min_messages TO warning;
CREATE EXTENSION IF NOT EXISTS pgtap;
RESET client_min_messages;

BEGIN;
SELECT plan(27);

SET search_path TO primer,public;

SELECT has_table('sessions');
SELECT has_pk(   'sessions' );
SELECT hasnt_fk( 'sessions' );

SELECT has_column(        'sessions', 'uuid' );
SELECT col_type_is(       'sessions', 'uuid', 'uuid' );
SELECT col_not_null(      'sessions', 'uuid' );
SELECT col_hasnt_default( 'sessions', 'uuid' );
SELECT col_is_pk(         'sessions', 'uuid' );
SELECT col_isnt_fk(       'sessions', 'uuid' );

SELECT has_column(        'sessions', 'gitversion' );
SELECT col_type_is(       'sessions', 'gitversion', 'text' );
SELECT col_not_null(      'sessions', 'gitversion' );
SELECT col_hasnt_default( 'sessions', 'gitversion' );
SELECT col_isnt_pk(       'sessions', 'gitversion' );
SELECT col_isnt_fk(       'sessions', 'gitversion' );

SELECT has_column(        'sessions', 'app' );
SELECT col_type_is(       'sessions', 'app', 'jsonb' );
SELECT col_not_null(      'sessions', 'app' );
SELECT col_hasnt_default( 'sessions', 'app' );
SELECT col_isnt_pk(       'sessions', 'app' );
SELECT col_isnt_fk(       'sessions', 'app' );

SELECT has_column(        'sessions', 'name' );
SELECT col_type_is(       'sessions', 'name', 'text' );
SELECT col_not_null(      'sessions', 'name' );
SELECT col_hasnt_default( 'sessions', 'name' );
SELECT col_isnt_pk(       'sessions', 'name' );
SELECT col_isnt_fk(       'sessions', 'name' );

SELECT finish();
ROLLBACK;
