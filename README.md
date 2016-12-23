# Erlang Embedded SQL library

## Next release

## v0.3.13

- Fixed NOT operator's precedence.

## v0.3.12

- Select for update: Creates a lock over the
  read object to safely perform an update

## v0.3.11

- Fixed bug in SQL IN operator

## v0.3.10

- Dialyzer configuration improved: unknown detected now.
- Enrich: Count distinct

## v0.3.9

- Offset type specification fixed. The index starts
  in 0.

## v0.3.8

- Fix precedence of operators

## v0.3.7

- Option group by in select
- Option count in select

## v0.3.6

- Cascade option in truncate

## v0.3.5

- `true` and `false` atoms allowed as predicates.

## v0.3.4

- Feature TRUNCATE table
- New test truncate

## v0.3.3

- Adding ON CONFLICT DO NOTHING if the given conflict columns are
  an empty list. This allows to have an insert that does not fail
  if there is a conflict.
- New test.

## v0.3.2

- Adding a light version of ON-CONFLICT-UPDATE new postgres feature to
  INSERT. This light version just specified the conflict target and
  the rest of the values are automaticaly udpated on conflict by using
  table EXCLUDED.
- New tests

## v0.3.1

- Refactor some folds applying to_sql to lists of clauses.
- OFFSET/FETCH clause added to SELECT (introduced in SQL in 2008).
- ORDER BY clause added to SELECT (<sort specification>).

## v0.3.0

- This library now uses placeholders.
- The input remains the same, but returns a tuple,
  with the statement and the parameters.
- New tests. Old tests adapted.

## v0.2.1

- New statements added: <start transaction statement>,
  <commit statement>, <rollback statement>.
- Refactoring: introducing non terminal symbols from SQL grammar.
- eesql:query_specification/0 renamed to eesql:query_spec/0

## v0.2.0

- Makefile pull/push targets added.
- Serialization of <joined table>.
- Initial Makefile.
- Directory test/ added to source dir (to allow dialyzer to dialyzes
  the tests).
- Using non terminal symbols at
  http://savage.net.au/SQL/sql-2003-2.bnf.html as reference.
- Usage examples moved to tests.
- TOC added to Changelog.
- First unit test added, extracted from examples in comments.

## v0.1.1

- sql module name reminiscence moved to eesql.

## v0.1.0

- Initial realease supporting the representation of serveral SQL AST nodes.
