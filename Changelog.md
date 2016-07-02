[TOC]

# eesql Changelog

## Next release

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
