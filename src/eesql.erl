%% @author Angel Herranz <aherranz@gmail.com>
%%
%% @copyright 2016 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc SQL Abstract Syntax Tree.
%%
%% Reference material:
%% - http://savage.net.au/SQL/ (http://savage.net.au/SQL/sql-2003-2.bnf.html)
%% - Module epgsql.erl
%% - http://www.postgresql.org/docs/current/static/sql.html
%% - http://ns.inria.fr/ast/sql/index.html
-module(eesql).

-include("include/eesql.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Non terminal symbols from http://savage.net.au/SQL/sql-2003-2.bnf.html
-export_type(
   [sql_stmt/0,
    query_spec/0,
    from_clause/0,
    join_type/0,
    table_ref/0]
).

%% TODO: convert to non terminal symbols from http://savage.net.au/SQL/sql-2003-2.bnf.html
-export_type(
   [insert_stmt/0, update_stmt/0, delete_stmt/0,
    dup/0,
    name/0, column/0,
    predicate/0, join_cond/0, group_by_expr/0, order_by_expr/0,
    value/0,
    expr/0,
    binop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([to_sql/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The following types represents the PG SQL Abstract Syntax Tree.

%% Key SQL statements and fragments
-type sql_stmt() ::
        query_spec()
      | insert_stmt()
      | update_stmt()
      | delete_stmt().

%% Any SQL value in the rows (inspired by epgsql:bind_param())
-type value() ::
        null
      | boolean()
      %% | string() %% Let's avoid confussion with arrays
      | binary()
      | integer()
      | float()
      %% | calendar:date()
      %% | calendar:time() %actualy, `Seconds' may be float()
      %% | calendar:datetime()
      %% | {calendar:time(), Days::non_neg_integer(), Months::non_neg_integer()}
      | [value()]. %array (maybe nested)

%% Any name (column name, table name, alias, ...)
-type name() :: atom().

%% Expressions for describing columns (eg. in a SELECT statement)
-type column() :: name()
                | {name(), name()}. %% AS

%% Expressions for describing "tables" (eg. FROM in a SELECT statement)
-type table_ref() :: table_primary()
                   | joined_table().

-type table_primary() :: name()
                       | {name(), name()}. %% AS

-type joined_table() :: qualified_join().

-type qualified_join() :: #join{}.

-type join_type() :: inner | left | right | full.

-type join_condition() :: join_condition().

%% ALL and DISTINCT
-type dup() :: all | distinct.

%% Join condition (not contemplated for the moment).
-type join_cond() :: undefined.

%% Predicates
-type predicate() ::
        {'not', predicate()}
      | {'and', [predicate()]}
      | {'or', [predicate()]}
      | {expr(), binop(), expr()}
      | {column(), like, binary()}
      | {is_null, column()}
      | {exists, query_spec()}
      | {between, expr(), expr(), expr()}
      | {in, expr(), query_spec()}.
      %% | some, all, ...

%% Expressions
-type expr() :: column() | value().

%% Binary operators
-type binop() :: '=' | '!=' | '<' | '>' | '<=' | '>=' | like.

%% Group by expression (not contemplated for the moment).
-type group_by_expr() :: undefined.

%% Order by expression (not contemplated for the moment).
-type order_by_expr() :: undefined.

%% SELECT <query specification>
-type query_spec() :: #select{}.

-type from_clause() :: nonempty_list(table_ref()).

%% A insert statement
-type insert_stmt() :: #insert{}.

%% A update statement
-type update_stmt() :: #update{}.

%% A delete statement
-type delete_stmt() :: #delete{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc X = [1,2,3], [1, x, 2, x, 3] = intersperse(X, x)
-spec intersperse(list(),list()) -> list().
intersperse([], _) -> [];
intersperse([X | Xs], I) ->
  [X | lists:foldr(fun(Y, Acc) -> [I, Y | Acc] end,
                   [],
                   Xs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serializes an SQL statement.
-spec to_sql(sql_stmt()) -> Equery :: iodata().
to_sql(#select{dup = Duplicate,
               columns = Columns,
               from = From,
               where = Where}) ->
  Dup = dup_to_sql(Duplicate),
  case Columns of
    [] ->
      Items = "*";
    _ ->
      Items = intersperse([col_to_sql(Column) || Column <- Columns], ", ")
  end,
  From_Clause = ["FROM ", intersperse([table_ref_to_sql(Table_Ref) || Table_Ref <- From], ", ")],
  Where_Clause =
    case Where of
      [] -> "";
      [Predicate] -> ["WHERE ", pred_to_sql(Predicate)]
    end,
  Equery = intersperse(
             ["SELECT", Dup, Items, From_Clause, Where_Clause],
             " "
            ),
  [Equery, ";"];
to_sql(#insert{table = Table, columns = Columns, values = Rows}) ->
  ["INSERT INTO ", table_to_sql(Table),
   " (",
   intersperse([col_to_sql(Column) || Column <- Columns], ", "),
   ")",
   " VALUES ", intersperse([["(",
                             intersperse([expr_to_sql(Expr)
                                          || Expr <- Row],
                                         ", "),
                             ")"]
                            || Row <- Rows],
                           ", "),
   " RETURNING *;"];
to_sql(#update{table = Table,
               set = Set,
               where = Where}) ->
  Where_Clause =
    case Where of
      [] -> "";
      [Predicate] -> [" WHERE ", pred_to_sql(Predicate)]
    end,
  ["UPDATE ", table_to_sql(Table),
   " SET ",
   intersperse([[col_to_sql(Column), " = ", expr_to_sql(Expr)]
                || {Column, Expr} <- Set],
               ", "),
   Where_Clause,
   " RETURNING *;"];
to_sql(#delete{from = Table,
               where = Where}) ->
  Where_Clause =
    case Where of
      [] -> "";
      [Predicate] -> [" WHERE ", pred_to_sql(Predicate)]
    end,
  ["DELETE FROM ", table_to_sql(Table),
   Where_Clause,
   " RETURNING *;"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a <table reference>
-spec table_ref_to_sql(table_ref()) -> iodata().
table_ref_to_sql(#join{type = Type,
                       left = Left,
                       right = Right,
                       spec = Spec}) ->
  [table_ref_to_sql(Left),$ ,
   case Type of
     inner -> "INNER";
     left -> "LEFT OUTER";
     right -> "RIGHT OUTER";
     full -> "FULL OUTER"
   end, $ ,
   "JOIN",$ ,
   table_ref_to_sql(Right),$ ,
   "ON",$ ,
   pred_to_sql(Spec)];
table_ref_to_sql({Table_Name, Correlation_Name}) ->
  [atom_to_binary(Table_Name, utf8),$ ,
   "AS",$ ,
   atom_to_binary(Correlation_Name, utf8)];
table_ref_to_sql(Table_Name) ->
  atom_to_binary(Table_Name, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a name
-spec name_to_sql(name()) -> iodata().
name_to_sql(Name) ->
  atom_to_binary(Name, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize all and distinct
-spec dup_to_sql(dup()) -> iodata().
dup_to_sql(all) -> "ALL";
dup_to_sql(distinct) -> "DISTINCT".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a column description
-spec col_to_sql(column()) -> iodata().
col_to_sql(Column) when is_atom(Column) ->
  name_to_sql(Column);
col_to_sql({Column, Alias}) ->
  [name_to_sql(Column), " AS ", name_to_sql(Alias)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a data source description
-spec table_to_sql(column()) -> iodata().
table_to_sql(Table) when is_atom(Table) ->
  name_to_sql(Table);
table_to_sql({Table, Alias}) ->
  [name_to_sql(Table), " AS ", name_to_sql(Alias)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a predicate to SQL.
-spec pred_to_sql(Predicate :: predicate()) -> iodata().
pred_to_sql({'not', Predicate}) ->
  ["NOT ", pred_to_sql(Predicate)];
pred_to_sql({'and', Predicates}) ->
  intersperse([pred_to_sql(Predicate) || Predicate <- Predicates],
              " AND ");
pred_to_sql({'or', Predicates}) ->
  intersperse([pred_to_sql(Predicate) || Predicate <- Predicates],
              " OR ");
pred_to_sql({is_null, Column}) ->
  [name_to_sql(Column), " IS NULL"];
pred_to_sql({Left, Bin_Op, Right}) when Bin_Op == '=';
                                        Bin_Op == '!=';
                                        Bin_Op == '<>';
                                        Bin_Op == '<';
                                        Bin_Op == '>';
                                        Bin_Op == '<=';
                                        Bin_Op == '>=' ->
  [expr_to_sql(Left), " ",
   atom_to_binary(Bin_Op, utf8),
   " ", expr_to_sql(Right)];
pred_to_sql({Column, like, Match_String}) ->
  [name_to_sql(Column), " LIKE ", "'", Match_String, "'"];
pred_to_sql({exists, Select = #select{}}) ->
  ["EXISTS ", "(", to_sql(Select), ")"];
pred_to_sql({between, Expr, Min, Max}) ->
  [expr_to_sql(Expr),
   " BETWEEN ",
   expr_to_sql(Min), " AND ", expr_to_sql(Max)];
pred_to_sql({in, Expr, Select = #select{}}) ->
  [expr_to_sql(Expr), " IN ", to_sql(Select)].

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a expression to SQL.
-spec expr_to_sql(Expr :: expr()) -> iodata().
expr_to_sql(null) ->
  <<"NULL">>;
expr_to_sql(true) ->
  <<"TRUE">>;
expr_to_sql(false) ->
  <<"FALSE">>;
expr_to_sql(Column) when is_atom(Column) ->
  name_to_sql(Column);
expr_to_sql(Binary) when is_binary(Binary) ->
  ["'", Binary, "'"];
expr_to_sql(Integer) when is_integer(Integer) ->
  integer_to_binary(Integer);
expr_to_sql(Float) when is_float(Float) ->
  float_to_binary(Float);
expr_to_sql(Values) when is_list(Values) ->
  ["{",
   intersperse([expr_to_sql(Value) || Value <- Values],
               ", "),
   "}"].
