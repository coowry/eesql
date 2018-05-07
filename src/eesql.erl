%% @author Angel Herranz <angel.herranz@coowry.com>
%%         Manuel Cherep <manuel.cherep@coowry.com>
%%
%% @copyright 2016 Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file for detailed information.
%%
%% @doc SQL Abstract Syntax Tree.
%%
%% Reference material:
%% - USING: https://ronsavage.github.io/SQL/ (https://ronsavage.github.io/SQL/sql-2003-2.bnf.html)
%% - As a reference: Module epgsql.erl
%% - As a reference: http://www.postgresql.org/docs/current/static/sql.html
%% - As a reference: https://docs.jboss.org/author/display/teiid81final/BNF+for+SQL+Grammar
%% - As a reference: http://ns.inria.fr/ast/sql/index.html
%% - To be reviewed: https://jakewheat.github.io/sql-overview/sql-2011-foundation-grammar.html
-module(eesql).

-include("include/eesql.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Non terminal symbols from https://ronsavage.github.io/SQL/sql-2003-2.bnf.html
-export_type(
   [
    commit_stmt/0,
    column_name/0,
    column_reference/0,
    delete_stmt/0,
    derived_column/0,
    from_clause/0,
    insert_stmt/0,
    join_type/0,
    literal/0,
    query_spec/0,
    rollback_stmt/0,
    row_value_expr/0,
    set_clause/0,
    set_quant/0,
    sql_stmt/0,
    start_trans_stmt/0,
    sort_spec/0,
    table_name/0,
    table_ref/0,
    joined_table/0,
    table_primary/0,
    
    update_stmt/0,
    value_expr/0
   ]
).

-export_type(
   [
    pg_with_as/0,
    pg_call/0
   ]
).

%% TODO: convert to non terminal symbols from https://ronsavage.github.io/SQL/sql-2003-2.bnf.html
-export_type(
   [id/0,
    identifier_chain/0,
    predicate/0,
    binop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([to_sql/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([identifier_to_sql/1,
         identifier_chain_to_sql/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precompiled regular expressions for checking proper identifiers and
%% identifier chains

%% io:format("~w~n", [re:compile("^(\"[a-zA-Z0-9_]+\"|^[a-zA-Z0-9_]+)$")]).
-define(IDENTIFIER_MP, {re_pattern,1,0,0,<<69,82,67,80,157,0,0,0,16,0,0,0,1,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,0,0,1,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,0,89,25,127,0,43,0,1,29,34,106,0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,29,34,113,0,38,25,106,0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,114,0,81,27,114,0,89,0>>}).

%% io:format("~w~n", [re:compile("^(\"[a-zA-Z0-9_]+\"|[a-zA-Z0-9_]+)(\.(\"[a-zA-Z0-9_]+\"|[a-zA-Z0-9_]+))*(\.\*)?$")]).
-define(IDENTIFIER_CHAIN_MP, {re_pattern,4,0,0,<<69,82,67,80,4,1,0,0,16,0,0,0,1,0,0,0,255,255,255,255,255,255,255,255,0,0,0,0,0,0,4,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,0,192,25,127,0,43,0,1,29,34,106,0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,29,34,113,0,37,106,0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,114,0,80,140,127,0,89,0,2,12,127,0,43,0,3,29,34,106,0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,29,34,113,0,37,106,0,0,0,0,0,0,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,114,0,80,115,0,89,140,127,0,7,0,4,85,12,114,0,7,27,114,0,192,0>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Guards to pre validate that an atom could be an identifier
-define(IS_IDENTIFIER(X),
        is_atom(X),
        X /= null,
        X /= true,
        X /= false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The following types represents the PG SQL Abstract Syntax Tree.

%% Key SQL statements and fragments
-type sql_stmt() ::
        commit_stmt()
      | start_trans_stmt()
      | rollback_stmt()
      | query_spec()
      | insert_stmt()
      | update_stmt()
      | delete_stmt()
      | truncate_stmt()
      | pg_refresh_stmt()
      | union_stmt()
      | pg_with_as().

%% <identfier> ::= <actual identifier>
%% <actual identifier> ::= <regular identifier> | <delimited identifier>
%% <regular identifier> ::= <identifier body>
%% <identifier body> ::= <identifier start> [ <identifier part> ... ]
%% <identifier part> ::= <identifier start> | <identifier extend>
%% <delimited identifier> ::= <double quote> <delimited identifier body> <double quote>
%% <delimited identifier body> ::= <delimited identifier part> ...
%% <delimited identifier part> ::= <nondoublequote character> | <doublequote symbol>
-type id() :: atom().

%% <identifier chain> ::= <identifier> [ { <period> <identifier> }... ]
-type identifier_chain() :: atom().

%% <literal> (any SQL literal, for the moment just inspired by epgsql:bind_param())
%% <literal> ::= <signed numeric literal> | <general literal>
%% <unsigned literal> ::= <unsigned numeric literal> | <general literal>
%% <general literal> ::= 
%%   <character string literal>
%% | <national character string literal>
%% | <Unicode character string literal>
%% | <binary string literal>
%% | <datetime literal>
%% | <interval literal>
%% | <boolean literal>
-type literal() ::
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
      .

%% <table name> ::= <local or schema qualified name>
%% <local or schema qualified name> ::= [ <local or schema qualifier> <period> ] <qualified identifier>
%% <local or schema qualifier> ::= <schema name> | MODULE
%% <schema name> ::= [ <catalog name> <period> ] <unqualified schema name>
%% <unqualified schema name> ::= ???
%% <catalog name> ::= <identifier>
-type table_name() :: identifier_chain().

%% <column reference> ::=
%%   <basic identifier chain>
%% | MODULE <period> <qualified identifier> <period> <column name>
%% <basic identifier chain> ::= <identifier chain>
%% <qualified identifier> ::= <identifier>
-type column_reference() :: identifier_chain().

%% <column name> ::= <identifier>
-type column_name() :: id().

%% <row value expression>
-type row_value_expr() :: nonempty_list(literal()).

%% <derived column>
-type derived_column() ::
        value_expr()
      | {value_expr(), column_name()} %% AS
        %% TODO: Improve type
      | {count, value_expr() | all}
      | {count, {distinct, value_expr()}}.

%% <table reference>
%% Expressions for describing "tables" (eg. FROM in a SELECT statement)
-type table_ref() :: table_primary()
                   | joined_table().

%% <table primary>
-type table_primary() :: id()
                       | {id(), id()} %% AS
                       | {query_spec(), id()}
                       | pg_call()
                       | {pg_call(), id}.

%% <joined table>
-type joined_table() :: cross_join()
                      | qualified_join()
                      | natural_join()
                      | union_join().

-type cross_join() :: #cross_join{}.

-type qualified_join() :: #join{} | #qualified_join{}.

-type natural_join() :: #natural_join{}.

-type union_join() :: #union_join{}.

-type join_type() :: inner | left | right | full.

%% <set quantifier>
-type set_quant() :: all | distinct.

%% Predicates
-type predicate() ::
        boolean()
      | {'not', predicate()}
      | {'and', [predicate()]}
      | {'or', [predicate()]}
      | {value_expr(), binop(), value_expr()}
      | {is_null, column_reference()}
      | {exists, query_spec()}
      | {between, value_expr(), value_expr(), value_expr()}
      | {in, value_expr(), query_spec()}.
      %% | some, all, ...

%% <value expr> over-simplified for the moment.
%% <value expr> ::= <common value expression> | <boolean value expression> | <row value expression>
%% <common value expression> ::=
%%   <numeric value expression>
%% | <string value expression>
%% | <datetime value expression>
%% | <interval value expression>
%% | <user-defined type value expression>
%% | <reference value expression>
%% | <collection value expression>
%% <user-defined type value expression> ::= <value expression primary>
%% <reference value expression> ::= <value expression primary>
%% <collection value expression> ::=  <array value expression> | <multiset value expression>
%% <collection value constructor> ::=  <array value constructor> | <multiset value constructor>
%% <value expression primary> ::= <parenthesized value expression> | <nonparenthesized value expression primary>
%% <parenthesized value expression> ::= <left paren> <value expression> <right paren>
%% <nonparenthesized value expression primary> ::= 
%%   <unsigned value specification>
%% | <column reference>
%% | <set function specification>
%% | <window function>
%% | <scalar subquery>
%% | <case expression>
%% | <cast specification>
%% | <field reference>
%% | <subtype treatment>
%% | <method invocation>
%% | <static method invocation>
%% | <new specification>
%% | <attribute or method reference>
%% | <reference resolution>
%% | <collection value constructor>
%% | <array element reference>
%% | <multiset element reference>
%% | <routine invocation>
%% | <next value expression>
-type value_expr() ::
        identifier_chain() %% <column reference>
      | {routine_invocation(), [value_expr()]} %% Represents function calls (a lot of clauses such us <fold>, <trim>, <natural logarithm>, ...
      | [value_expr()] % Array (maybe nested)
      | literal().

%% Supported function names
-type routine_invocation() :: identifier_chain(). %% Function names such as UPPER, LOWER, POWER, ABS...

%% Binary operators
-type binop() :: '=' | '!=' | '<>' | '<' | '>' | '<=' | '>=' | like.

%% COMMIT <commit stmt>
-type commit_stmt() :: commit | commit_and_chain | commit_and_no_chain.

%% START TRANSACTION <start transaction statement>
-type start_trans_stmt() :: start_transaction.

%% ROLLBACK <rollback stmt>
-type rollback_stmt() :: rollback.

%% SELECT <query specification>
-type query_spec() :: #select{}.

%% <from clause>
-type from_clause() :: nonempty_list(table_ref()).

%% <sort specification>
-type sort_spec() :: {value_expr(), asc | desc, last | first}
                   | {value_expr(), asc | desc | last | first}
                   | value_expr().

%% INSERT <insert statement>
-type insert_stmt() :: #insert{}.

%% UPDATE <update statement: searched>
-type update_stmt() :: #update{}.

%% <set clause>
-type set_clause() :: {column_reference(), value_expr()}.

%% DELETE <delete statement: searched>
-type delete_stmt() :: #delete{}.

%% TRUNCATE
-type truncate_stmt() :: #truncate{}.

%% TRUNCATE
-type pg_refresh_stmt() :: #pg_refresh{}.

%% UNION
-type union_stmt() :: #union{}.

%% WITH AS
-type pg_with_as() :: #pg_with{}.

%% Table function call
-type pg_call() :: #pg_call{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc X = [1,2,3], [1, x, 2, x, 3] = intersperse(X, x)
-spec intersperse(list(), any()) -> list().
intersperse([], _) -> [];
intersperse([X | Xs], I) ->
  [X | lists:foldr(fun(Y, Acc) -> [I, Y | Acc] end,
                   [],
                   Xs)].

-spec to_sql(sql_stmt()) -> {Equery, Params}
                              when Equery :: iodata(),
                                   Params :: [literal()].
to_sql(Statement) ->
  Position = 1,
  {_Last_Pos, {Equery, Params}} = to_sql(Position, {sql_stmt, Statement}),
  {[Equery, ";"], Params}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serializes SQL (sub)sentences.
-spec to_sql(Pos,
               {sql_stmt, sql_stmt()}
             | {where_clause, undefined | predicate()}
             | {predicate, predicate()}
             | {value_expr, value_expr()}
             | {value_expr_list, [value_expr()]}
             | {table_ref, table_ref()}
             | {table_primary, table_primary()}
             | {literal, literal()}
             | {offset, undefined | {pos_integer(), pos_integer()}}
             | {on_conflict_update_target, undefined | [column_reference()], [column_reference()]})
            -> {Pos, {Equery, Params}}
            when Pos :: pos_integer(),
                 Equery :: iodata(),
                 Params :: [literal()].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize SQL statement
to_sql(Position, {sql_stmt, start_transaction}) ->
  {Position, {"BEGIN TRANSACTION", []}};
to_sql(Position, {sql_stmt, commit}) ->
  {Position, {"COMMIT", []}};
to_sql(Position, {sql_stmt, commit_and_chain}) ->
  {Position, {"COMMIT AND CHAIN", []}};
to_sql(Position, {sql_stmt, commit_and_no_chain}) ->
  {Position, {"COMMIT AND NO CHAIN", []}};
to_sql(Position, {sql_stmt, rollback}) ->
  {Position, {"ROLLBACK", []}};
to_sql(Position, {sql_stmt, #pg_refresh{materialized_view = View}}) ->
  {Position, {["REFRESH MATERIALIZED VIEW ", identifier_to_sql(View)], []}};
to_sql(Position, {sql_stmt, #truncate{table = Table, cascade = Cascade}}) ->
  case Cascade of
    false ->
      {Position, {["TRUNCATE ", identifier_to_sql(Table)], []}};
    true ->
      {Position, {["TRUNCATE ", identifier_to_sql(Table), " CASCADE"], []}}
  end;
to_sql(P, {sql_stmt, #union{type = Type, queries = Queries, order_by = Sort_Specs}}) ->
  {P2, {Clauses, Params}} = 
    lists:foldl(fun(S, {P0, {Clauses0, Params0}}) -> 
                    {P1, {Clauses1, Params1}} = to_sql(P0, {sql_stmt, S}),
                    %% Remove semicolon from select sql to avoid syntax error
                    Clauses1_Without_Semicolon = lists:droplast(Clauses1),
                    {P1, {Clauses0 ++ [Clauses1_Without_Semicolon], Params0 ++ Params1}}
                end,
                {P, {[], []}},
                Queries),
  {P3, {Sort_Spec_Clauses, Sort_Specs_Params}} =
    to_sql_fold(P2, sort_spec, Sort_Specs),
  Order_By_Clause =
    case Sort_Spec_Clauses of
      [] -> "";
      _ -> [" ORDER BY ", intersperse(Sort_Spec_Clauses, ", ")]
    end,
  Intersperse = 
    case Type of
      all -> " UNION ALL ";
      null -> " UNION "
    end,
  {P3, {["(", 
         intersperse(Clauses, Intersperse), 
         ")",
         Order_By_Clause], Params ++ Sort_Specs_Params}};
to_sql(P0, {sql_stmt, #select{quantifier = Quant,
                              columns = Columns,
                              from = From,
                              where = Where,
                              group_by = Group_By,
                              order_by = Sort_Specs,
                              offset = Offset,
                              for_update = For_Update}}) ->
  Quant_SQL = set_quant_to_sql(Quant),
  {P1, {Columns_SQL, Columns_Parameters}} = to_sql_fold(P0, derived_column, Columns),
  case Columns_SQL of
    [] ->
      Items = "*";
    _ ->
      Items = intersperse(Columns_SQL, ", ")
  end,
  {P2, {Table_Ref_Clauses, Table_Ref_Parameters}} =
    to_sql_fold(P1, table_ref, From),
  {P3, {Where_Clause, Where_Parameters}} = to_sql(P2, {where_clause, Where}),
  {P4, {Sort_Spec_Clauses, Sort_Specs_Parameters}} =
    to_sql_fold(P3, sort_spec, Sort_Specs),
  %% TODO: Group By expression, not only a list of columns.
  {P5, {Group_By_Exprs_SQL, Group_By_Parameters}} = to_sql_fold(P4, derived_column, Group_By),
  case Group_By_Exprs_SQL of
    [] ->
      Group_By_Clause = "";
    _ ->
      Group_By_Clause = [" GROUP BY ", intersperse(Group_By_Exprs_SQL, ", ")]
  end,
  Order_By_Clause =
    case Sort_Spec_Clauses of
      [] -> "";
      _ -> [" ORDER BY ", intersperse(Sort_Spec_Clauses, ", ")]
    end,
  {P6, {Offset_Clause, Offset_Params}} = to_sql(P5, {offset, Offset}),
  case For_Update of
    false ->
      For_Update_Clause = "";
    true ->
      For_Update_Clause = " FOR UPDATE"
  end,
  {P6, {["SELECT ", Quant_SQL, " ", Items,
         [" FROM ", intersperse(Table_Ref_Clauses, ", ")],
         Where_Clause,
         Group_By_Clause,
         Order_By_Clause,
         Offset_Clause,
         For_Update_Clause],
        Columns_Parameters ++ Table_Ref_Parameters ++ Where_Parameters ++ Group_By_Parameters ++ Sort_Specs_Parameters ++ Offset_Params}};
to_sql(P0, {sql_stmt, #insert{table = Table, 
                              columns = Columns, 
                              values = Rows,
                              on_conflict_update_target = Conflict_Columns}}) ->
  {P1, {Values_Clause, Values_Parameters}} =
    to_sql_fold(P0, value_expr_list, Rows),
  {P2, {Conflict_Clause, Conflict_Params}} = to_sql(P1, {on_conflict_update_target, Conflict_Columns, Columns}),
  {P2, {["INSERT INTO ",
         identifier_to_sql(Table),
         " (", intersperse([identifier_to_sql(Column) || Column <- Columns], ", "), ")",
         " VALUES ",
         intersperse(Values_Clause, ", "),
         Conflict_Clause,
         " RETURNING *"], Values_Parameters ++ Conflict_Params}};
to_sql(P0, {sql_stmt, #update{table = Table,
                              set = Set,
                              where = Where}}) ->
  {P1, {Set_Clause, Set_Parameters}} = 
    %% TODO: cannot be easily factored into to_sql_fold
    lists:foldl(fun({Column, Value}, {PI, {Accum_SQL, Accum_Params}}) ->
                    {PJ, {Expr_SQL, Expr_Params}} = to_sql(PI, {value_expr, Value}),
                    {PJ, {Accum_SQL ++ [[identifier_to_sql(Column), " = ", Expr_SQL]], Accum_Params ++ Expr_Params}}
                end,
                {P0, {[], []}},
                Set),
  {P2, {Where_Clause, Where_Parameters}} = to_sql(P1, {where_clause, Where}),
  {P2, {["UPDATE ",
         identifier_to_sql(Table),
         " SET ",
         intersperse(Set_Clause, ", "),
         Where_Clause,
         " RETURNING *"], Set_Parameters ++ Where_Parameters}};
to_sql(P0, {sql_stmt, #delete{table = Table,
                              where = Where}}) ->
  {P1, {Where_Clause, Where_Parameters}} = to_sql(P0, {where_clause, Where}),
  {P1, {["DELETE FROM ", identifier_to_sql(Table), Where_Clause, " RETURNING *"], Where_Parameters}};
%% Serialize <contextually typed row value expression list>
%% Serialize a values clause
to_sql(P0, {sql_stmt, #pg_with{definitions = Definitions,
			    select = Select}}) ->
  {P1, {Query_Clause, Query_Params}} =
    lists:foldl(fun({Name, Query}, {PI, {Accum_SQL, Accum_Params}}) ->
		    {PJ, {Expr_SQL, Expr_Params}} = to_sql(PI, {sql_stmt, Query}),
		    {PJ, {Accum_SQL ++ [[identifier_chain_to_sql(Name), " AS (", Expr_SQL, ")"]], Accum_Params ++ Expr_Params}}
		end,
	       {P0, {[], []}},
	       Definitions),
  {P2, {Select_Clause, Select_Params}} = to_sql(P1, {sql_stmt, Select}),
  {P2, {["WITH ",
	 intersperse(Query_Clause, ", "),
	 " ",
	 Select_Clause], Query_Params ++ Select_Params}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize <derived column>
to_sql(P0, {derived_column, {count, all}}) ->
  {P0, {["COUNT(*)"],[]}};
to_sql(P0, {derived_column, {count, {distinct, Column}}}) ->
  {P1, {Value_Expr_SQL, Value_Expr_Parameters}} =
    to_sql(P0, {value_expr, Column}),
  {P1,{["COUNT(DISTINCT ", Value_Expr_SQL, ")"], Value_Expr_Parameters}};
to_sql(P0, {derived_column, {count, Column}}) ->
  {P1, {Value_Expr_SQL, Value_Expr_Parameters}} =
    to_sql(P0, {value_expr, Column}),
  {P1,{["COUNT(", Value_Expr_SQL, ")"], Value_Expr_Parameters}};
to_sql(P0, {derived_column, {Column, Alias}}) when ?IS_IDENTIFIER(Alias)->
  {P1, {Value_Expr_SQL, Value_Expr_Parameters}} =
    to_sql(P0, {value_expr, Column}),
  {P1, {[Value_Expr_SQL, " AS ", identifier_to_sql(Alias)], Value_Expr_Parameters}};
to_sql(P0, {derived_column, Column}) ->
  to_sql(P0, {value_expr, Column});
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize a list of <value expression>
to_sql(P0, {value_expr_list, Row}) ->
  {P1, {Values_Clause, Values_Parameters}} = 
    to_sql_fold(P0, value_expr, Row),
  {P1, {["(", intersperse(Values_Clause, ", "), ")"], Values_Parameters}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize <table reference>
to_sql(P0, {table_ref, #join{type = no_join,
                             table = Table,
                             joins = Joins}}) ->
  {P1, {Table_Clauses, Table_Params}} =
    to_sql(P0, {table_ref, Table}),
  {P2, {Joins_Clauses, Joins_Params}} = 
    to_sql_fold(P1, join, Joins),
  {P2, {[Table_Clauses, $ ,
         intersperse(Joins_Clauses, " ")],
        Table_Params ++ Joins_Params}};
to_sql(P0, {join, #join{type = Type,
                        table = Table,
                        spec = Spec}}) ->
  {P1, {Table_Clauses, Table_Params}} = to_sql(P0, {table_ref, Table}),
  {P2, {Pred_SQL, Pred_Parameters}} = to_sql(P1, {predicate, Spec}),
  {P2, {[case Type of
           inner -> "INNER";
           left -> "LEFT OUTER";
           right -> "RIGHT OUTER";
           full -> "FULL OUTER"
         end, $ ,
         "JOIN", $ ,
         Table_Clauses,$ ,
         "ON",$ ,
         Pred_SQL], 
        Table_Params ++ Pred_Parameters}};
to_sql(P0, {table_ref, #cross_join{left = Left, right = Right}}) ->
  {P1, {Left_Clause, Left_Params}} = to_sql(P0, {table_ref, Left}),
  {P2, {Right_Clause, Right_Params}} = to_sql(P1, {table_primary, Right}),
  {P2, {["(",
         Left_Clause, $ ,
         "CROSS JOIN", $ ,
         Right_Clause,
         ")"],
        Left_Params ++ Right_Params}};
to_sql(P0, {table_ref, #natural_join{left = Left, right = Right}}) ->
  {P1, {Left_Clause, Left_Params}} = to_sql(P0, {table_ref, Left}),
  {P2, {Right_Clause, Right_Params}} = to_sql(P1, {table_primary, Right}),
  {P2, {["(",
         Left_Clause, $ ,
         "NATURAL JOIN", $ ,
         Right_Clause,
         ")"],
        Left_Params ++ Right_Params}};
to_sql(P0, {table_ref, #union_join{left = Left, right = Right}}) ->
  {P1, {Left_Clause, Left_Params}} = to_sql(P0, {table_ref, Left}),
  {P2, {Right_Clause, Right_Params}} = to_sql(P1, {table_primary, Right}),
  {P2, {["(",
         Left_Clause, $ ,
         "UNION JOIN", $ ,
         Right_Clause,
         ")"],
        Left_Params ++ Right_Params}};
to_sql(P0, {table_ref, #qualified_join{type = Type, left = Left, right = Right, on = On}}) ->
  {P1, {Left_Clause, Left_Params}} = to_sql(P0, {table_ref, Left}),
  {P2, {Right_Clause, Right_Params}} = to_sql(P1, {table_ref, Right}),
  {P3, {On_Clause, On_Params}} = to_sql(P2, {predicate, On}),
  {P3, {["(",
         Left_Clause, $ ,
         case Type of
           inner -> "INNER";
           left -> "LEFT OUTER";
           right -> "RIGHT OUTER";
           full -> "FULL OUTER"
         end, $ ,
         "JOIN", $ ,
         Right_Clause, $ ,
         "ON", $ ,
         On_Clause,
         ")"
        ],
        Left_Params ++ Right_Params ++ On_Params}};
to_sql(P0, {table_ref, Table_Primary}) ->
  to_sql(P0, {table_primary, Table_Primary});
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize <table primary>
to_sql(P0, {table_primary, #pg_call{name = Name, args = Args}}) ->
  {P1, {Args_Clauses, Args_Params}} = to_sql_fold(P0, value_expr, Args),
  {P1, {[identifier_to_sql(Name),
         "(",
         intersperse(Args_Clauses, ", "),
         ")"], Args_Params}};
to_sql(P0, {table_primary, {#pg_call{} = Call, Correlation_Name}}) ->
  {P1, {Call_Clause, Call_Params}} = to_sql(P0, {table_primary, Call}),
  Correlation = identifier_to_sql(Correlation_Name),
  {P1, {[Call_Clause, $ ,
         "AS", $ ,
         Correlation], Call_Params}};
to_sql(P0, {table_primary, {#select{} = Select, Correlation_Name}}) ->
  {P1, {Clauses, Select_Params}} = to_sql(P0, {sql_stmt, Select}),
  Correlation = identifier_to_sql(Correlation_Name),
  %% Remove semicolon from select sql to avoid syntax error
  %% TODO: The select shouldn't have the semicolon, which sould be added
  %% at the end of any query, since semicolon can only happen once and at the end.
  Clauses_Without_Semicolon = lists:droplast(Clauses),
  {P1, {["(",
         Clauses_Without_Semicolon,
         ")", $ ,
         "AS", $ ,
         Correlation], Select_Params}};
to_sql(P0, {table_primary, {Table_Name, Correlation_Name}}) ->
  Table = identifier_chain_to_sql(Table_Name),
  Correlation = identifier_to_sql(Correlation_Name),
  {P0, {[Table, $ ,
        "AS", $ , Correlation], []}};
to_sql(P0, {table_primary, Table_Name}) ->
  {P0, {identifier_chain_to_sql(Table_Name), []}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize <where clause>
to_sql(P0, {where_clause, undefined}) ->
  {P0, {"", []}};
to_sql(P0, {where_clause, Predicate}) ->
  {P1, {Pred_SQL, Pred_Params}} = to_sql(P0, {predicate, Predicate}),
  {P1, {[" WHERE ", Pred_SQL], Pred_Params}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize a sort specification (<window clause>)
to_sql(P0, {sort_spec, {Value, Order, Nulls}}) ->
  {P1, {Value_SQL, Value_Params}} = to_sql(P0, {value_expr, Value}),
  {P1, {[Value_SQL, " ",
         case Order of asc -> "ASC"; desc -> "DESC" end,
         " NULLS ",
         case Nulls of last -> "LAST"; first -> "FIRST" end],
       Value_Params}};
to_sql(P0, {sort_spec, {Value, asc}}) ->
  {P1, {Value_SQL, Value_Params}} = to_sql(P0, {value_expr, Value}),
  {P1, {[Value_SQL, " ASC"], Value_Params}};
to_sql(P0, {sort_spec, {Value, desc}}) ->
  {P1, {Value_SQL, Value_Params}} = to_sql(P0, {value_expr, Value}),
  {P1, {[Value_SQL, " DESC"], Value_Params}};
to_sql(P0, {sort_spec, {Value, first}}) ->
  {P1, {Value_SQL, Value_Params}} = to_sql(P0, {value_expr, Value}),
  {P1, {[Value_SQL, " NULLS FIRST"], Value_Params}};
to_sql(P0, {sort_spec, {Value, last}}) ->
  {P1, {Value_SQL, Value_Params}} = to_sql(P0, {value_expr, Value}),
  {P1, {[Value_SQL, " NULLS LAST"], Value_Params}};
to_sql(P0, {sort_spec, Value}) ->
  to_sql(P0, {value_expr, Value});
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize offset/fetch (<window clause>)
to_sql(P0, {offset, undefined}) ->
  {P0, {"", []}};
to_sql(P0, {offset, {Start, Count}}) ->
  {P1, {Start_SQL, Start_Params}} = to_sql(P0, {value_expr, Start}),
  {P2, {Count_SQL, Count_Params}} = to_sql(P1, {value_expr, Count}),
  {P2, {[" OFFSET (", Start_SQL, ") FETCH NEXT (", Count_SQL, ") ROWS ONLY"],
        Start_Params ++ Count_Params}};
to_sql(P0, {offset, {Value, Order, Nulls}}) ->
  {P1, {Value_SQL, Value_Params}} = to_sql(P0, {value_expr, Value}),
  {P1, {[Value_SQL, " ",
         case Order of asc -> "ASC"; desc -> "DESC" end,
         " NULLS ",
         case Nulls of last -> "LAST"; first -> "FIRST" end],
       Value_Params}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize on conflict
to_sql(P0, {on_conflict_update_target, undefined, _}) ->
  {P0, {"", []}};
to_sql(P0, {on_conflict_update_target, [], _Columns}) ->
  %% Conflict columns is []
  {P0, {[" ON CONFLICT DO NOTHING"],
        []}};
to_sql(P0, {on_conflict_update_target, Conflict_Columns, Columns}) ->
  Columns_To_Update = lists:subtract(Columns, Conflict_Columns),
  Set_Clauses = [ begin
                    Column_SQL = identifier_to_sql(Column),
                    [Column_SQL, " = EXCLUDED.", Column_SQL]
                  end || Column <- Columns_To_Update ],
  {P0, {[" ON CONFLICT (", 
         intersperse([identifier_to_sql(Column) || Column <- Conflict_Columns], ", "), 
         ")",
         " DO UPDATE SET ", 
         intersperse(Set_Clauses, ", ")],
        []}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize <predicate>
to_sql(P0, {predicate, true}) ->
  {P0, {"TRUE", []}};
to_sql(P0, {predicate, false}) ->
  {P0, {"FALSE", []}};
to_sql(P0, {predicate, {'not', Predicate}}) ->
  {P1, {Pred_SQL, Pred_Params}} = to_sql(P0, {predicate, Predicate}),
  {P1, {["NOT (", Pred_SQL, ")"], Pred_Params}};
to_sql(P0, {predicate, {Logic_Bin_Op, [Pred1 | Predicates]}}) when Logic_Bin_Op == 'and';
                                                                   Logic_Bin_Op == 'or' ->
  Operator = case Logic_Bin_Op of
               'and' -> " AND ";
               'or' -> " OR "
             end,
  {P1, {Pred1_SQL, Pred1_Params}} = to_sql(P0, {predicate, Pred1}),
  %% TODO: cannot be easily factored into to_sql_fold
  lists:foldl(fun(Pred, {PI, {Accum_SQL, Accum_Params}}) ->
                  {PJ, {Pred_SQL, Pred_Params}} = to_sql(PI, {predicate, Pred}),
                  {PJ, {["(", Accum_SQL, Operator, Pred_SQL, ")"], Accum_Params ++ Pred_Params}}
              end,
              {P1, {Pred1_SQL, Pred1_Params}},
              Predicates);
to_sql(P0, {predicate, {is_null, Column}}) ->
  {P0, {[identifier_chain_to_sql(Column), " IS NULL"], []}};
to_sql(P0, {predicate, {Left, Bin_Op, Right}})
  when Bin_Op == '=';
       Bin_Op == '!=';
       Bin_Op == '<>';
       Bin_Op == '<';
       Bin_Op == '>';
       Bin_Op == '<=';
       Bin_Op == '>=';
       Bin_Op == like ->
  Bin_Op_SQL =
    if Bin_Op == like -> <<"LIKE">>;
       true -> atom_to_binary(Bin_Op, utf8)
    end,
  {P1, {Left_SQL, Left_Params}} = to_sql(P0, {value_expr, Left}),
  {P2, {Right_SQL, Right_Params}} = to_sql(P1, {value_expr, Right}),
  {P2, {[Left_SQL, " ", Bin_Op_SQL, " ", Right_SQL],
        Left_Params ++ Right_Params}};
to_sql(P0, {predicate, {exists, Select = #select{}}}) ->
  {P1, {Select_Clause, Select_Parameters}} = to_sql(P0, {sql_stmt, Select}),
  {P1, {["EXISTS ", "(", Select_Clause, ")"], Select_Parameters}};
to_sql(P0, {predicate, {between, Expr, Min, Max}}) ->
  {P1, {Expr_SQL, Expr_Params}} = to_sql(P0, {value_expr, Expr}),
  {P2, {Min_SQL, Min_Params}} = to_sql(P1, {value_expr, Min}),
  {P3, {Max_SQL, Max_Params}} = to_sql(P2, {value_expr, Max}),
  {P3, {[Expr_SQL,
        " BETWEEN ",
        Min_SQL, " AND ", Max_SQL], Expr_Params ++ Min_Params ++ Max_Params}};
to_sql(P0, {predicate, {in, Expr, Select = #select{}}}) ->
  {P1, {Expr_SQL, Expr_Params}} = to_sql(P0, {value_expr, Expr}),
  {P2, {Select_SQL, Select_Params}} = to_sql(P1, {sql_stmt, Select}),
  %% Remove semicolon from select sql to avoid syntax error
  Select_SQL_Without_Semicolon = lists:droplast(Select_SQL),
  {P2, {[Expr_SQL, " IN (", Select_SQL_Without_Semicolon, ")"], Expr_Params ++ Select_Params}};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize a <value expression>
to_sql(P0, {value_expr, Column}) when ?IS_IDENTIFIER(Column) ->
  {P0, {identifier_chain_to_sql(Column), []}};
to_sql(P0, {value_expr, {cast, [Expr, Type]}}) ->
  {P1, {Expr_SQL, Expr_Params}} = 
    to_sql(P0, {value_expr, Expr}),
  Type_SQL = identifier_chain_to_sql(Type),
  {P1, {["CAST(",Expr_SQL," AS ", Type_SQL, ")"], Expr_Params}};
to_sql(P0, {value_expr, {Function_Name, Actual_Args}}) ->
  Routine_Name = atom_to_binary(Function_Name, utf8),
  {P1, {Args_SQLs, Args_Params}} = 
    to_sql_fold(P0, value_expr, Actual_Args),
  {P1, {[Routine_Name, "(", intersperse(Args_SQLs, ", "), ")"], Args_Params}};
to_sql(P0, {value_expr, Value_Exprs}) when is_list(Value_Exprs) ->
  {P1, {Values_SQLs, Values_Params}} = 
    to_sql_fold(P0, value_expr, Value_Exprs),
  {P1, {["{", intersperse(Values_SQLs, ", "), "}"], Values_Params}};
to_sql(P0, {value_expr, Literal}) ->
  to_sql(P0, {literal, Literal});
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize a <literal>
to_sql(P0, {literal, null}) ->
  {P0, {<<"NULL">>, []}};
to_sql(P0, {literal, true}) ->
  {P0, {<<"TRUE">>, []}};
to_sql(P0, {literal, false}) ->
  {P0, {<<"FALSE">>, []}};
to_sql(P0, {literal, Value}) when is_binary(Value);
                                  is_integer(Value);
                                  is_float(Value) ->
  {P0+1, {get_placeholder(P0), [Value]}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serialize a list of AST nodes
to_sql_fold(P0, Node_Type, Nodes) ->
  lists:foldl(fun(Node, {PI, {Accum_SQL, Accum_Params}}) ->
                  {PJ, {Node_SQL, Node_Params}} =
                    to_sql(PI, {Node_Type, Node}),
                  {PJ, {Accum_SQL ++ [Node_SQL], Accum_Params ++ Node_Params}}
              end,
              {P0, {[], []}},
              Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize set quantifier
-spec set_quant_to_sql(set_quant()) -> iodata().
set_quant_to_sql(all) -> "ALL";
set_quant_to_sql(distinct) -> "DISTINCT".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize an identifier
-spec identifier_to_sql(id()) -> iodata().
identifier_to_sql(Id) ->
  Identifier = atom_to_binary(Id, utf8),
  case re:run(Identifier, ?IDENTIFIER_MP) of
    {match, [{0,0}]} ->
      throw({non_valid_identifier, Id});
    {match, _Captured} ->
      Identifier;
    nomatch ->
      throw({non_valid_identifier, Id})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize an identifier_chain
-spec identifier_chain_to_sql(identifier_chain()) -> iodata().
identifier_chain_to_sql(Id) ->
  Identifier = atom_to_binary(Id, utf8),
  case re:run(Identifier, ?IDENTIFIER_CHAIN_MP) of
    {match, [{0,0}]} ->
      throw({non_valid_identifier, Id});
    {match, _Captured} ->
      Identifier;
    nomatch ->
      throw({non_valid_identifier, Id})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns placeholder according to position
-spec get_placeholder(Position :: integer()) -> string().
get_placeholder(Position) when is_integer(Position) ->
  "$" ++ integer_to_list(Position).
