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
   [
    commit_stmt/0,
    column_name/0,
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
    update_stmt/0,
    value_expr/0
   ]
).

%% TODO: convert to non terminal symbols from http://savage.net.au/SQL/sql-2003-2.bnf.html
-export_type(
   [name/0,
    predicate/0,
    binop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([to_sql/1]).

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
      | delete_stmt().

%% Any name (column name, table name, alias, ...)
-type name() :: atom().

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

%% <table name>
-type table_name() :: name().

%% <column name>
-type column_name() :: name().

%% <row value expression>
-type row_value_expr() :: nonempty_list(literal()).

%% <derived column>
-type derived_column() ::
        value_expr()
      | {value_expr(), column_name()}. %% AS

%% Expressions for describing "tables" (eg. FROM in a SELECT statement)
-type table_ref() :: table_primary()
                   | joined_table().

-type table_primary() :: name()
                       | {name(), name()}. %% AS

-type joined_table() :: qualified_join().

-type qualified_join() :: #join{}.

-type join_type() :: inner | left | right | full.

-type join_condition() :: join_condition().

%% <set quantifier>
-type set_quant() :: all | distinct.

%% Predicates
-type predicate() ::
        {'not', predicate()}
      | {'and', [predicate()]}
      | {'or', [predicate()]}
      | {value_expr(), binop(), value_expr()}
      | {is_null, column_name()}
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
        column_name() %% <column reference>
      | {function_name(), [value_expr()]} %% Represents function calls (a lot of clauses such us <fold>, <trim>, <natural logarithm>, ...
      | [value_expr()] % Array (maybe nested)
      | literal().

%% Supported function names
-type function_name() :: binary(). %% Function names such as UPPER, LOWER, POWER, ABS...

%% Binary operators
-type binop() :: '=' | '!=' | '<' | '>' | '<=' | '>=' | like.

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
-type set_clause() :: {column_name(), value_expr()}.

%% DELETE <delete statement: searched>
-type delete_stmt() :: #delete{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc X = [1,2,3], [1, x, 2, x, 3] = intersperse(X, x)
-spec intersperse(list(),list()) -> list().
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
  {_Last_Pos, Result} = to_sql(Position, {sql_stmt, Statement}),
  Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serializes SQL (sub)sentences.
-spec to_sql(Pos,
               {sql_stmt, sql_stmt()}
             | {where_clause, undefined | predicate()}
             | {predicate, predicate()}
             | {value_expr, value_expr()}
             | {value_expr_list, [value_expr()]}
             | {table_ref, table_ref()}
             | {literal, literal()})
            -> {Pos, {Equery, Params}}
            when Pos :: pos_integer(),
                 Equery :: iodata(),
                 Params :: [literal()].
%% Serialize SQL statement
to_sql(Position, {sql_stmt, start_transaction}) ->
  {Position, {"BEGIN TRANSACTION;", []}};
to_sql(Position, {sql_stmt, commit}) ->
  {Position, {"COMMIT;", []}};
to_sql(Position, {sql_stmt, commit_and_chain}) ->
  {Position, {"COMMIT AND CHAIN;", []}};
to_sql(Position, {sql_stmt, commit_and_no_chain}) ->
  {Position, {"COMMIT AND NO CHAIN;", []}};
to_sql(Position, {sql_stmt, rollback}) ->
  {Position, {"ROLLBACK;", []}};
to_sql(P0, {sql_stmt, #select{quantifier = Quant,
                              columns = Columns,
                              from = From,
                              where = Where,
                              order_by = Sort_Specs,
                              offset = Offset}}) ->
  Quant_SQL = set_quant_to_sql(Quant),
  case Columns of
    [] ->
      Items = "*";
    _ ->
      Items = intersperse([derived_col_to_sql(Column) || Column <- Columns], ", ")
  end,
  {P1, {Table_Ref_Clauses, Table_Ref_Parameters}} = 
    to_sql_fold(P0, table_ref, From),
  {P2, {Where_Clause, Where_Parameters}} = to_sql(P1, {where_clause, Where}),
  {P3, {Sort_Spec_Clauses, Sort_Specs_Parameters}} =
    to_sql_fold(P2, sort_spec, Sort_Specs),
  Order_By_Clause =
    case Sort_Spec_Clauses of
      [] -> "";
      _ -> [" ORDER BY ", intersperse(Sort_Spec_Clauses, ", ")]
    end,
  {P4, {Offset_Clause, Offset_Params}} = to_sql(P3, {offset, Offset}),
  {P4, {["SELECT ", Quant_SQL, " ", Items,
         [" FROM ", intersperse(Table_Ref_Clauses, ", ")],
         Where_Clause,
         Order_By_Clause,
         Offset_Clause,
         ";"], 
        Table_Ref_Parameters ++ Where_Parameters ++ Sort_Specs_Parameters ++ Offset_Params}};
to_sql(P0, {sql_stmt, #insert{table = Table, 
                              columns = Columns, 
                              values = Rows}}) ->
  {P1, {Values_Clause, Values_Parameters}} =
    to_sql_fold(P0, value_expr_list, Rows),
  {P1, {["INSERT INTO ",
         name_to_sql(Table),
         " (", intersperse([name_to_sql(Column) || Column <- Columns], ", "), ")",
         " VALUES ",
         intersperse(Values_Clause, ", "),
         " RETURNING *;"], Values_Parameters}};
to_sql(P0, {sql_stmt, #update{table = Table,
                              set = Set,
                              where = Where}}) ->
  {P1, {Set_Clause, Set_Parameters}} = 
    %% TODO: cannot be easily factored into to_sql_fold
    lists:foldl(fun({Column, Value}, {PI, {Accum_SQL, Accum_Params}}) ->
                    {PJ, {Expr_SQL, Expr_Params}} = to_sql(PI, {value_expr, Value}),
                    {PJ, {Accum_SQL ++ [[name_to_sql(Column), " = ", Expr_SQL]], Accum_Params ++ Expr_Params}}
                end,
                {P0, {[], []}},
                Set),
  {P2, {Where_Clause, Where_Parameters}} = to_sql(P1, {where_clause, Where}),
  {P2, {["UPDATE ",
         name_to_sql(Table),
         " SET ",
         intersperse(Set_Clause, ", "),
         Where_Clause,
         " RETURNING *;"], Set_Parameters ++ Where_Parameters}};
to_sql(P0, {sql_stmt, #delete{table = Table,
                              where = Where}}) ->
  {P1, {Where_Clause, Where_Parameters}} = to_sql(P0, {where_clause, Where}),
  {P1, {["DELETE FROM ", name_to_sql(Table), Where_Clause, " RETURNING *;"], Where_Parameters}};
%% Serialize <contextually typed row value expression list>
%% Serialize a values clause
to_sql(P0, {value_expr_list, Row}) ->
  {P1, {Values_Clause, Values_Parameters}} = 
    to_sql_fold(P0, value_expr, Row),
  {P1, {["(", intersperse(Values_Clause, ", "), ")"], Values_Parameters}};
%% Serialize <table reference>
to_sql(P0, {table_ref, #join{type = Type,
                             left = Left,
                             right = Right,
                             spec = Spec}}) ->
  {P1, {Ref_Left_SQL, _Ref_Left_Params}} = to_sql(P0, {table_ref, Left}),
  {P2, {Ref_Right_SQL, _Ref_Right_Params}} = to_sql(P1, {table_ref, Right}),
  {P3, {Pred_SQL, Pred_Parameters}} = to_sql(P2, {predicate, Spec}),
  {P3, {[Ref_Left_SQL,$ ,
         case Type of
           inner -> "INNER";
           left -> "LEFT OUTER";
           right -> "RIGHT OUTER";
           full -> "FULL OUTER"
         end, $ ,
         "JOIN ",
         Ref_Right_SQL,$ ,
         "ON",$ ,
         Pred_SQL], 
        Pred_Parameters}};
to_sql(P0, {table_ref, {Table_Name, Correlation_Name}}) ->
  {P0, {[atom_to_binary(Table_Name, utf8),$ ,
         "AS",$ ,
         atom_to_binary(Correlation_Name, utf8)], 
        []}};
to_sql(P0, {table_ref, Table_Name}) ->
  {P0, {atom_to_binary(Table_Name, utf8), []}};
%% Serialize <where clause>
to_sql(P0, {where_clause, undefined}) ->
  {P0, {"", []}};
to_sql(P0, {where_clause, Predicate}) ->
  {P1, {Pred_SQL, Pred_Params}} = to_sql(P0, {predicate, Predicate}),
  {P1, {[" WHERE ", Pred_SQL], Pred_Params}};
%% Serialize <sort_spec>
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
%% Serialize offset/fetch
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
%% Serialize <predicate>
to_sql(P0, {predicate, {'not', Predicate}}) ->
  {P1, {Pred_SQL, Pred_Params}} = to_sql(P0, {predicate, Predicate}),
  {P1, {["NOT ", Pred_SQL], Pred_Params}};
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
                  {PJ, {[Accum_SQL, Operator, Pred_SQL], Accum_Params ++ Pred_Params}}
              end,
              {P1, {Pred1_SQL, Pred1_Params}},
              Predicates);
to_sql(P0, {predicate, {is_null, Column}}) ->
  {P0, {[name_to_sql(Column), " IS NULL"], []}};
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
  {P2, {[Expr_SQL, " IN ", Select_SQL], Expr_Params ++ Select_Params}};
%% Serialize a value expression
to_sql(P0, {value_expr, Column}) when is_atom(Column),
                                      Column /= null,
                                      Column /= true,
                                      Column /= false ->
  {P0, {name_to_sql(Column), []}};
to_sql(P0, {value_expr, {Function_Name, Actual_Args}})
  when is_binary(Function_Name) ->
  {P1, {Args_SQLs, Args_Params}} = 
    to_sql_fold(P0, value_expr, Actual_Args),
  {P1, {[Function_Name, "(", intersperse(Args_SQLs, ", "), ")"], Args_Params}};
to_sql(P0, {value_expr, Value_Exprs}) when is_list(Value_Exprs) ->
  {P1, {Values_SQLs, Values_Params}} = 
    to_sql_fold(P0, value_expr, Value_Exprs),
  {P1, {["{", intersperse(Values_SQLs, ", "), "}"], Values_Params}};
to_sql(P0, {value_expr, Literal}) ->
  to_sql(P0, {literal, Literal});
%% Serialize a literal
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
%% @doc Serialize a derived column
-spec derived_col_to_sql(derived_column()) -> iodata().
derived_col_to_sql(Column) when is_atom(Column) ->
  name_to_sql(Column);
derived_col_to_sql({Column, Alias}) ->
  [name_to_sql(Column), " AS ", name_to_sql(Alias)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize a name
-spec name_to_sql(name()) -> iodata().
name_to_sql(Name) ->
  atom_to_binary(Name, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns placeholder according to position
-spec get_placeholder(Position :: integer()) -> string().
get_placeholder(Position) when is_integer(Position) ->
  "$" ++ integer_to_list(Position).
