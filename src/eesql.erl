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

%% <table name>
-type table_name() :: identifier_chain().

%% <column reference>
-type column_reference() :: identifier_chain().

%% <column name>
-type column_name() :: id().

%% <row value expression>
-type row_value_expr() :: nonempty_list(literal()).

%% <derived column>
-type derived_column() ::
        value_expr()
      | {value_expr(), column_reference()} %% AS
        %% TODO: Improve type
      | {count, column_reference() | all}
      | {count, {distinct, column_reference()}}.

%% Expressions for describing "tables" (eg. FROM in a SELECT statement)
-type table_ref() :: table_primary()
                   | joined_table()
                   | {query_spec(), id()}
                   | pg_call().

-type table_primary() :: id()
                       | {id(), id()}. %% AS

-type joined_table() :: qualified_join().

-type qualified_join() :: #join{}.

-type join_type() :: inner | left | right | full.

-type join_condition() :: join_condition().

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
             | {literal, literal()}
             | {offset, undefined | {pos_integer(), pos_integer()}}
             | {on_conflict_update_target, undefined | [column_reference()], [column_reference()]}
             | {identifier, id()}
             | {identifier_chain, identifier_chain()})
            -> {Pos, {Equery, Params}}
            when Pos :: pos_integer(),
                 Equery :: iodata(),
                 Params :: [literal()].
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
  {Position, {["REFRESH MATERIALIZED VIEW ", name_to_sql(View)], []}};
to_sql(Position, {sql_stmt, #truncate{table = Table, cascade = Cascade}}) ->
  case Cascade of
    false ->
      {Position, {["TRUNCATE ", name_to_sql(Table)], []}};
    true ->
      {Position, {["TRUNCATE ", name_to_sql(Table), " CASCADE"], []}}
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
  %% TODO: Group By expression, not only a list of columns.
  case Group_By of
    [] ->
      Group_By_Clause = "";
    _ ->
      Group_By_Clause = [" GROUP BY ", intersperse([derived_col_to_sql(Column) || Column <- Group_By], ", ")]
  end,
  Order_By_Clause =
    case Sort_Spec_Clauses of
      [] -> "";
      _ -> [" ORDER BY ", intersperse(Sort_Spec_Clauses, ", ")]
    end,
  {P4, {Offset_Clause, Offset_Params}} = to_sql(P3, {offset, Offset}),
  case For_Update of
    false ->
      For_Update_Clause = "";
    true ->
      For_Update_Clause = " FOR UPDATE"
  end,
  {P4, {["SELECT ", Quant_SQL, " ", Items,
         [" FROM ", intersperse(Table_Ref_Clauses, ", ")],
         Where_Clause,
         Group_By_Clause,
         Order_By_Clause,
         Offset_Clause,
         For_Update_Clause],
        Table_Ref_Parameters ++ Where_Parameters ++ Sort_Specs_Parameters ++ Offset_Params}};
to_sql(P0, {sql_stmt, #insert{table = Table, 
                              columns = Columns, 
                              values = Rows,
                              on_conflict_update_target = Conflict_Columns}}) ->
  {P1, {Values_Clause, Values_Parameters}} =
    to_sql_fold(P0, value_expr_list, Rows),
  {P2, {Conflict_Clause, Conflict_Params}} = to_sql(P1, {on_conflict_update_target, Conflict_Columns, Columns}),
  {P2, {["INSERT INTO ",
         name_to_sql(Table),
         " (", intersperse([name_to_sql(Column) || Column <- Columns], ", "), ")",
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
         " RETURNING *"], Set_Parameters ++ Where_Parameters}};
to_sql(P0, {sql_stmt, #delete{table = Table,
                              where = Where}}) ->
  {P1, {Where_Clause, Where_Parameters}} = to_sql(P0, {where_clause, Where}),
  {P1, {["DELETE FROM ", name_to_sql(Table), Where_Clause, " RETURNING *"], Where_Parameters}};
%% Serialize <contextually typed row value expression list>
%% Serialize a values clause
to_sql(P0, {sql_stmt, #pg_with{definitions = Definitions,
			    select = Select}}) ->
  {P1, {Query_Clause, Query_Params}} =
    lists:foldl(fun({Name, Query}, {PI, {Accum_SQL, Accum_Params}}) ->
		    {PJ, {Expr_SQL, Expr_Params}} = to_sql(PI, {sql_stmt, Query}),
		    {PJ, {Accum_SQL ++ [[name_to_sql(Name), " AS (", Expr_SQL, ")"]], Accum_Params ++ Expr_Params}}
		end,
	       {P0, {[], []}},
	       Definitions),
  {P2, {Select_Clause, Select_Params}} = to_sql(P1, {sql_stmt, Select}),
  {P2, {["WITH ",
	 intersperse(Query_Clause, ", "),
	 " ",
	 Select_Clause], Query_Params ++ Select_Params}};
to_sql(P0, {value_expr_list, Row}) ->
  {P1, {Values_Clause, Values_Parameters}} = 
    to_sql_fold(P0, value_expr, Row),
  {P1, {["(", intersperse(Values_Clause, ", "), ")"], Values_Parameters}};
%% Serialize <table reference>
to_sql(P0, {table_ref, {#select{} = Select, Alias}}) ->
  {P1, {Clauses, Params}} = to_sql(P0, {sql_stmt, Select}),
  %% Remove semicolon from select sql to avoid syntax error
  %% TODO: The select shouldn't have the semicolon, which sould be added
  %% at the end of any query, since semicolon can only happen once and at the end.
  Clauses_Without_Semicolon = lists:droplast(Clauses),
  {P1, {["(",
         Clauses_Without_Semicolon,
         ") AS ",
         atom_to_binary(Alias, utf8)], Params}};
to_sql(P0, {table_ref, #pg_call{name = Name,
                                args = Args}}) ->
  {P1, {Args_Clauses, Args_Params}} = to_sql_fold(P0, value_expr, Args),
  {P1, {[name_to_sql(Name),
         "(",
         intersperse(Args_Clauses, ", "),
        ")"], Args_Params}};
to_sql(P0, {table_ref, #join{type = no_join,
                             table = Table,
                             joins = Joins}}) ->
  {P1, {Table_Clauses, _Params}} = to_sql(P0, {table_ref, Table}),
  {P2, {Joins_Clauses, Joins_Params}} = 
    to_sql_fold(P1, join, Joins),
  {P2, {[Table_Clauses, $ ,
         intersperse(Joins_Clauses, " ")],
        Joins_Params}};
to_sql(P0, {join, #join{type = Type,
                        table = Table,
                        spec = Spec}}) ->
  {P1, {Table_Clauses, _Params}} = to_sql(P0, {table_ref, Table}),
  {P2, {Pred_SQL, Pred_Parameters}} = to_sql(P1, {predicate, Spec}),
  {P2, {[case Type of
           inner -> "INNER";
           left -> "LEFT OUTER";
           right -> "RIGHT OUTER";
           full -> "FULL OUTER"
         end, $ ,
         "JOIN ",
         Table_Clauses,$ ,
         "ON",$ ,
         Pred_SQL], 
        Pred_Parameters}};
to_sql(P0, {table_ref, {Table_Name, Correlation_Name}}) ->
  {P1, {Table, Table_Params}} = to_sql(P0, {identifier_chain, Table_Name}),
  {P2, {Correlation, Correlation_Params}} = to_sql(P1, {identifier, Correlation_Name}),
  {P2, {[Table, $ ,
        "AS", $ , Correlation], Table_Params ++ Correlation_Params}};
to_sql(P0, {table_ref, Table_Name}) ->
  to_sql(P0, {identifier, Table_Name});
to_sql(P0, {identifier, Id}) ->
  %% TODO: Check if Identifier is wellformed
  Identifier = atom_to_binary(Id, utf8),
  {P0, {Identifier, []}};
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
                    Column_SQL = name_to_sql(Column),
                    [Column_SQL, " = EXCLUDED.", Column_SQL]
                  end || Column <- Columns_To_Update ],
  {P0, {[" ON CONFLICT (", 
         intersperse([name_to_sql(Column) || Column <- Conflict_Columns], ", "), 
         ")",
         " DO UPDATE SET ", 
         intersperse(Set_Clauses, ", ")],
        []}};
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
  %% Remove semicolon from select sql to avoid syntax error
  Select_SQL_Without_Semicolon = lists:droplast(Select_SQL),
  {P2, {[Expr_SQL, " IN (", Select_SQL_Without_Semicolon, ")"], Expr_Params ++ Select_Params}};
%% Serialize a value expression
to_sql(P0, {value_expr, Column}) when is_atom(Column),
                                      Column /= null,
                                      Column /= true,
                                      Column /= false ->
  to_sql(P0, {identifier_chain, Column});
to_sql(P0, {identifier_chain, Id}) ->
  %% TODO: Check identifier chain is wellformed
  Identifier_Chain = atom_to_binary(Id, utf8),
  {P0, {Identifier_Chain, []}};
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
derived_col_to_sql({count, all}) ->
  ["COUNT(*)"];
derived_col_to_sql({count, {distinct, Column}}) ->
  ["COUNT(DISTINCT ", name_to_sql(Column), ")"];
derived_col_to_sql({count, Column}) ->
  ["COUNT(", name_to_sql(Column), ")"];
derived_col_to_sql({Column, Alias}) ->
  [name_to_sql(Column), " AS ", name_to_sql(Alias)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Serialize an identifier
-spec name_to_sql(identifier_chain() | id()) -> iodata().
name_to_sql(Name) ->
  Identifier = atom_to_binary(Name, utf8),
  %% Regex = "^[a-zA-Z][a-zA-Z0-9_]*[\.a-zA-Z0-9_]*$|^[a-zA-Z][a-zA-Z0-9_]*$|^\"[a-zA-Z][a-zA-Z0-9_]*\"$",
  %% {ok, MP} = re:compile(Regex),
  %% case re:run(Identifier, MP) of
  %%   {match, [{0,0}]} ->
  %%     throw(non_valid_identifier);
  %%   {match, _Captured} ->
  %%     Identifier;
  %%   nomatch ->
  %%     throw(non_valid_identifier)
  %% end.
  Identifier.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns placeholder according to position
-spec get_placeholder(Position :: integer()) -> string().
get_placeholder(Position) when is_integer(Position) ->
  "$" ++ integer_to_list(Position).
