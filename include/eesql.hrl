%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Author: Angel Herranz <aherranz@gmail.com>
%%
%% Copyright 2016, Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file in src directory for detailed information.
%%
%% This HRL contains records to represent PG SQL Abstract Syntax
%% Tree. Non-terminal symbols of the grammar are referenced according
%% to http://savage.net.au/SQL/sql-2003-2.bnf.html
-record(
   select,
   {
     quantifier = all :: eesql:set_quant(), %% <set quantifier>
     columns = [] :: list(eesql:derived_column()), %% List of columns to retrieve, * as [] (<select list>)
     %% <table expression> from here
     from :: eesql:from_clause(), %% (FROM) Source of data, table names for the moment (<from clause>)
     where :: eesql:predicate(), %% (WHERE) search condition (<where clause>)
     order_by = [] :: list(eesql:sort_spec()), %% (ORDER BY) Sort specification (<sort specification list>)
     offset :: undefined | {pos_integer(), pos_integer()} %% (OFFSET/FETCH)
   }
  ).

-record(
   join,
   {
     type = inner :: eesql:join_type(),
     left :: eesql:table_ref(),
     right :: eesql:table_ref(),
     spec :: eesql:predicate()
   }
  ).

-record(
   insert,
   {
     table :: eesql:table_name(),
     columns :: nonempty_list(eesql:column_name()), %% List of columns to set
     values :: nonempty_list(eesql:row_value_expr()), %% (VALUES) values to insert
     on_conflict_update_target :: undefined | eesql:column_name() %% UPDATE when the column conflicts
   }
  ).

-record(
   update,
   {
     table :: eesql:table_name(), %% UPDATE target table
     set :: nonempty_list(eesql:set_clause()), %% (SET) columns to update
     where :: eesql:predicate() %% (WHERE) search condition
   }
  ).

-record(
   delete,
   {
     table :: eesql:table_name(), %% (DELETE FROM) target table
     where :: eesql:predicate() %% (WHERE) search_condition
   }
  ).
