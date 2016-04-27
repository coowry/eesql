%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Author: Angel Herranz <aherranz@gmail.com>
%%
%% Copyright 2016, Coowry Ltd. - All rights reserved.
%%
%% See LICENSE.txt file in src directory for detailed information.
%%
%% This HRL contains records to represent PG SQL Abstract Syntax Tree.

-record(
   select,
   {
     quantifier = all :: eesql:set_quant(),
     columns = [] :: list(eesql:derived_column()), %% List of columns to retrieve, * as []
     from :: eesql:from_clause(), %% (FROM) Source of data, table names for the moment
     where :: eesql:predicate() %% (WHERE) search condition
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
     values :: nonempty_list(eesql:row_value_expr()) %% (VALUES) values to insert
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
