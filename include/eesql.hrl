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
     dup = all :: eesql:dup(),
     columns = [] :: list(eesql:column()), %% List of columns to retrieve, * as []
     from :: eesql:from_clause(), %% (FROM) Source of data, table names for the moment
     where = [] :: list(eesql:predicate()), %% (WHERE) search_condition
     group_by = [] :: list(eesql:group_by_expr()), %% (GROUP BY) group_by_expression
     having = [] :: list(eesql:predicate()), %% (HAVING) search_condition
     order_by = [] :: list(eesql:order_by_expr()) %% (ORDER BY) order_expression [ASC | DESC]
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
     table :: eesql:table_ref(),
     columns :: nonempty_list(eesql:column()), %% List of columns to set
     values :: nonempty_list(nonempty_list(eesql:value())) %% (VALUES) values to insert
   }
  ).

-record(
   update,
   {
     table :: eesql:table_ref(),
     set :: nonempty_list({eesql:name(), eesql:expr()}), %% (SET) columns to update
     where = [] :: list(eesql:predicate()) %% (WHERE) search_condition
   }
  ).

-record(
   delete,
   {
     from :: eesql:table_ref(), %% (FROM) Source of data, table names for the moment
     where = [] :: list(eesql:predicate()) %% (WHERE) search_condition
   }
  ).
