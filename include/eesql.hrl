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
     dup = all :: sql:dup(),
     columns = [] :: list(sql:column()), %% List of columns to retrieve, * as []
     from :: nonempty_list(sql:table()), %% (FROM) Source of data, table names for the moment
     join = [] :: list(sql:join_cond()), %% (JOIN) join_condition
     where = [] :: list(sql:predicate()), %% (WHERE) search_condition
     group_by = [] :: list(sql:group_by_expr()), %% (GROUP BY) group_by_expression
     having = [] :: list(sql:predicate()), %% (HAVING) search_condition
     order_by = [] :: list(sql:order_by_expr()) %% (ORDER BY) order_expression [ASC | DESC]
   }
  ).

-record(
   insert,
   {
     table :: sql:table(),
     columns :: nonempty_list(sql:column()), %% List of columns to set
     values :: nonempty_list(nonempty_list(sql:value())) %% (VALUES) values to insert
   }
  ).

-record(
   update,
   {
     table :: sql:table(),
     set :: nonempty_list({sql:name(), sql:expr()}), %% (SET) columns to update
     where = [] :: list(sql:predicate()) %% (WHERE) search_condition
   }
  ).

-record(
   delete,
   {
     from :: sql:table(), %% (FROM) Source of data, table names for the moment
     where = [] :: list(sql:predicate()) %% (WHERE) search_condition
   }
  ).
