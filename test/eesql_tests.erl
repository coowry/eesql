-module(eesql_tests).

-include_lib("include/eesql.hrl").
-include_lib("eunit/include/eunit.hrl").

select_test() ->
  Select_AST = eesql:to_sql(#select{columns=[username, name],from = [users]}),
  ?assertEqual("SELECT ALL username, name FROM users ;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).
