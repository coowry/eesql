-module(eesql_tests).

-include_lib("include/eesql.hrl").
-include_lib("eunit/include/eunit.hrl").

select1_test() ->
  Select_AST = eesql:to_sql(#select{from = [users]}),
  ?assertEqual("SELECT ALL * FROM users ;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select2_test() ->
  Select_AST = eesql:to_sql(#select{columns=[username, name],from = [users]}),
  ?assertEqual("SELECT ALL username, name FROM users ;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_theta_test() ->
  Select_AST = eesql:to_sql(#select{
                               columns=['users.name','emails.address'],
                               from = [users,emails],
                               where=[{'users.id','=','emails.id'}]}),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_gt_test() ->
  Select_AST = eesql:to_sql(#select{
                               from = [users],
                               where=[{created, '>', 1459286860}]}),
  ?assertEqual("SELECT ALL * FROM users WHERE created > 1459286860;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

delete_test() ->
  Delete_AST = eesql:to_sql(#delete{from=preuser}),
  ?assertEqual("DELETE FROM preuser RETURNING *;",
               lists:flatten(io_lib:format("~s",[Delete_AST]))).
