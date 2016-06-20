-module(eesql_tests).

-include_lib("include/eesql.hrl").
-include_lib("eunit/include/eunit.hrl").

select1_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{from = [users]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM users;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select2_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{columns=[username, name],from = [users]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL username, name FROM users;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_join_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [#join{type= inner,
                                                       left = users,
                                                       right = emails,
                                                       spec = {'users.id','=','emails.id'}}]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users INNER JOIN emails ON users.id = emails.id;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_theta_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where= {'users.id','=','emails.id'}
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_gt_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         from = [users],
                                         where= {created, '>', 1459286860}
                                        }),
  ?assertEqual([1459286860], Params),
  ?assertEqual("SELECT ALL * FROM users WHERE created > $1;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

delete_test() ->
  {Delete_AST, Params} = eesql:to_sql(#delete{table = preuser}),
  ?assertEqual([], Params),
  ?assertEqual("DELETE FROM preuser RETURNING *;",
               lists:flatten(io_lib:format("~s",[Delete_AST]))).

insert1_test() ->
  {Insert_AST, Params} = eesql:to_sql(#insert{table = preuser,
                                              columns = [c1, c2],
                                              values = [[<<"a">>,21],[<<"b">>,42]]}),
  ?assertEqual([<<"a">>,21,<<"b">>,42], Params),
  ?assertEqual("INSERT INTO preuser (c1, c2) VALUES ($1, $2), ($3, $4) RETURNING *;",
               lists:flatten(io_lib:format("~s",[Insert_AST]))).

insert2_test() ->
  {Insert_AST, Params} = eesql:to_sql(#insert{table = preuser,
                                              columns = [created, expired, fired, id, user_id],
                                              values = [[11234, null, null, 1, <<"email@example.com">>]]}),
  ?assertEqual([11234, 1, <<"email@example.com">>], Params),
  ?assertEqual("INSERT INTO preuser (created, expired, fired, id, user_id) VALUES ($1, NULL, NULL, $2, $3) RETURNING *;",
               lists:flatten(io_lib:format("~s",[Insert_AST]))).

update_test() ->
  {Update_AST, Params} = eesql:to_sql(#update{table = preuser,
                                              set = [{a,21}]}),
  ?assertEqual([21], Params),
  ?assertEqual("UPDATE preuser SET a = $1 RETURNING *;",
               lists:flatten(io_lib:format("~s",[Update_AST]))).

start_trans_test() ->
  {Start_Trans_AST, Params} = eesql:to_sql(start_transaction),
  ?assertEqual([], Params),
  ?assertEqual("BEGIN TRANSACTION;",
               lists:flatten(io_lib:format("~s",[Start_Trans_AST]))).

commit_test() ->
  {Commit_AST, Params} = eesql:to_sql(commit),
  ?assertEqual([], Params),
  ?assertEqual("COMMIT;",
               lists:flatten(io_lib:format("~s",[Commit_AST]))).

rollback_test() ->
  {Rollback_AST, Params} = eesql:to_sql(rollback),
  ?assertEqual([], Params),
  ?assertEqual("ROLLBACK;",
               lists:flatten(io_lib:format("~s",[Rollback_AST]))).
