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

select_true_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = true
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE TRUE;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_false_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = false
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE FALSE;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_and_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'and', [true,
                                                          {'users.id', '=', 'emails.id'},
                                                          {'emails.valid', '=', true},
                                                          {'emails.id', like, <<"a.*">>}]}
                                        }),
  ?assertEqual([<<"a.*">>], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE TRUE AND users.id = emails.id AND emails.valid = TRUE AND emails.id LIKE $1;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_order_by_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'users.id','=','emails.id'},
                                         order_by = ['users.name']
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id ORDER BY users.name;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_order_by_asc_last_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'users.id','=','emails.id'},
                                         order_by = [{'users.name', asc, last}]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id ORDER BY users.name ASC NULLS LAST;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_order_by_desc_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'users.id','=','emails.id'},
                                         order_by = [{'users.name', desc}]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id ORDER BY users.name DESC;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_order_by_first_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'users.id','=','emails.id'},
                                         order_by = [{'users.name', first}]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id ORDER BY users.name NULLS FIRST;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_order_by_2_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'users.id','=','emails.id'},
                                         order_by = [{'users.name', first},
                                                     {'emails.id', asc}]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id ORDER BY users.name NULLS FIRST, emails.id ASC;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_offset_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns=['users.name','emails.address'],
                                         from = [users,emails],
                                         where = {'users.id','=','emails.id'},
                                         order_by = [{'users.name', first},
                                                     {'emails.id', asc}],
                                         offset = {120, 20}
                                        }),
  ?assertEqual([120, 20], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE users.id = emails.id ORDER BY users.name NULLS FIRST, emails.id ASC OFFSET ($1) FETCH NEXT ($2) ROWS ONLY;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_gt_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         from = [users],
                                         where= {created, '>', 1459286860}
                                        }),
  ?assertEqual([1459286860], Params),
  ?assertEqual("SELECT ALL * FROM users WHERE created > $1;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_function_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         from = [users],
                                         where= {created, '>', {<<"POWER">>, [1459286860,1]}}
                                        }),
  ?assertEqual([1459286860,1], Params),
  ?assertEqual("SELECT ALL * FROM users WHERE created > POWER($1, $2);",
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

insert_conflict1_test() ->
  {Insert_AST, Params} = eesql:to_sql(#insert{table = preuser,
                                              columns = [created, expired, fired, id, user_id],
                                              values = [[11234, null, null, 1, <<"email@example.com">>]],
                                              on_conflict_update_target = [id]}),
  ?assertEqual([11234, 1, <<"email@example.com">>], Params),
  ?assertEqual("INSERT INTO preuser (created, expired, fired, id, user_id) VALUES ($1, NULL, NULL, $2, $3) ON CONFLICT (id) DO UPDATE SET created = EXCLUDED.created, expired = EXCLUDED.expired, fired = EXCLUDED.fired, user_id = EXCLUDED.user_id RETURNING *;",
               lists:flatten(io_lib:format("~s",[Insert_AST]))).

insert_conflict2_test() ->
  {Insert_AST, Params} = eesql:to_sql(#insert{table = hlr_lookups_networks,
                                              columns = [mcc, mnc, originalnetworkname],
                                              values = [[<<"214">>, <<"01">>, <<"Vodafone">>]],
                                              on_conflict_update_target = [mcc, mnc]}),
  ?assertEqual([<<"214">>, <<"01">>, <<"Vodafone">>], Params),
  ?assertEqual("INSERT INTO hlr_lookups_networks (mcc, mnc, originalnetworkname) VALUES ($1, $2, $3) ON CONFLICT (mcc, mnc) DO UPDATE SET originalnetworkname = EXCLUDED.originalnetworkname RETURNING *;",
               lists:flatten(io_lib:format("~s",[Insert_AST]))).

insert_conflict3_test() ->
  {Insert_AST, Params} = eesql:to_sql(#insert{table = hlr_lookups_networks,
                                              columns = [mcc, mnc, originalnetworkname],
                                              values = [[<<"214">>, <<"01">>, <<"Vodafone">>]],
                                              on_conflict_update_target = []}),
  ?assertEqual([<<"214">>, <<"01">>, <<"Vodafone">>], Params),
  ?assertEqual("INSERT INTO hlr_lookups_networks (mcc, mnc, originalnetworkname) VALUES ($1, $2, $3) ON CONFLICT DO NOTHING RETURNING *;",
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

truncate_test() ->
  {Truncate_AST, Params} = eesql:to_sql(#truncate{table = sims}),
  ?assertEqual([], Params),
  ?assertEqual("TRUNCATE sims;",
               lists:flatten(io_lib:format("~s",[Truncate_AST]))).

truncate2_test() ->
  {Truncate_AST, Params} = eesql:to_sql(#truncate{table = sims, cascade = true}),
  ?assertEqual([], Params),
  ?assertEqual("TRUNCATE sims CASCADE;",
               lists:flatten(io_lib:format("~s",[Truncate_AST]))).
