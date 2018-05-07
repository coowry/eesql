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
                                         from = [#join{type = no_join,
                                                       table = users,
                                                       joins = [#join{type = inner,
                                                                      table = emails,
                                                                      spec = {'users.id','=','emails.id'}}]}]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name, emails.address FROM users INNER JOIN emails ON users.id = emails.id;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_column_as_alias_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{columns = [{'users.name', name},
                                                         {'users.email', email}],
                                              from = [users]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL users.name AS name, users.email AS email FROM users;",
               lists:flatten(io_lib:format("~s", [Select_AST]))).

select_join2_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns = ['sim.id','users.name'],
                                         from = [#join{type = no_join,
                                                       table = {sims, sim},
                                                       joins = [#join{type = left,
                                                                      table = sim_owners,
                                                                      spec = {'sim_owners.id','=','sim.id'}},
                                                                #join{type = left,
                                                                      table = users,
                                                                      spec = {'users.id','=','sim_owners.owner_id'}}]
                                                      }],
                                         where = {'not', {is_null,'sim.secret'}}}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL sim.id, users.name FROM sims AS sim "
               "LEFT OUTER JOIN sim_owners ON sim_owners.id = sim.id "
               "LEFT OUTER JOIN users ON users.id = sim_owners.owner_id "
               "WHERE NOT (sim.secret IS NULL);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_join3_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns = ['sim.id','users.name'],
                                         from = [#join{type = no_join,
                                                       table = {#select{from = [sims], where = {'sims.net', '=', <<"21407">>}},
                                                                sim},
                                                       joins = [#join{type = left,
                                                                      table = sim_owners,
                                                                      spec = {'sim_owners.id','=','sim.id'}},
                                                                #join{type = left,
                                                                      table = users,
                                                                      spec = {'users.id','=','sim_owners.owner_id'}}]
                                                      }],
                                         where = {'not', {is_null,'sim.secret'}}}),
  ?assertEqual([<<"21407">>], Params),
  ?assertEqual("SELECT ALL sim.id, users.name FROM (SELECT ALL * FROM sims WHERE sims.net = $1) AS sim "
               "LEFT OUTER JOIN sim_owners ON sim_owners.id = sim.id "
               "LEFT OUTER JOIN users ON users.id = sim_owners.owner_id "
               "WHERE NOT (sim.secret IS NULL);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_cross_join_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         from = [#cross_join{
                                                    left = countries,
                                                    right = languages
                                                   }]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM (countries CROSS JOIN languages);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_natural_join_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         from = [#natural_join{
                                                    left = countries,
                                                    right = languages
                                                   }]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM (countries NATURAL JOIN languages);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_union_join_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         from = [#union_join{
                                                    left = countries,
                                                    right = languages
                                                   }]
                                        }),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM (countries UNION JOIN languages);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_rightjoin_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns = ['sim.id','users.name'],
                                         from = [#qualified_join{
                                                    type = right,
                                                    left = #qualified_join{
                                                              type = left,
                                                              left = {#select{from=[sims],
                                                                              where = {'sims.operator', '=', <<"op-00c9">>}},
                                                                      sim},
                                                              right = sim_owners,
                                                              on = {'sim_owners.id','=','sim.id'}},
                                                    right = users,
                                                    on = {'users.id','=','sim_owners.owner_id'}}],
                                         where = {'not', {is_null,'sim.secret'}}}),
  ?assertEqual([<<"op-00c9">>], Params),
  ?assertEqual("SELECT ALL sim.id, users.name "
               "FROM (((SELECT ALL * FROM sims WHERE sims.operator = $1) AS sim "
               "LEFT OUTER JOIN sim_owners ON sim_owners.id = sim.id) "
               "RIGHT OUTER JOIN users ON users.id = sim_owners.owner_id) "
               "WHERE NOT (sim.secret IS NULL);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

%% TODO (AH): this test fails, not clear what's the associativity and precedence of joins are.
select_rightjoin2_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns = ['sim.id','users.name'],
                                         from = [#qualified_join{
                                                    type = right,
                                                    left = users,
                                                    right = #qualified_join{
                                                               type = left,
                                                               left = {#select{from=[sims],
                                                                               where = {'sims.operator', '=', <<"op-00c9">>}},
                                                                       sim},
                                                               right = sim_owners,
                                                               on = {'sim_owners.id','=','sim.id'}},
                                                    on = {'users.id','=','sim_owners.owner_id'}}],
                                         where = {'not', {is_null,'users.secret'}}}),
  ?assertEqual([<<"op-00c9">>], Params),
  ?assertEqual("SELECT ALL sim.id, users.name FROM (users RIGHT OUTER JOIN ((SELECT ALL * FROM sims WHERE sims.operator = $1) AS sim LEFT OUTER JOIN sim_owners ON sim_owners.id = sim.id) ON users.id = sim_owners.owner_id) WHERE NOT (users.secret IS NULL);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_leftjoin_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns = ['sim.id','users.name'],
                                         from = [#qualified_join{
                                                    type = left,
                                                    left = #qualified_join{
                                                              type = left,
                                                              left = {#select{from=[sims],
                                                                              where = {'sims.operator', '=', <<"op-00c9">>}},
                                                                      sim},
                                                              right = sim_owners,
                                                              on = {'sim_owners.id','=','sim.id'}},
                                                    right = users,
                                                    on = {'users.id','=','sim_owners.owner_id'}}],
                                         where = {'not', {is_null,'sim.secret'}}}),
  ?assertEqual([<<"op-00c9">>], Params),
  ?assertEqual("SELECT ALL sim.id, users.name "
               "FROM (((SELECT ALL * FROM sims WHERE sims.operator = $1) AS sim "
               "LEFT OUTER JOIN sim_owners ON sim_owners.id = sim.id) "
               "LEFT OUTER JOIN users ON users.id = sim_owners.owner_id) "
               "WHERE NOT (sim.secret IS NULL);",
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
  ?assertEqual("SELECT ALL users.name, emails.address FROM users, emails WHERE (((TRUE AND users.id = emails.id) AND emails.valid = TRUE) AND emails.id LIKE $1);",
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
                                         where= {created, '>', {'POWER', [1459286860,1]}}
                                        }),
  ?assertEqual([1459286860,1], Params),
  ?assertEqual("SELECT ALL * FROM users WHERE created > POWER($1, $2);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_cast_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{
                                         columns = [id, {cast, [value, bigint]}],
                                         from = [users],
                                         where= {created, '>', {'POWER', [1459286860,1]}}
                                        }),
  ?assertEqual([1459286860,1], Params),
  ?assertEqual("SELECT ALL id, CAST(value AS bigint) FROM users WHERE created > POWER($1, $2);",
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

group_by_test() ->
  {Group_AST, Params} = eesql:to_sql(#select{from = [trades], group_by = [sort]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM trades GROUP BY sort;",
               lists:flatten(io_lib:format("~s",[Group_AST]))).

group_by2_test() ->
  {Group_AST, Params} = eesql:to_sql(#select{from = [trades], group_by = [sort], order_by = [sort]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM trades GROUP BY sort ORDER BY sort;",
               lists:flatten(io_lib:format("~s",[Group_AST]))).

count_all_test() ->
  {Count_AST, Params} = eesql:to_sql(#select{from = [trades], columns = [{count, all}]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL COUNT(*) FROM trades;",
               lists:flatten(io_lib:format("~s",[Count_AST]))).

count_test() ->
  {Count_AST, Params} = eesql:to_sql(#select{from = [trades], columns = [{count, 'trades.id'}], where = {sort, '=', <<"p2p">>}}),
  ?assertEqual([<<"p2p">>], Params),
  ?assertEqual("SELECT ALL COUNT(trades.id) FROM trades WHERE sort = $1;",
               lists:flatten(io_lib:format("~s",[Count_AST]))).

count_literal_test() ->
  {Count_AST, Params} = eesql:to_sql(#select{from = [trades], columns = [{count, 1}], where = {sort, '=', <<"p2p">>}}),
  ?assertEqual([1,<<"p2p">>], Params),
  ?assertEqual("SELECT ALL COUNT($1) FROM trades WHERE sort = $2;",
               lists:flatten(io_lib:format("~s",[Count_AST]))).

sum_test() ->
  {Sum_AST, Params} = eesql:to_sql(#select{from = [trades], columns = [{sum, ['trades.value']}], where = {sort, '=', <<"p2p">>}}),
  ?assertEqual([<<"p2p">>], Params),
  ?assertEqual("SELECT ALL sum(trades.value) FROM trades WHERE sort = $1;",
               lists:flatten(io_lib:format("~s",[Sum_AST]))).

sum_as_test() ->
  {Sum_AST, Params} = eesql:to_sql(#select{from = [trades], columns = [{{sum, ['trades.value']}, total_value}], where = {sort, '=', <<"p2p">>}}),
  ?assertEqual([<<"p2p">>], Params),
  ?assertEqual("SELECT ALL sum(trades.value) AS total_value FROM trades WHERE sort = $1;",
               lists:flatten(io_lib:format("~s",[Sum_AST]))).

distinct_count_test() ->
  {Count_AST, Params} = eesql:to_sql(#select{from = [trades], columns = [{count, {distinct, 'trades.sender'}}], where = {sort, '=', <<"p2p">>}}),
  ?assertEqual([<<"p2p">>], Params),
  ?assertEqual("SELECT ALL COUNT(DISTINCT trades.sender) FROM trades WHERE sort = $1;",
               lists:flatten(io_lib:format("~s",[Count_AST]))).

select_in_test() ->
  Bulks = #select{from = [bulks], columns = [id]},
  {Select_AST, Params} = eesql:to_sql(#select{from = [trades], where = {in, bkid, Bulks}}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM trades WHERE bkid IN (SELECT ALL id FROM bulks);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_for_update_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{from = [trades], 
                                              for_update = true, 
                                              where = {id, '=', <<"tr-123456789">>}}),
  ?assertEqual([<<"tr-123456789">>], Params),
  ?assertEqual("SELECT ALL * FROM trades WHERE id = $1 FOR UPDATE;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

not_test() ->
  {Select_AST, Params} = eesql:to_sql(#select{from = [trades],
                                              where = {'and',[{'not',{is_null,authorized}},
                                                              {'not',{is_null,created}}]}}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM trades WHERE (NOT (authorized IS NULL) AND NOT (created IS NULL));",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_from_select_test() ->
  Bulks = #select{from = [bulks], columns = [id]},
  {Select_AST, Params} = eesql:to_sql(#select{from = [{Bulks, alias}]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM (SELECT ALL id FROM bulks) AS alias;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

select_from_function_call_test() ->
  Function = #pg_call{name = total_balance , args = [<<"agid">>]},
  {Select_AST, Params} = eesql:to_sql(#select{from = [{Function, balance}]}),
  ?assertEqual([<<"agid">>], Params),
  ?assertEqual("SELECT ALL * FROM total_balance($1) AS balance;",
              lists:flatten(io_lib:format("~s", [Select_AST]))).

select_from_function_call_no_args_test() ->
  Function = #pg_call{name = user_info, args = []},
  {Select_AST, Params} = eesql:to_sql(#select{from = [Function]}),
  ?assertEqual([], Params),
  ?assertEqual("SELECT ALL * FROM user_info();",
               lists:flatten(io_lib:format("~s", [Select_AST]))).

select_from_function_call_more_args_test() ->
  Function = #pg_call{name = all_transactions, args = [725846400, 1519220895]},
  {Select_AST, Params} = eesql:to_sql(#select{columns = [trades],
                                              from = [Function]}),
  ?assertEqual([725846400,1519220895], Params),
  ?assertEqual("SELECT ALL trades FROM all_transactions($1, $2);",
               lists:flatten(io_lib:format("~s", [Select_AST]))).

select_from_function_call_fun_argument_test() ->
  Function  = #pg_call{name = products,
                       args = [{country, [<<"ES">>]}]},
  {Select_AST, Params} = eesql:to_sql(#select{from = [Function]}),
  ?assertEqual([<<"ES">>], Params),
  ?assertEqual("SELECT ALL * FROM products(country($1));",
              lists:flatten(io_lib:format("~s", [Select_AST]))).

union_test() ->
  S1 = #select{from = [trades], columns = [{count, {distinct, 'trades.sender'}}], where = {sort, '=', <<"p2p">>}},
  S2 = #select{from = [trades], group_by = [sort]},
  S3 = #select{columns=[username, name], from = [users], where = {name, '<=', <<"some_name">>}},
  {Select_AST, Params} = eesql:to_sql(#union{queries = [S1, S2, S3]}),
  ?assertEqual([<<"p2p">>, <<"some_name">>], Params),
  ?assertEqual("(SELECT ALL COUNT(DISTINCT trades.sender) FROM trades WHERE sort = $1 UNION SELECT ALL * FROM trades GROUP BY sort UNION SELECT ALL username, name FROM users WHERE name <= $2);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

union_all_test() ->
  S1 = #select{from = [trades], columns = [{count, {distinct, 'trades.sender'}}], where = {sort, '=', <<"p2p">>}},
  S2 = #select{from = [trades], group_by = [sort]},
  S3 = #select{columns=[username, name], from = [users], where = {name, '<=', <<"some_name">>}},
  {Select_AST, Params} = eesql:to_sql(#union{type = all, queries = [S1, S2, S3]}),
  ?assertEqual([<<"p2p">>, <<"some_name">>], Params),
  ?assertEqual("(SELECT ALL COUNT(DISTINCT trades.sender) FROM trades WHERE sort = $1 UNION ALL SELECT ALL * FROM trades GROUP BY sort UNION ALL SELECT ALL username, name FROM users WHERE name <= $2);",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

union_all_2_test() ->
  S1 = #select{from = [trades], columns = [{count, {distinct, 'trades.sender'}}], where = {sort, '=', <<"p2p">>}},
  S2 = #select{from = [trades], group_by = [sort]},
  S3 = #select{columns=[username, name], from = [users], where = {name, '<=', <<"some_name">>}},
  {Select_AST, Params} = eesql:to_sql(#union{type = all, queries = [S1, S2, S3], order_by = [{created, desc}, {id, desc}]}),
  ?assertEqual([<<"p2p">>, <<"some_name">>], Params),
  ?assertEqual("(SELECT ALL COUNT(DISTINCT trades.sender) FROM trades WHERE sort = $1 UNION ALL SELECT ALL * FROM trades GROUP BY sort UNION ALL SELECT ALL username, name FROM users WHERE name <= $2) ORDER BY created DESC, id DESC;",
               lists:flatten(io_lib:format("~s",[Select_AST]))).

refresh_test() ->
  {Refresh_AST, Params} = eesql:to_sql(#pg_refresh{materialized_view = my_materialized_view}),
  ?assertEqual([], Params),
  ?assertEqual("REFRESH MATERIALIZED VIEW my_materialized_view;",
               lists:flatten(io_lib:format("~s",[Refresh_AST]))).

with_as_test() ->
  {With_As_AST, Params} = eesql:to_sql(#pg_with{definitions = [{data_for_user, #select{columns = [username, name],
										       from = [users],
										       where = {name, '=', <<"some_name">>}}
							       }],
						select = #select{columns = [country, {count, all}],
								 from = [data_for_user],
								 group_by = [country]}}),
  ?assertEqual([<<"some_name">>], Params),
  ?assertEqual("WITH data_for_user AS (SELECT ALL username, name FROM users WHERE name = $1) SELECT ALL country, COUNT(*) FROM data_for_user GROUP BY country;",	       
lists:flatten(io_lib:format("~s", [With_As_AST]))).

identifier_test() ->
  ?assertEqual(<<"user">>, eesql:identifier_to_sql(user)),
  ?assertEqual(<<"user1">>, eesql:identifier_to_sql(user1)),
  ?assertEqual(<<"_user">>, eesql:identifier_to_sql('_user')),
  ?assertEqual(<<"user_">>, eesql:identifier_to_sql(user_)),
  ?assertEqual(<<"us1er">>, eesql:identifier_to_sql(us1er)),
  ?assertEqual(<<"\"user\"">>, eesql:identifier_to_sql('"user"')),
  ?assertException(throw, {non_valid_identifier,'user.address'}, eesql:identifier_to_sql('user.address')),
  ?assertEqual(<<"user.address">>, eesql:identifier_chain_to_sql('user.address')),
  ?assertEqual(<<"\"user\".address">>, eesql:identifier_chain_to_sql('"user".address')),
  ?assertEqual(<<"user.\"address\"">>, eesql:identifier_chain_to_sql('user."address"')),
  ?assertException(throw, {non_valid_identifier,'user.'}, eesql:identifier_to_sql('user.')).
