# Erlang Embedded SQL library

This library is allow you to represent SQL sentences as Erlang terms.

A pretty print function generates SQL in the form of an Extended
Query: `iodata()` + parameters. The is directly usable in second and
third parameter of `epgsql:equery/3`. The SQL string (`iodata()`) is
directly used in the second argument of `epgsql:parse/2`.

## Usage

Let's write a representation of this SQL query:

```sqp
SELECT users.name, emails.address FROM users INNER JOIN emails ON users.id = emails.id
```

In eesql, the following term represents the previous query:

```erlang
Q = #select{
      columns = ['users.name','emails.address'],
      from = [#join{type = inner,
                    left = users
                    right = emails
                    on = {'users.id','=','emails.id'}}]
}.
```

Now, you can generate the SQL ready for the actual query:

```erlang
1> {S,P} = eesql:to_sql(Q).
{[["SELECT ","ALL"," ",
   [<<"users.name">>,", ",<<"emails.address">>],
   [" FROM ",
    [[<<"users">>,32,
      [["INNER",32,"JOIN ",<<"emails">>,32,"ON",32,
        [<<"users.id">>," ",<<"=">>," ",<<"emails.i"...>>]]]]]],
   [],[],[],[],[]],
  ";"],
 []}
```

Just to check:

```erlang
2> iolist_to_binary(S).
<<"SELECT ALL users.name, emails.address FROM users INNER JOIN emails ON users.id = emails.id;">>
```

You can find more usage examples in `tests/eesql_tests.erl`.
