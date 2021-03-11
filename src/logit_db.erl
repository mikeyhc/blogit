-module(logit_db).

-export([install/1, wait_for_tables/0]).
-export([add_entry/3, latest_entry/0, next_entry/1, prev_entry/1, entry/1]).

-record(logit_entry, {date :: calendar:date(),
                      author :: binary(),
                      content :: binary()
                     }).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(logit_entry,
                        [{attributes, record_info(fields, logit_entry)},
                         {index, [#logit_entry.author]},
                         {disc_copies, Nodes},
                         {type, bag}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

wait_for_tables() ->
    mnesia:wait_for_tables([logit_entry], 5000).

add_entry(Date, Author, Content) ->
    F = fun() ->
        mnesia:write(#logit_entry{date=Date,
                                  author=Author,
                                  content=Content
                                 })
    end,
    mnesia:activity(transaction, F).

latest_entry() ->
    SelectAll = fun() ->
        mnesia:foldl(fun(X, Acc) -> [X|Acc] end, [], logit_entry)
    end,
    Entries = mnesia:activity(transaction, SelectAll),
    [First|_] = lists:reverse(lists:keysort(#logit_entry.date, Entries)),
    logit_entry_to_dict(First).

entry(Date) ->
    Select = fun() -> mnesia:read({logit_entry, Date}) end,
    case mnesia:activity(transaction, Select) of
        [E] -> {ok, logit_entry_to_dict(E)};
        [_|_] -> {error, multiple_entries};
        [] -> {error, no_entry}
    end.


logit_entry_to_dict(E) ->
    #{date => E#logit_entry.date,
      author => E#logit_entry.author,
      content => E#logit_entry.content
     }.

prev_entry(#{date := RootDate}) ->
    RootDays = calendar:date_to_gregorian_days(RootDate),
    DateDiff = fun(D) ->
        Diff = 0 - (calendar:date_to_gregorian_days(D) - RootDays),
        if Diff =< 0 -> undefined;
           true -> Diff
        end
    end,
    adjacent_entry(DateDiff).

next_entry(#{date := RootDate}) ->
    RootDays = calendar:date_to_gregorian_days(RootDate),
    DateDiff = fun(D) ->
        Diff = calendar:date_to_gregorian_days(D) - RootDays,
        if Diff =< 0 -> undefined;
           true -> Diff
        end
    end,
    adjacent_entry(DateDiff).

adjacent_entry(DiffFn) ->
    ClosestDate = fun(D=#logit_entry{date=Date}, undefined) ->
        Days = DiffFn(Date),
        if Days =:= undefined -> undefined;
           true -> {D, Days}
        end;
    (D=#logit_entry{date=Date}, Acc={_, Days0}) ->
        Days1 = DiffFn(Date),
        if Days1 =:= undefined -> Acc;
           Days1 < Days0 -> {D, Days1};
           true -> Acc
        end
    end,
    SelectNext = fun() ->
        case mnesia:foldl(ClosestDate, undefined, logit_entry) of
            undefined -> undefined;
            {R, _} -> R
        end
    end,
    Entry = mnesia:activity(transaction, SelectNext),
    if Entry =:= undefined -> undefined;
       true -> logit_entry_to_dict(Entry)
    end.
