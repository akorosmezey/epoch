%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    Implementation of `mp_trie_db`, backed by ets.
%%% @end
%%%-------------------------------------------------------------------
-module(mp_trie_db_ets).
-behaviour(mp_trie_db).


%%%=============================================================================
%%% mp_trie_db behaviour callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%  Performs initialization for this db.
%% @end
%%------------------------------------------------------------------------------
-spec init(mp_trie_db:db_name()) -> mp_trie_db:db().
init(DbName) ->
    ets.new(DbName, [set, public, named_table]),
    {?MODULE, DbName}.

%%------------------------------------------------------------------------------
%% @doc
%%  Retrieves a key from the database.
%% @end
%%------------------------------------------------------------------------------
-spec get(mp_trie_db:db_ref(), mp_trie:key()) -> mp_trie_db:result(mp_trie_db:value()).
get(DbRef, Key) ->
    case ets.lookup(DbRef, Key) of
        [{Key, V} | _Rest] -> {ok, V};
        _ -> {error, not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%  Stores a key in the database.
%% @end
%%------------------------------------------------------------------------------
-spec put(mp_trie_db:db_ref(), mp_trie:key(), mp_trie_db:value()) -> ok.
put(DbRef, Key, Value) ->
    case ets:insert(DbRef, {Key, Value}) of
        true -> ok
    end.
