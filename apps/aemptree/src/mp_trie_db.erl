%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%
%%% @doc
%%%  Defines a general key-value storage to back and persist
%%%  out Merkle Patricia Trie. This is generally LevelDB in the
%%%  community, but for testing, we'll generally use `:ets`.
%%%
%%%  We define a callback that can be implemented by a number
%%%  of potential backends.
%%% @end
%%%-------------------------------------------------------------------
-module(mp_trie_db).

-export([get/2]).

-define(KEY_NOT_FOUND_ERROR(Key), {error, lists:flatten("cannot find key ~p", [Key])}).
-type db_mod() :: module().
-type db_name() :: atom().
-type db_ref() :: atom().
-type db() :: {db_mod(), db_ref()}.
-type value() :: binary().

-export_type([db_name/0,
              db_ref/0,
              db/0,
              value/0]).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================
-callback init(db_name()) -> db().
-callback get(db_ref(), mp_trie:key()) -> {'ok', value()} | {'error', 'not_found'}.
-callback put(db_ref(), mp_trie:key(), value()) -> 'ok'.


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%   Retrieves a key from the database.
%%
%%   Examples TODO: FIX THIS
%%
%%       iex> db = MerklePatriciaTree.Test.random_ets_db()
%%       iex> MerklePatriciaTree.DB.get(db, "name")
%%       :not_found
%%
%%       iex> db = MerklePatriciaTree.Test.random_ets_db()
%%       iex> MerklePatriciaTree.DB.put!(db, "name", "bob")
%%       iex> MerklePatriciaTree.DB.get(db, "name")
%%       {:ok, "bob"}
%% @end
%%------------------------------------------------------------------------------
-spec get(db(), mp_trie:key()) -> {'ok', value()} | {'error', 'not_found'}.
get(_Db = {DBMod, DbRef}, Key) ->
    DbMod:get(DbRef, Key).

%%------------------------------------------------------------------------------
%% @doc
%%   Retrieves a key from the database, but raises if that key does not exist.
%%
%%   Examples
%%
%%       iex> db = MerklePatriciaTree.Test.random_ets_db()
%%       iex> MerklePatriciaTree.DB.get!(db, "name")
%%       ** (MerklePatriciaTree.DB.KeyNotFoundError) cannot find key `name`
%%
%%       iex> db = MerklePatriciaTree.Test.random_ets_db()
%%       iex> MerklePatriciaTree.DB.put!(db, "name", "bob")
%%       iex> MerklePatriciaTree.DB.get!(db, "name")
%%       "bob"
%% @end
%%------------------------------------------------------------------------------
-spec get_value(db(), mp_trie:key()) -> value().
get_value(Db, Key) ->
    case get(Db, Key) of
        {ok, Value} -> Value;
        {error, not_found} -> throw(?KEY_NOT_FOUND_ERROR(Key))
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Stores a key in the database.
%%
%%   Examples
%%
%%       iex> db = MerklePatriciaTree.Test.random_ets_db()
%%       iex> MerklePatriciaTree.DB.put!(db, "name", "bob")
%%       iex> MerklePatriciaTree.DB.get(db, "name")
%%       {:ok, "bob"}
%%       iex> MerklePatriciaTree.DB.put!(db, "name", "tom")
%%       iex> MerklePatriciaTree.DB.get(db, "name")
%%       {:ok, "tom"}
%% @end
%%------------------------------------------------------------------------------
-spec put(db(), mp_trie:key(), value()) -> 'ok'.
  def put!(_db={db_mod, db_ref}, key, value) do
    db_mod.put!(db_ref, key, value)
  end

end
