%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    An Erlang implementation of
%%%    https://github.com/exthereum/merkle_patricia_tree
%%%    Proof handling from
%%%    https://github.com/ethereum/cpp-ethereum/blob/develop/libdevcore/TrieDB.h
%%% @end
%%%-------------------------------------------------------------------
-module(mp_trie).

-export([new/2,
         get/2,
         update/3,
         store/1]).

%% TODO:
%% {deps, [
%%   {keccakf1600, ".*", {git, "git://github.com/potatosalad/erlang-keccakf1600.git", {branch, "master"}}}
%% ]}.

-record(trie, {db :: db:db(),
               root_hash = <<>> :: root_hash()}).

-define(EMPTY_TRIE, <<>>).
-define(EMPTY_TRIE_ROOT_HASH, keccakf1600:hash(sha3_256, rlp:encode(?EMPTY_TRIE))).


-type root_hash() :: binary().
-type key() :: binary().
-type value() :: binary() | 'nil'.
-type result(T) :: {'ok', T} | {'error', term()}.
-export_type([root_hash/0,
              key/0,
              value/0,
              result/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%  Contructs a new unitialized trie.
%%
%%  ## Examples
%%
%%    iex> MerklePatriciaTree.Trie.new(MerklePatriciaTree.Test.random_ets_db(:trie_test_1))
%%    %MerklePatriciaTree.Trie{db: {MerklePatriciaTree.DB.ETS, :trie_test_1}, root_hash: <<86, 232, 31, 23, 27, 204, 85, 166, 255, 131, 69, 230, 146, 192, 248, 110, 91, 72, 224, 27, 153, 108, 173, 192, 1, 98, 47, 181, 227, 99, 180, 33>>}
%%
%%    iex> MerklePatriciaTree.Trie.new(MerklePatriciaTree.Test.random_ets_db(:trie_test_2), <<1, 2, 3>>)
%%    %MerklePatriciaTree.Trie{db: {MerklePatriciaTree.DB.ETS, :trie_test_2}, root_hash: <<241, 136, 94, 218, 84, 183, 160, 83, 49, 140, 212, 30, 32, 147, 34, 13, 171, 21, 214, 83, 129, 177, 21, 122, 54, 51, 168, 59, 253, 92, 146, 57>>}
%%
%%    iex> trie = MerklePatriciaTree.Trie.new(MerklePatriciaTree.DB.LevelDB.init("/tmp/#{MerklePatriciaTree.Test.random_string(20)}"), <<1, 2, 3>>)
%%    iex> trie.root_hash
%%    <<241, 136, 94, 218, 84, 183, 160, 83, 49, 140, 212, 30, 32, 147, 34,
%%      13, 171, 21, 214, 83, 129, 177, 21, 122, 54, 51, 168, 59, 253, 92,
%%      146, 57>>
%%    iex> {db, _db_ref} = trie.db
%%    iex> db
%%    MerklePatriciaTree.DB.LevelDB
%% @end
%%------------------------------------------------------------------------------
-spec new(db:db(), root_hash()) -> #trie{}.
new(Db = {_, _}, RootHash) ->
    store(#trie{db: db, root_hash: root_hash}).

%%------------------------------------------------------------------------------
%% @doc
%%   Given a trie, returns the value associated with key.
%% @end
%%------------------------------------------------------------------------------
-spec get(#trie{}, key()) -> binary() | 'nil'.
get(Trie, Key) ->
    do_get(Trie, trie_helper:get_nibbles(Key)).

%%------------------------------------------------------------------------------
%% @doc
%%   Updates a Trie by setting Key equal to Value. If Value is nil,
%%   we will instead remove `Key` from the trie.
%% @end
%%------------------------------------------------------------------------------
-spec update(#trie{}, key(0, rlp:rlp() | 'nil') -> #trie{}.
update(Trie, Key, nil) ->
    DecTrie = mp_trie_node:decode_trie(Trie),
    DecTrie2 = mp_trie_destroyer:remove_key(DecTrie,
                                            mp_trie_helper:get_nibbles(Key), Trie),
    Trie2 = mp_trie_node:encode_node(DecTrie2, Trie),
    store(into(Trie2, Trie));
update(Trie, Key, Value) ->
    %% We're going to recursively walk toward our key,
    %% then we'll add our value (either a new leaf or the value
    %% on a branch node), then we'll walk back up the tree and
    %% update all previous ndes. This may require changing the
    %% type of the node.
    DecTrie = mp_trie_node:decode_trie(Trie),
    DecTrie2 = mp_trie_builder:put_key(
                 DecTrie, mp_trie_helper:get_nibbles(Key), Value, Trie),
    Trie2 = mp_trie_node:encode_node(DecTrie2, Trie),
    store(into(Trie2, Trie)).

store(#trie{} = Trie) ->
    RootHash = case Trie#trie.root_hash of
                   <<>> ->
                       rlp:encode(<<>>);
                   RH when is_binary(RH) ->
                       RH;
                   RH ->
                       rlp:encode(RH)
               end,
    case size(RootHash) < mp_trie_storage:max_rlp_len() of
        true ->
            mp_trie_storage:store(Trie#{root_hash = RootHash}, Trie#trie.db);
        false ->
            Trie
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%   Returns the canonical empty trie.
%%
%%   Note: this root hash will not be accessible unless you have stored
%%   the result in a db. If you are initializing a new trie, instead of
%%   checking a result is empty, it's strongly recommended you use
%%   `Trie.new(db).root_hash`.
%%
%%   ## Examples
%%
%%       iex> MerklePatriciaTree.Trie.empty_trie_root_hash()
%%       <<86, 232, 31, 23, 27, 204, 85, 166, 255, 131, 69, 230, 146, 192, 248, 110, 91, 72, 224, 27, 153, 108, 173, 192, 1, 98, 47, 181, 227, 99, 180, 33>>
%% @end
%%------------------------------------------------------------------------------
-spec empty_trie_root_hash() -> root_hash().
empty_trie_root_hash() ->
    ?EMPTY_TRIE_ROOT_HASH.

%%------------------------------------------------------------------------------
%% @doc
%%   Moves trie down to be rooted at `next_node`,
%%   this is effectively (and literally) just changing
%%   the root_hash to `node_hash`.
%% @end
%%------------------------------------------------------------------------------
-spec into(root_hash(), #trie{}) -> #trie{}.
into(NextNodeHash, Trie) ->
    Trie#{root_hash = NextNodeHash}.

-spec do_get(#trie{} | 'nil', [integer()]) -> binary() | 'nil'.
do_get(nil, _) ->
    nil;
do_get(Trie, [Nibble | Rest] = Nibbles) ->
    %% Let's decode `c(I, i)`
    case mp_trie_node:decode_trie(Trie) of
      empty ->
            %% no node, bail out
            nil;
      {branch, Branches} ->
        %% branch node
        case lists:nth(Nibble, Branches) of
            []       -> nil;
            do_get(into(NodeHash, Trie), Rest)
        end;
      {leaf, Prefix, Value} ->
        %% leaf, Value is second value if match first
        case Nibbles of
            Prefix -> Value;
            _ -> nil
        end;
      {ext, SharedPrefix, NextNode} ->
        %% extension, continue walking tree if we match
        case list_helper:get_postfix(Nibbles, SharedPrefix) of
             %% did not match extension node
            nil -> nil;
            Rest -> do_get(into(NextNode, Trie), Rest)
        end
    end;
do_get(Trie, []) ->
    %% Only branch nodes can have values for a nil lookup
    case mp_trie_node:decode_trie(Trie) of
      {branch, Branches} -> lists.last(Branches);
      {leaf, [], V} -> V;
      _ -> nil
    end.
