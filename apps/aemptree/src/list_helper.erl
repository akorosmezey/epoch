%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Helpers for navigating lists, specifically for
%%%   finding shared prefixes and postfixes.
%%% @end
%%%-------------------------------------------------------------------
-module(list_helper).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%  Returns the post of list A if starts with list B, otherwise nil
%%
%%  ## Examples
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([1,2,3], [1,2])
%%    [3]
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([1,2,3,4], [1,2])
%%    [3,4]
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([1,2,3,4], [1])
%%    [2,3,4]
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([1,2,3,4], [0,1])
%%    nil
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([1,2,3,4], [])
%%    [1,2,3,4]
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([1,2], [1,2,3])
%%    nil
%%
%%    iex> MerklePatriciaTree.ListHelper.get_postfix([], [])
%%    []
%% @end
%%------------------------------------------------------------------------------
-spec get_postfix([integer()], [integer()]) -> [integer()] | nil.
get_postfix([H0 | T0], [H1 | T1]) ->
    case H0 of
        H1 ->
            get_postfix(T0, T1);
        _ ->
            nil
    end;
get_postfix(L, []) ->
    L;
get_postfix([], [_|_]) ->
    nil.

%%------------------------------------------------------------------------------
%% @doc
%%  Returns the overlap of two lists in terms of a shared prefix, then the relative postfixes
%%
%%  ## Examples
%%
%%    iex> MerklePatriciaTree.ListHelper.overlap([1,2,3], [1,2])
%%    {[1,2],[3],[]}
%%
%%    iex> MerklePatriciaTree.ListHelper.overlap([1,2,3], [1,2,3,4])
%%    {[1,2,3],[],[4]}
%%
%%    iex> MerklePatriciaTree.ListHelper.overlap([1,2,3], [2,3,4])
%%    {[],[1,2,3],[2,3,4]}
%%
%%    iex> MerklePatriciaTree.ListHelper.overlap([], [2,3,4])
%%    {[],[],[2,3,4]}
%%
%%    iex> MerklePatriciaTree.ListHelper.overlap([1,2,3], [])
%%    {[],[1,2,3],[]}
%%
%%    iex> MerklePatriciaTree.ListHelper.overlap([15, 10, 5, 11], [15, 11, 1, 14])
%%    {[15], [10, 5, 11], [11, 1, 14]}
%% @end
%%------------------------------------------------------------------------------
-spec overlap([integer()], [integer()]) -> {[integer()], [integer()], [integer()]}.
overlap([], [_|_] = B) ->
    {[], [], B;}
overlap([_|_] = A, []) ->
    {[], A, []};
overlap([], []) ->
    {[], [], []};
overlap([A0 | A], [B0 | B]) when A0 == B0 ->
    {O1, A1, B1} = overlap(A, B)
    {[A0 | O1], A1, B1};
overlap(A, B) ->
    {[], A, B}.
