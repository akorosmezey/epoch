-module(aec_coinbase_tx_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_coinbase_tx).


%%%=============================================================================
%%% Tests
%%%=============================================================================

coinbase_tx_existing_account_test_() ->
    {foreach,
     fun() ->
             PubKey = <<"my_pubkey">>,
             Account0 = #account{pubkey = PubKey,
                                 balance = 23,
                                 height = 6},
             {PubKey, create_state_tree_with_account(Account0)}
     end,
     fun(_) ->
             ok
     end,
     [fun({PubKey, Trees0}) ->
              {"Check coinbase trx with existing account: shall not change state",
               fun() ->
                       {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey}, Trees0),
                       ?assertEqual({ok, Trees0}, aec_coinbase_tx:check(CoinbaseTx, Trees0, 9))
               end}
      end,
      fun({PubKey, Trees0}) ->
              {"Process coinbase trx with existing account",
               fun() ->
                       {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey}, Trees0),
                       {ok, Trees} = aec_coinbase_tx:process(CoinbaseTx, Trees0, 9),

                       AccountsTree = aec_trees:accounts(Trees),
                       {ok, Account} = aec_accounts:get(PubKey, AccountsTree),
                       ?assertEqual(PubKey, Account#account.pubkey),
                       ?assertEqual(23 + 10, Account#account.balance), %% block reward = 10
                       ?assertEqual(9, Account#account.height)
               end}
      end
     ]
    }.

create_coinbase_tx_no_account_test() ->
    {foreach,
     fun() ->
             PubKey = <<"my_pubkey">>,
             Trees0 = create_state_tree(),
             {ok, CoinbaseTx} = aec_coinbase_tx:new(#{account => PubKey}, Trees0),
             {PubKey, Trees0, CoinbaseTx}
     end,
     fun(_) ->
             ok
     end,
     [fun({PubKey, Trees0, CoinbaseTx}) ->
              {"Check coinbase trx without existing account in state: shall create account",
               fun() ->
                       {Succ, Trees} = aec_coinbase_tx:check(CoinbaseTx, Trees0, 9),
                       ?assertEqual(ok, Succ),
                       AccountsTrees = aec_trees:accounts(Trees),
                       ?assertEqual({ok, #account{pubkey = PubKey,
                                                  balance = 0,
                                                  nonce = 0,
                                                  height = 9}},
                                    aec_accounts:get(PubKey, AccountsTrees))
               end}
      end,
      fun({_PubKey, Trees0, CoinbaseTx}) ->
              {"Process coinbase trx without existing account in state: shall fail",
               ?assertEqual({error, account_not_found}, aec_coinbase_tx:process(CoinbaseTx, Trees0, 9))}
      end
     ]
    }.


mine_from_genesis_test_() ->
    {foreach,
     fun() ->
             application:start(crypto),
             meck:new(aec_chain, [passthrough]),
             meck:expect(aec_chain, top,
                         fun() ->
                                 {ok, T} = meck:passthrough([]),
                                 case aec_trees:accounts(T#block.trees) of
                                     undefined ->
                                         {ok, AccountsTree} = aec_accounts:empty(),
                                         Trees0 = T#block.trees,
                                         Trees = aec_trees:set_accounts(Trees0, AccountsTree),
                                         {ok, T#block{trees = Trees}};
                                     A ->
                                         {ok, T}
                                 end
                         end),
             BGenesis = aec_genesis:genesis_block(),
             StateTree = create_state_tree(),
             ?debugFmt("Setting state tree: ~p~n", [StateTree]),
             aec_chain:start_link(BGenesis#block{trees = StateTree}),
             ?debugFmt("Top block: ~p~n", [aec_chain:top_block()]),
             Pwd = <<"mypassword">>,
             aec_keys:start_link([".", Pwd]),
             {PubKey, PrivKey} = crypto:generate_key(ecdh, secp256k1),
             %%aec_keys:open(Pwd),
             aec_keys:set(Pwd, PrivKey, PubKey)
     end,
     fun(_) ->
             application:stop(crypto),
             meck:unload(aec_chain)
     end,
     [
      {"Mining with (almost) all the bells and whistles",
       fun() ->
               {ok, Block} = aec_mining:mine(),

               ?assertEqual(1, Block#block.height),
               ?assertEqual(1, length(Block#block.txs))
       end}
     ]}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

create_state_tree() ->
    {ok, AccountsTree} = aec_accounts:empty(),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree).

create_state_tree_with_account(Account) ->
    StateTrees0 = create_state_tree(),
    AccountsTree0 = aec_trees:accounts(StateTrees0),
    {ok, AccountsTree} = aec_accounts:put(Account, AccountsTree0),
    aec_trees:set_accounts(StateTrees0, AccountsTree).


-endif.
