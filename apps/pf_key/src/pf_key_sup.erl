%%% @doc The {@link //pf_key} supervisor callback module.
%%% This is the top-level
%%% <a href="http://www.erlang.org/doc/design_principles/sup_princ.html">
%%% Erlang/OTP supervisor</a> for the pf_key application.
%%% See the supervisor document for details of the callbacks defined here.
%%%
%%% Supervises monitoring processes for pf_key.
-module(pf_key_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% @doc Start the pf_key top-level supervisor.
%% Note that we pass empty `Args' to the init/1 callback.
%% @private
-spec start_link() ->
    {ok, pid()} |
    ignore |
    {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initialize the pf_key top-level supervisor.
%% @private
-spec init(Args :: list()) ->
    {ok, {{RestartStrategy :: supervisor:strategy(),
           MaxR :: non_neg_integer(),
           MaxT :: non_neg_integer()},
          [supervisor:child_spec()]}} |
    ignore.
init(_Args) ->
    {ok, {{one_for_one, 5, 10}, []}}.
