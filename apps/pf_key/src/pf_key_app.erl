%%% @doc The {@link //pf_key} application callback module.
%%% This is the entry point for the pf_key
%%% <a href="http://www.erlang.org/doc/design_principles/applications.html">
%%% Erlang/OTP application</a>; see that document for details of the callbacks
%%% defined here.
%%%
%%% Starts the top-level application supervisor and initializes metrics.
%%% Cf. `pf_key.app.src'.
-module(pf_key_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @private
-spec start(Type :: term(), Args :: term()) ->
    {ok, Pid :: pid()} |
    {error, Reason :: term()}.
start(_, _) ->
    pf_key_sup:start_link().

%% @private
-spec stop(State :: term()) ->
    ok.
stop(_) ->
    ok.
