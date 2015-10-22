%%% @doc The {@link //sesame} application callback module.
%%% This is the entry point for the Workbench Metrics
%%% <a href="http://www.erlang.org/doc/design_principles/applications.html">
%%% Erlang/OTP application</a>; see that document for details of the callbacks
%%% defined here.
%%%
%%% Starts the top-level application supervisor and initializes metrics.
%%% Cf. `sesame.app.src'.
-module(sesame_app).
-behaviour(application).
-export([start/2, stop/1]).

%% @private
-spec start(Type :: term(), Args :: term()) ->
    {ok, Pid :: pid()} |
    {error, Reason :: term()}.
start(_, _) ->
    sesame_sup:start_link().

%% @private
-spec stop(State :: term()) ->
    ok.
stop(_) ->
    ok.
