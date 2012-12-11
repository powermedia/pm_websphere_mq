%%% pm_websphere_mq - a simple WMQ gateway.
%%%
%%% Copyright (c) 2012, Power Media S.A.  All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met: Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%% Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(pm_websphere_mq_regsrv).

-behaviour(gen_server).

-export([register/1, unregister/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {queue = queue:new(), listeners = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% ========================================
%%% Interface
%%% ========================================

%% @doc Registers process to EAI messages.
%%
%% Messages will be sent as {wmq_msg, Message}.
%% Multiple calls will cause multiple messages.
%%
%% If no process were registered, any messages received
%% since last process unregistered / regsrv started
%% will be send to the new listener.
register(Whom) ->
    gen_server:cast(?SERVER, {register, Whom}).

%% @doc Registers process from receiving EAI messages.
%%
%% Messages will be sent as {wmq_msg, Message}.
%% Cancels only one registration. Has no effect
%% if process is not registered.
unregister(Whom) ->
    gen_server:cast(?SERVER, {unregister, Whom}).

%%% ========================================
%%% Callbacks
%%% ========================================

init(_Args) ->
    Listeners = case config:get_env(pm_websphere_mq, default_recipient) of
                {ok, R} -> [R];
                _ -> []
            end,
    {ok, #state{listeners = Listeners}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({register, Pid}, State = #state{listeners = [], queue = Queue}) ->
    lager:info([{system, backend}], "MQ registering listener ~p", [Pid]),
    lager:info([{system, backend}], "Sending ~p queued messages to listener ~p", [queue:len(Queue), Pid]),
    send_old_messages(Pid, Queue),
    NewState = State#state{listeners = [Pid], queue = queue:new()},
    {noreply, NewState};
handle_cast({register, Pid}, State = #state{listeners = Listeners}) ->
    lager:info([{system, backend}], "MQ registering listener ~p", [Pid]),
    NewState = State#state{listeners = [Pid|Listeners]},
    {noreply, NewState};
handle_cast({unregister, Pid}, State = #state{listeners = Listeners}) ->
    lager:info([{system, backend}], "MQ unregistering listener ~p", [Pid]),
    NewState = State#state{listeners = lists:delete(Pid, Listeners)},
    {noreply, NewState}.

handle_info({wmq_msg, _Receiver, Message},
            State = #state{listeners = [], queue = Queue}) ->
    NewState = State#state{queue = queue:in(Message, Queue)},
    {noreply, NewState};
handle_info({wmq_msg, _Receiver, Message},
            State = #state{listeners = Listeners}) when Listeners /= [] ->
    lists:foreach(fun (Listener) -> Listener ! {wmq_msg, Message} end, Listeners),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ========================================
%%% Internal utils 
%%% ========================================

send_old_messages(Pid, Q) ->
    case queue:out(Q) of
        {{value, Message}, Rest} ->
            Pid ! {wmq_msg, Message},
            send_old_messages(Pid, Rest);
        {empty, _} ->
            ok
    end.
