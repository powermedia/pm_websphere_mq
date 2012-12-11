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

-module(pm_websphere_mq_sender).

-behaviour(gen_server).

-export([send/1, send/2]).

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("pm_websphere_mq.hrl").
-define(ACK_TIMEOUT, 600).

-record(state, {port, spid, bin, conf, log}).

start_link(Configuration) ->
    start_link(?SERVER, Configuration).

start_link(Server, {_Bin, _Conf, _Log} = Configuration) ->
    gen_server:start_link({local, Server}, ?MODULE, Configuration, []).

%%% ========================================
%%% Interface
%%% ========================================

send(Msg) ->
    send(?SERVER, Msg).

send(Server, Msg) ->
    try 
        gen_server:call(Server, {msg, Msg})
    catch
        'exit':{ack_timeout, _} ->
            timer:sleep(1000),
            gen_server:call(Server, {msg, Msg})
    end.

%%% ========================================
%%% Callbacks
%%% ========================================

init({Bin, Conf, Log}) ->
    process_flag(trap_exit, true),
    {Port, SPid} = pm_websphere_mq_utils:start_process(Bin, Conf, Log),
    lager:notice("MQ started sender bin: ~w conf: ~w log: ~w", [Bin, Conf, Log]),
    {ok, #state{bin = Bin, conf = Conf, log = Log, port = Port, spid = SPid}}.

handle_call({msg, Msg}, _From, #state{port = Port} = State) ->
    lager:info("Sending message ~s", [pm_seqs:print_term_one_line(Msg)]),
    Port ! {self(), {command, format_message(Msg)}},
    receive
        {Port, {data, <<?EAI_MSG_ACK, _/binary>>}} ->
            ok
    after ?ACK_TIMEOUT ->
            exit(ack_timeout)
    end,
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Port, {data, _}}, #state{port = Port} = State) ->
    %% ignore
    {noreply, State};
handle_info({Port, {exit_status, ExitStatus}},
            #state{port = Port, bin = Bin, conf = Conf, log = Log, spid = SPid}) ->
    lager:warning("sender ~w terminates after port exit status ~w", [self(), ExitStatus]),
    exit({port_terminated,
          {Port, [{bin, Bin}, {conf, Conf}, {log, Log}, {spid, SPid},
                  {exit_status, ExitStatus}]}}).

terminate(_Reason, #state{spid = SPid}) ->
    pm_websphere_mq_utils:terminate_process(SPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ========================================
%%% Internal utils
%%% ========================================

%% Formats message according to our protocol.
-spec format_message(binary()) -> binary().
format_message(Msg) ->
    MsgLen = list_to_binary(integer_to_list(size(Msg))),
    << ?EAI_MSG_BEGIN "\n", MsgLen/binary, "\n", Msg/binary, ?EAI_MSG_END "\n">>.
