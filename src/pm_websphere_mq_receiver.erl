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

-module(pm_websphere_mq_receiver).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("pm_websphere_mq.hrl").

%% msgstate \in { waiting, beginning, reading, ending }
%% msg - buffer for partial messages
-record(state, {recipient, port, spid, bin, conf, log, msgstate = waiting, msg = [], msglen = 0}).

start_link(Recipient, {Bin, Conf, Log}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Recipient, Bin, Conf, Log}, []).

%%% ========================================
%%% Callbacks
%%% ========================================

init({Recipient, Bin, Conf, Log}) ->
    process_flag(trap_exit, true),
    {Port, SPid} = pm_websphere_mq_utils:start_process(Bin, Conf, Log),
    lager:notice("MQ started receiver bin: ~w conf: ~w log: ~w recipient: ~w", [Bin, Conf, Log, Recipient]),
    {ok, #state{recipient = Recipient, bin = Bin, conf = Conf, log = Log, port = Port, spid = SPid}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}},
            State = #state{recipient = Recipient, 
                           port = Port,
                           msgstate = MsgState,
                           msg = Msg,
                           msglen = MsgLen}) ->
    lager:debug("receiver ~w got data ~s", [self(), pm_seqs:print_term_one_line(Data)]),
    {NewMsgState, NewMsg, NewMsgLen, Output} = handle_data(Data, MsgState, Msg, MsgLen),
    [ begin
          lager:info("Received message ~s", [pm_seqs:print_term_one_line(Message)]),
          Recipient ! {wmq_msg, self(), Message} end
      || Message <- lists:reverse(Output) ],
    NewState = State#state{msgstate = NewMsgState, msg = NewMsg, msglen = NewMsgLen},
    {noreply, NewState};
handle_info({Port, {exit_status, ExitStatus}},
            #state{port = Port, bin = Bin, conf = Conf, log = Log, spid = SPid}) ->
    lager:warning("receiver ~w terminates after port exit status ~w", [self(), ExitStatus]),
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

handle_data(D, S, M, L) ->
    handle_data(D, S, M, L, []).

handle_data(<<>>, Status, Msg, Len, Out) ->
    {Status, Msg, Len, Out};

handle_data(Data, waiting = Status, Msg, Len, Out) ->
    case Data of
        <<?EAI_MSG_BEGIN "\n", Rest/binary>> ->
            handle_data(Rest, beginning, <<>>, 0, Out);
        _ ->
            {Status, Msg, Len, Out}
            %% handle_data(tl(Data), Status, Msg, Len, Out)
    end;

handle_data(Data, beginning = _Status, _Msg, _Len, Out) ->
    case pm_websphere_mq_utils:bin_scan_integer(Data) of
        {NewLen, <<"\n", Rest/binary>>} ->
            handle_data(Rest, reading, <<>>, NewLen, Out);
        _ ->
            {waiting, <<>>, 0, Out}
    end;

handle_data(Data, reading = _Status, Msg, Len, Out) ->
    {TData, Taken, Rest} = takemax(Len, Data),
    NewMsg = <<Msg/binary, TData/binary>>,
    case Len - Taken of
        0 ->
            handle_data(Rest, ending, NewMsg, 0, Out);
        NewLen ->
            handle_data(Rest, reading, NewMsg, NewLen, Out)
    end;

handle_data(Data, ending = Status, Msg, 0, Out) ->
    case Data of 
        <<?EAI_MSG_END, Rest/binary>> ->
            handle_data(Rest, waiting, <<>>, 0, [Msg|Out]);
        _ ->
            {Status, <<>>, 0, Out}
    end.

takemax(N, Bin) when size(Bin) >= N ->
    <<Data:N/binary, Rest/binary>> = Bin,
    {Data, N, Rest};
takemax(N, Bin) when size(Bin) < N ->
    {Bin, size(Bin), <<>>}.
