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

-module(pm_websphere_mq_proc_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).
init(_Args) ->
    Sender = {pm_websphere_mq_sender, {pm_websphere_mq_sender, start_link, [sender_config()]},
                  permanent, 200, worker, [pm_websphere_mq_sender]},
    ErrorSender = {pm_websphere_mq_error_sender, {pm_websphere_mq_sender, start_link, [pm_websphere_mq_error_sender, error_sender_config()]},
                  permanent, 200, worker, [pm_websphere_mq_sender]},
    Receiver = {pm_websphere_mq_receiver, {pm_websphere_mq_receiver, start_link, [pm_websphere_mq_regsrv, receiver_config()]},
                    permanent, 200, worker, [pm_websphere_mq_receiver]},
    %% {one_for_one, 100, 1} below: this is correct;
    %% worker processes and their connections are supposed
    %% to be restarted on error
    {ok, {{one_for_one, 100, 1},
          [Sender, ErrorSender, Receiver]}}.

sender_config() ->
    {ok, Bin}  = config:get_env(pm_websphere_mq, sender_binary),
    {ok, Conf} = config:get_env(pm_websphere_mq, sender_config),
    {ok, Log}  = config:get_env(pm_websphere_mq, sender_log),
    {Bin, Conf, Log}.

error_sender_config() ->
    {ok, Bin}  = config:get_env(pm_websphere_mq, sender_binary),
    {ok, Conf} = config:get_env(pm_websphere_mq, error_sender_config),
    {ok, Log}  = config:get_env(pm_websphere_mq, error_sender_log),
    {Bin, Conf, Log}.

receiver_config() ->
    {ok, Bin}  = config:get_env(pm_websphere_mq, receiver_binary),
    {ok, Conf} = config:get_env(pm_websphere_mq, receiver_config),
    {ok, Log}  = config:get_env(pm_websphere_mq, receiver_log),
    {Bin, Conf, Log}.
