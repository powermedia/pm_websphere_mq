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

-module(pm_websphere_mq_utils).

-export([start_process/3, terminate_process/1, formated_universal_time/0]).
-export([bin_scan_integer/1]).

%%% used by: pm_websphere_mq_sender, pm_websphere_mq_receiver to start connection processes
%%% @doc Starts process using spawner as required by this application.
-spec start_process(BinaryPath :: string(),
                    ConfigPath :: string(),
                    StdErrDest :: string()) ->
                           {Port :: port(), SystemPid :: integer()}.
start_process(Bin0, Conf0, Log0) ->
    {ok, App} = application:get_application(),
    {ok, SBin0} = config:get_env(spawner_binary),
    SBin = filename:join([code:priv_dir(App), SBin0]),
    Bin  = filename:join([code:priv_dir(App), Bin0]),
    Log  = filename:join([code:lib_dir(App), Log0]),
    Conf = filename:join([code:priv_dir(App), Conf0]),
    MqLibPath = filename:join([code:priv_dir(App), "lib"]),

    LD_LIBRARY_PATH = case os:getenv("LD_LIBRARY_PATH") of
        false ->
            MqLibPath;
        Path ->
            Path ++ [$:|MqLibPath]
    end,

    true = os:putenv("AMQ_SSL_ALLOW_DEFAULT_CERT", "1"),

    lager:debug([{system, backend}],
                "opening port ~w with options ~w",
                [SBin,
                 [stream, use_stdio, exit_status, binary,
                  {env, [{"LD_LIBRARY_PATH", LD_LIBRARY_PATH}]},
                  {args, ["-err", Log, Bin, Conf]}]]),
    
    Port = open_port({spawn_executable, SBin}, [stream, use_stdio, exit_status, binary,
            {env, [{"LD_LIBRARY_PATH", LD_LIBRARY_PATH}]},
            {args, ["-err", Log, Bin, Conf]}]),

    receive
        {Port, {exit_status, ExitStatus}} ->
            %% obviously an error
            lager:error([{system, backend}], "program ~w exited with error status ~w", [SBin, ExitStatus]),
            exit({port_terminated, {Port, [{spawner, SBin}, {bin, Bin},
                                           {conf, Conf}, {log, Log},
                                           {exit_status, ExitStatus}]}});
        {Port, {data, SPidBin}} ->
            {SPid, _} = bin_scan_integer(SPidBin),
            lager:debug([{system, backend}], "program ~w system pid: ~w", [SBin, SPidBin]),
            {Port, SPid}
    after 500 ->
            exit({port_timeouted, {Port, [{spawner, SBin}, {bin, Bin},
                                          {conf, Conf}, {log, Log}]}})
    end.

%%% @doc Terminates system process with a given pid.
terminate_process(SPid) ->
    Cmd = "kill " ++ integer_to_list(SPid),
    Port = open_port({spawn, Cmd}, [exit_status]),
    receive
        {Port, {exit_status, _ExitStatus}} ->
            ok
    after 500 ->
            exit(kill_timeouted)
    end.

formated_universal_time() ->
    {{Y,M,D},{H,Mm,S}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.0Z", [Y,M,D,H,Mm,S])).

bin_scan_integer(Bin) when is_binary(Bin) ->
    bin_scan_integer(Bin, 0).
bin_scan_integer(<<X, Rest/binary>>, N) when $0 =< X andalso X =< $9 ->
    bin_scan_integer(Rest, N*10+(X-$0));
bin_scan_integer(Bin, N) ->
    {N, Bin}.
