-module(port_protocol_SUITE).

-compile(export_all).

-include("ct.hrl").

all() -> [ message_sent_gets_ack,
           message_sent_gets_ack_through_error_channel,
           message_received_after_registration ].

-define(DEPS, [config, erlsom, pm_websphere_mq]).

init_per_suite(Config) ->
    test_helper:start_apps(?DEPS),
    timer:sleep(100),
    Config.

end_per_suite(Config) ->
    test_helper:stop_apps(?DEPS),
    Config.

%%% ========================================
%%% Test cases
%%% ========================================

message_sent_gets_ack(_Config) ->
    % let's break encapsulation
    Pid = whereis(pm_websphere_mq_sender),
    pm_websphere_mq:send(<<"whatever">>),
    timer:sleep(400), % more than pm_websphere_mq_sender:ACK_TIMEOUT
    Pid = whereis(pm_websphere_mq_sender),
    ok.

message_sent_gets_ack_through_error_channel(_Config) ->
    % let's break encapsulation
    Pid = whereis(pm_websphere_mq_error_sender),
    pm_websphere_mq:send_error(<<"whatever">>),
    timer:sleep(400), % more than pm_websphere_mq_sender:ACK_TIMEOUT
    Pid = whereis(pm_websphere_mq_error_sender),
    ok.

message_received_after_registration(_Config) ->
    pm_websphere_mq:register(self()),
    receive
        {wmq_msg, _} ->
            ok
    after 3000 -> % more than ticker's interval
            error("ticker should've sent a message by now")
    end,
    pm_websphere_mq:unregister(self()),
    ok.
