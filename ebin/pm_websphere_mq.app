{application, pm_websphere_mq, [
    {description,"Application intefacing erlang with WebSphere MQ"},
    {vsn,"0.0.1"},
    {modules, [
        pm_websphere_mq_app,
        pm_websphere_mq_sup,
        pm_websphere_mq,
        pm_websphere_mq_proc_sup,
        pm_websphere_mq_sender,
        pm_websphere_mq_receiver,
        pm_websphere_mq_regsrv,
        pm_websphere_mq_utils]},
    {registered, [
        pm_websphere_mq_sender,
        pm_websphere_mq_receiver,
        pm_websphere_mq_regsrv]},
    {env,[]},
    {applications, [kernel, stdlib, erlsom, config]},
    {mod, {pm_websphere_mq_app, []}}
]}.
