### pm_websphere_mq

pm_websphere_mq is a simple Erlang Websphere MQ client application
written by Power Media S.A (http://www.power.com.pl)

### Installation and usage

Edit priv/eai_clients/Makefile with correct paths to wmq library and
header files. To compile run make in priv/.

Application configuration example:

    {pm_websphere_mq, [
        %% *binaries' and *configs' paths are relative to app's priv/ directory
        {spawner_binary,  "bin/spawner"},
        {sender_binary,   "bin/sender"},
        {receiver_binary, "bin/receiver"},
        {sender_config,   "sender_prod.ini"},
        {error_sender_config, "error_sender_prod.ini"},
        {receiver_config, "receiver_prod.ini"},
        %% *logs' paths are relative to app's main directory;
        %% these files are for redirected stdout (debug messages)
        {sender_log,   "../../log/sender_debug.txt"},
        {error_sender_log,    "../../log/error_debug.txt"},
        {receiver_log, "../../log/receiver_debug.txt"}
    ]}

### Third party code

Includes source code from:
inih: https://code.google.com/p/inih/ (BSD-2)
