/* pm_websphere_mq - a simple WMQ gateway.
*
* Copyright (c) 2012, Power Media S.A.  All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met: Redistributions of source code must retain the above copyright
* notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
* notice, this list of conditions and the following disclaimer in the
* documentation and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
* HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
* LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
  Sender application: puts messages from stdin to a WMQ queue

  Protocol is defined in common.h
 */

#include "common.h"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

MQHCONN hCon;

void termination_handler(int signum) {
   disconnect_qm(hCon);
   exit(EXIT_SUCCESS);
}

int main(int argc, char **argv) {

   if (!getenv("AMQ_SSL_ALLOW_DEFAULT_CERT")) {
      MSG("please set environment variable AMQ_SSL_ALLOW_DEFAULT_CERT=1\n");
      exit(678);
   }

   char *config_path;
   handle_cmd_args(argc, argv, &config_path);
   EAIConfig* conf = new_config();
   load_config(config_path, conf);
   
   create_pidfile(conf->pidfile);

   hCon = connect_qm(conf);
   if (!hCon) { return 1; }
   MQHOBJ  hObj = open_queue(hCon, conf->output_queue, OUTPUT);
   if (!hObj) { return 1; }

   if (signal(SIGINT, termination_handler) == SIG_IGN)
      signal(SIGINT, SIG_IGN);
   if (signal(SIGHUP, termination_handler) == SIG_IGN)
      signal(SIGHUP, SIG_IGN);
   if (signal(SIGTERM, termination_handler) == SIG_IGN)
      signal(SIGTERM, SIG_IGN);
   
   while (1) {
      EAIMsg* msg = read_next_message();
      send_message(hCon, hObj, msg);
      write_ack();
      free_message(msg);
   }
   
   disconnect_qm(hCon);
   free_config(conf);
   return 0;
}

