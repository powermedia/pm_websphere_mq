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
  This program is a dummy receiver that writes a message with current
  time to stdout, sometimes.  */

#include "common.h"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

void termination_handler(int signum) {
   MSG("Funny, handler, here.\n");
   exit(EXIT_SUCCESS);
}

const char* env_pre  = "<test_message>";
const char* env_post = "</test_message>";

int main(int argc, char **argv) {
   char *config_path;
   handle_cmd_args(argc, argv, &config_path);
   EAIConfig* conf = new_config();
   load_config(config_path, conf);
   
   if (signal(SIGINT, termination_handler) == SIG_IGN)
      signal(SIGINT, SIG_IGN);
   if (signal(SIGHUP, termination_handler) == SIG_IGN)
      signal(SIGHUP, SIG_IGN);
   if (signal(SIGTERM, termination_handler) == SIG_IGN)
      signal(SIGTERM, SIG_IGN);
   
   char msgbuf[1024];
   while (1) {
      sleep(2);
      MSG("TICK\n");
      time_t timer = time(NULL);
      char *m = ctime(&timer);
      EAIMsg msg;
      msg.length = strlen(m) + strlen(env_pre) + strlen(env_post);
      strncpy(msgbuf, env_pre, strlen(env_pre));
      strncpy(msgbuf+strlen(env_pre), m, strlen(m));
      strncpy(msgbuf+strlen(env_pre)+strlen(m), env_post, strlen(env_post)+1);
      msg.msg = msgbuf;
      write_message(&msg);
   }
   
   free_config(conf);
   return 0;
}
