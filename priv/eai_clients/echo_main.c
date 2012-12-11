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
  This program just loads configuration and echoes messages sent to
  it; perhaps this could be used for testing.
 */

#include "common.h"
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

void termination_handler(int signum) {
   MSG("Funny, handler.\n");
   exit(EXIT_SUCCESS);
}

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
   
   while (1) {
      EAIMsg* msg = read_next_message();
      write_ack();
      write_message(msg);
      free_message(msg);
   }
   
   free_config(conf);
   return 0;
}
