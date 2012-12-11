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
  WMQ client implementation common to both sender and receiver.
 */

#ifndef COMMON_H_
#define COMMON_H_

#include <cmqc.h>

#ifdef DEBUG
#include <time.h>
#define MSG(...) do {                                    \
      time_t __DEBUG_TIMER = time(NULL);                 \
      fprintf(stderr, "%s", ctime(&__DEBUG_TIMER));   \
      fprintf(stderr, __VA_ARGS__);                      \
      fprintf(stderr, "\n"); } while(0);
#else
#define MSG(...)         /* debug msg */
#endif

#define GET_WAIT_INTERVAL 1500

/*
 *  pidfile
 */

int create_pidfile(const char *pidfile);

/*
  command line arguments
*/

/* tests and loads config_path; prints reason & terminates application
   upon error (e.g. no access to the specified file) */
void handle_cmd_args(int argc, char** argv, char** config_path);

/*
   handling application config
 */

typedef struct tagEAIConfig EAIConfig;

struct tagEAIConfig {
   const char* channel;
   const char* port;
   const char* host;
   const char* queue_manager;
   const char* output_queue;
   const char* input_queue;

   const char* cipher_spec;
   const char* key_repository;

   const char* pidfile;
};

EAIConfig* new_config();
int load_config(const char *filename, EAIConfig *conf);
int validate_config(EAIConfig *conf);
void free_config(EAIConfig *conf);

/*
  communication with erlang
 */

/*
  our i/o "protocol":
  "EAI_MSG_BEGIN\n<strlen including '\n's>\n<message>EAI_MSG_END\n"
  
  , where:
*/
#define EAI_MSG_BEGIN "eai_msg_begin"
#define EAI_MSG_END   "eai_msg_end"
#define EAI_MSG_ACK   "eai_msg_ack"

typedef struct tagEAIMsg EAIMsg;

struct tagEAIMsg {
   int  length; // not counting final \0!
   char *msg;
};

/* Reads message from stdin. Blocks until a valid message is read,
   discarding any invalid ones. Declaring length as greater than
   message's length will spoil your messages.
   Returns EAIMsg* - heap-allocated (should be free()-ed) message,
   NULL - in case of IO error such as unexpected EOF */
EAIMsg* read_next_message();
/* Writes an ACK to stdout, to be used after read_next_message() */
void write_ack();
/* Writes message to stdout. */
void write_message(EAIMsg* msg);
/* Frees message */
void free_message(EAIMsg* msg);

/*
  MQ
 */

typedef MQLONG CType;
#define INPUT  MQOO_INPUT_AS_Q_DEF
#define OUTPUT MQOO_OUTPUT

/* Returns connection handle */
MQHCONN connect_qm(const EAIConfig* conf);

/* Opens named queue, connection type must be either INPUT or OUTPUT */
MQHOBJ  open_queue(MQHCONN hCon,  const char* queue_name, CType conn_type);

/* Sends a message */
void send_message(MQHCONN hCon, MQHOBJ hObj, const EAIMsg* msg);

/* Receive a message, blocking indefinitely.
   Returned message will be invalidated after next call to receive_message
   (for memory usage and performance reasons, message points to msg input buffer,
   i.e. messages received using receive_message share memory) */
EAIMsg* receive_message(MQHCONN hCon, MQHOBJ hObj);

/* Disconnects, invalidates connection's objects' handles (hObj) */
void disconnect_qm(MQHCONN hCon);

#endif // COMMON_H_
