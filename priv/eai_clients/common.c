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

#include "common.h"
#include "ini.h"

// websphere
#include <cmqc.h>
#include <cmqxc.h>

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>

/* Maximum message length, this variable will be initialized by
   open_queue.
   Sender will silently discard messages longer than max_msg_length.
   Receiver will use buffer of max_msg_length+1 bytes. */
static MQLONG max_msg_length = 0;
/* Buffer for receiving messages, memory will be allocated by
   open_queue iff queue is open with INPUT as mode, and free-ed by
   disconnect_qm.
   This can take up to 100MB (+ one byte). */
static char* msg_buffer = NULL;

/*
 * PIDFILE
 */

#define MAX_PID_LEN 100

int lock_pidfile(int fd) {
   struct flock fl;
   fl.l_type = F_WRLCK;
   fl.l_whence = SEEK_SET;
   fl.l_start = 0;
   fl.l_len = 0;

   return fcntl(fd, F_SETLK, &fl);
}

int create_pidfile(const char *pidfile) {
   int fd;
   char pid_buf[MAX_PID_LEN];

   fd = open(pidfile, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
   if (fd == -1) {
      printf("ERROR: Could not open PID file %s\n", pidfile);
      exit(9);
   }

   int flags = fcntl(fd, F_GETFD);
   if (flags == -1) {
      printf("ERROR: Could not get flags for PID file %s\n", pidfile);
      exit(9);
   }

   flags |= FD_CLOEXEC;

   if (fcntl(fd, F_SETFD, flags) == -1) {
      printf("ERROR: Could not set flags for PID file %s\n", pidfile);
      exit(9);
   }

   if (lock_pidfile(fd) == -1) {
      if (errno == EAGAIN || errno == EACCES) {
         printf("ERROR: PID file %s is locked; is program already running?\n", pidfile);
         exit(9);
      }
   }

   if (ftruncate(fd, 0) == -1) {
      printf("ERROR: Could not truncate PID file %s\n", pidfile);
      exit(9);
   }

   snprintf(pid_buf, MAX_PID_LEN, "%ld\n", (long) getpid());
   if (write(fd, pid_buf, strlen(pid_buf)) != strlen(pid_buf)) {
      printf("ERROR: Failed to write to PID file %s\n", pidfile);
      exit(9);
   }

   return fd;
}


/*
 * CMD LINE ARGS
 */

static void print_usage_info_and_exit(char* progname) {
   printf("USAGE:\n\t%s <path to configuration file>\n", progname);
   exit(EXIT_FAILURE);
}

void handle_cmd_args(int argc, char** argv, char** config_path) {
   if (argc != 2)
      print_usage_info_and_exit(argv[0]);
   if (-1 == access(argv[1], R_OK)) {
      printf("Error: Cannot access file %s for reading, exiting\n", argv[1]);
      exit(EXIT_FAILURE);
   }
   *config_path = strdup(argv[1]);
}

/*
 * CONFIG
 */

EAIConfig* new_config() {
   EAIConfig *conf = (EAIConfig*)malloc(sizeof(EAIConfig));
   return conf;
}

static int handler(void* user, const char* section, const char* name, const char* value) {
   EAIConfig* conf = (EAIConfig*)user;

#define MATCH(s, n) strcasecmp(section, s) == 0 && strcasecmp(name, n) == 0
   if (MATCH("connection", "channel")) {
      conf->channel = strdup(value);
   } else if (MATCH("connection", "port")) {
      conf->port = strdup(value);
   } else if (MATCH("connection", "host")) {
      conf->host = strdup(value);
   } else if (MATCH("connection", "queue_manager")) {
      conf->queue_manager = strdup(value);
   } else if (MATCH("connection", "input_queue")) {
      conf->input_queue = strdup(value);
   } else if (MATCH("connection", "output_queue")) {
      conf->output_queue = strdup(value);
   } else if (MATCH("ssl", "cipher_spec")) {
      conf->cipher_spec = strdup(value);
   } else if (MATCH("ssl", "key_repository")) {
      conf->key_repository = strdup(value);
   } else if (MATCH("main", "pidfile")) {
      conf->pidfile = strdup(value);
   }
   return 0;
#undef MATCH
}

/* Loads and validates config, returns 0 upon success, positive number
   otherwise. */
int load_config(const char *filename, EAIConfig *conf) {
   if (ini_parse(filename, handler, conf) < 0) {
      printf("Can't open %s\n", filename);
      return 1;
   }
   return validate_config(conf);
}

int validate_config(EAIConfig *conf) {
#ifndef DEBUG
#define VAL(x) if (NULL == conf->x) { return 2; }
#else
#define VAL(x)                                          \
   MSG("Validating config %s: %s;\n", #x, conf->x);     \
   if (NULL == conf->x) { return 2; }
#endif
   VAL(channel);
   VAL(port);
   VAL(host);
   VAL(queue_manager);
   VAL(output_queue);
   VAL(input_queue);
   return 0;
#undef VAL
}

void free_config(EAIConfig *conf) {
   if (!conf) return;
#define F(x) if (conf->x) { free((void*)(conf->x)); }
   F(channel);
   F(port);
   F(host);
   F(queue_manager);
   F(output_queue);
   F(input_queue);
   free(conf);
#undef F
}

/*
 * ERLANG COMM
 */

EAIMsg* read_next_message() {
#define BUF_LEN 20
   char buf[BUF_LEN];
   char *msg_buf = NULL;
   char *ret = NULL;
   int  len = 0;
   memset(buf, 0, BUF_LEN);
   do {
      ret = fgets(buf, BUF_LEN, stdin);
      if (NULL == (void*)ret) return NULL;

      if (0 == strncmp(EAI_MSG_BEGIN "\n", buf, BUF_LEN)) {
         MSG("read EAI_MSG_BEGIN\n");
         // read message length
         ret = fgets(buf, BUF_LEN, stdin);
         if (NULL == ret) return NULL;
         len = atoi(buf);
         msg_buf = malloc(sizeof(char) * len + 1);
         memset(msg_buf, 0, len+1);
         if (NULL == msg_buf) return NULL;
         if (len <= 0) {
            MSG("not a proper message length\n");
            continue;
         }
         // read message
         int left = len;
         do {
            MSG("waiting for %d bytes of message\n", left);
            ret = fgets(msg_buf + (len-left), left+1, stdin);
            if (NULL == ret) { free(msg_buf); return NULL; }
            MSG("read: <<%s>>\n", ret);
            left -= strlen(ret);
         } while (left > 0);
         MSG("read the whole message, waiting for EAI_MSG_END\n");
         MSG("message: <<%s>>\n", msg_buf);
         ret = fgets(buf, BUF_LEN, stdin);
         if (NULL == ret) { free(msg_buf); return  NULL; }
         if (0 == strncmp(EAI_MSG_END "\n", buf, BUF_LEN)) {
            MSG("read EAI_MSG_END, message accepted\n");
            EAIMsg* msg = (EAIMsg*)malloc(sizeof(EAIMsg));
            if (NULL == msg) { free(msg_buf); return NULL; }
            // everything's okay, return message
            msg->length = len;
            msg->msg = msg_buf;
            return msg;
         } else {
            MSG("no EAI_MSG_END");
            free(msg_buf);
            continue;
         }
      } else {
         MSG("not an EAI_MSG_BEGIN\n");
         continue;
      }
   } while (1);
#undef BUFF_LEN
}

void write_ack() {
   printf(EAI_MSG_ACK "\n");
   fflush(NULL);
}

void write_message(EAIMsg* msg) {
   MSG("writing msg...\n");
   printf(EAI_MSG_BEGIN "\n%d\n", msg->length);
   printf("%s" EAI_MSG_END "\n", msg->msg);
   fflush(NULL);
}

void free_message(EAIMsg* msg) {
   MSG("freeing msg...\n");
   if (!msg) return;
   if ((msg->msg) && ((msg->msg < msg_buffer) || (msg->msg > msg_buffer + max_msg_length))) {
         free(msg->msg);
   } 
   free(msg);
}

/*
 * MQ
 */


MQHCONN connect_qm(const EAIConfig* conf) {
   
   // structures' declarations
   MQCNO    ConnOptions = {MQCNO_DEFAULT};
   MQCD     ClientConn = {MQCD_CLIENT_CONN_DEFAULT};
   MQSCO    SSLOptions = {MQSCO_DEFAULT};

   
   MQHCONN  hCon;
   MQLONG   CompCode;
   MQLONG   CReason;

   // // // setting client connection options

   // // channel
   strncpy(ClientConn.ChannelName,
           conf->channel,
           MQ_CHANNEL_NAME_LENGTH);
   MSG("'%s'", ClientConn.ChannelName);

   ClientConn.ChannelType = MQCHT_CLNTCONN;

   ClientConn.TransportType = MQXPT_TCP;
   
   // connection name: host(port)
   int host_length = strlen(conf->host);
   int port_length = strlen(conf->port);
   int connection_name_length = (host_length +
                                 port_length +
                                 2 +  // parens
                                 1); // terminating \0
   char* connection_name = malloc(sizeof(char) * connection_name_length);
   memset(connection_name, 0, connection_name_length);
   memcpy(connection_name, conf->host, host_length);
   connection_name[host_length] = '(';
   memcpy(connection_name + 1 + host_length, conf->port, port_length);
   connection_name[host_length + 1 + port_length] = ')';
   
   strncpy(ClientConn.ConnectionName,
           connection_name,
           MQ_CONN_NAME_LENGTH);

   MSG("'%s'\n", connection_name);
   free(connection_name);
   
   MSG("'%s'", ClientConn.ConnectionName);
   // short connection name
   strncpy(ClientConn.ShortConnectionName,
           ClientConn.ConnectionName,
           20);
   MSG("'%s'", ClientConn.ShortConnectionName);
   
   // // connection options
   ConnOptions.Version = MQCNO_VERSION_4; // 2 - mqcd, 3 - mqsco
   ClientConn.Version = MQCD_VERSION_7; // needed for ciphersuite
   
   ConnOptions.ClientConnPtr = &ClientConn;

   if (conf->cipher_spec) {
      // // // adding ssl configuration

      MSG("Using SSL with cipher '%s' and repository '%s'\n", conf->cipher_spec, conf->key_repository);

      strncpy(ClientConn.SSLCipherSpec,
              conf->cipher_spec,
              MQ_SSL_CIPHER_SPEC_LENGTH);
      MSG("'%s'", ClientConn.SSLCipherSpec);
   
      strncpy(SSLOptions.KeyRepository,
              conf->key_repository,
              MQ_SSL_KEY_REPOSITORY_LENGTH);
      MSG("'%s'", SSLOptions.KeyRepository);
      
      ConnOptions.SSLConfigPtr = &SSLOptions;
   } // end ssl
      
   // // // CONNECT
   MSG("Connecting using MQCONNX\n");

   MQCONNX((PMQCHAR)conf->queue_manager,
           &ConnOptions,
           &hCon,
           &CompCode,
           &CReason);

   MSG("MQCONNX CompCode %ld Reason %ld\n", CompCode, CReason);

   if (CompCode == MQCC_FAILED) {
      printf("Connection error\n");
      return 0;
   }

   return hCon;
}

MQHOBJ open_queue(MQHCONN hCon, const char* queue_name, const CType conn_type) {
   
   MQOD     ObjDesc = {MQOD_DEFAULT};
   MQHOBJ   hObj;
   MQLONG   CompCode;
   MQLONG   Reason;
   
   strncpy(ObjDesc.ObjectName,
           queue_name,
           MQ_Q_NAME_LENGTH);
   
   MSG("opening queue %s with conn_type %ld", queue_name, conn_type);

   MQOPEN(hCon,
          &ObjDesc,
          conn_type | MQOO_FAIL_IF_QUIESCING,
          &hObj,
          &CompCode,
          &Reason);

   MSG("MQOPEN CompCode: %ld Reason: %ld\n", CompCode, Reason);
   if ((CompCode != MQCC_OK) || (Reason != MQRC_NONE)) {
      printf("open error\n");
      return 0;
   }

   // // // uncommend if allowed/needed, see below

   // // ask about maximum message length
   
   // MQLONG Selector = MQIA_MAX_MSG_LENGTH;
   // 
   // MQINQ(hCon,
   //       hObj,
   //       1,
   //       &Selector,
   //       1,
   //       &max_msg_length,
   //       0,
   //       NULL,
   //       &CompCode,
   //       &Reason);
   // 
   // MSG("MQ CompCode: %ld, Reason: %ld\n", CompCode, Reason);
   // 
   // if (CompCode != MQCC_OK) {
   //    printf("Failed to get max msg length\n");
   //    exit(EXIT_FAILURE);
   // }
   // 
   // MSG("got max_msg_length := %ld\n", max_msg_length);
   

   // NOT AUTHORIZED FOR INQUIRY, ASSUME MAX POSSIBLE LENGTH OF 100MB
   max_msg_length = 1048576;
   
   // // // if receiving, initialize message buffer
   if (conn_type == INPUT) {
      
      MSG("initializing message buffer\n");
      
      msg_buffer = malloc(sizeof(MQBYTE) * max_msg_length + 1);
      if (NULL == msg_buffer) {
         printf("Failed to allocate memory for input message buffer\n");
         exit(EXIT_FAILURE);
      }

      MSG("finished initializing msg_buffer\n");
   }
   
   return hObj;
}

void send_message(MQHCONN hCon, MQHOBJ hObj, const EAIMsg* msg) {

   MQMD    MsgDesc = {MQMD_DEFAULT};
   MQPMO   PutMsgOpts = {MQPMO_DEFAULT};
   MQLONG  CompCode;
   MQLONG  Reason;
   
   // don't process messages longer than queues MaxMsgLength
   if (msg->length > max_msg_length) {
      return;
   }
   
   strncpy(MsgDesc.Format,
          MQFMT_STRING,
          MQ_FORMAT_LENGTH);
   MsgDesc.Encoding = 546;
   MsgDesc.CodedCharSetId = 1208;
   
   MSG("sending message length: %d msg: <<%s>>\n", msg->length, msg->msg);
   
   MQPUT(hCon, hObj, &MsgDesc, &PutMsgOpts,
         msg->length, msg->msg,
         &CompCode, &Reason);
   
   MSG("MQPUT CompCode: %ld Reason: %ld\n", CompCode, Reason);
   
   if (CompCode != MQCC_OK) {
      // we terminate; erlang should retry sending message if required
      exit(EXIT_FAILURE);
   }
}

EAIMsg* receive_message(MQHCONN hCon, MQHOBJ hObj) {
   
   MQMD  MsgDesc = {MQMD_DEFAULT};
   MQGMO GetMsgOpts = {MQGMO_DEFAULT};
   
   MQLONG MsgLength;
   MQLONG CompCode;
   MQLONG Reason;
   
   GetMsgOpts.Options =
      MQGMO_WAIT +
      MQGMO_CONVERT +
      MQGMO_FAIL_IF_QUIESCING;

   // GetMsgOpts.WaitInterval = MQWI_UNLIMITED;
   GetMsgOpts.WaitInterval = GET_WAIT_INTERVAL;
   
   strncpy(MsgDesc.Format,
          MQFMT_STRING,
          MQ_FORMAT_LENGTH);
   MsgDesc.Encoding = 546;
   MsgDesc.CodedCharSetId = 1208;
   
   MSG("waiting for message\n");
   
   MQGET(hCon,
         hObj,
         &MsgDesc,
         &GetMsgOpts,
         max_msg_length,
         msg_buffer,
         &MsgLength,
         &CompCode,
         &Reason);
   
   MSG("MQGET CompCode: %ld, Reason: %ld", CompCode, Reason);
   
   if (CompCode == MQCC_FAILED) {
      if (Reason == MQRC_NO_MSG_AVAILABLE) {
         return NULL;
      } else {
         printf("MQGET failure\n");
         exit(EXIT_FAILURE);
      }
   }
   
   msg_buffer[MsgLength] = 0;

   /* jms "compatibility mode" for sms as - ad hoc solution */

   /* sms as sends messages with rfh headers and I could not find how
      to convert message client-side, so let's just strip these
      headers. */


   char* msg_body = msg_buffer;
    if (0 == strncmp(msg_body, "RFH ", 4)) {
       while (0 != strncmp(msg_body, "<?xml", 5)) {
          msg_body++;
          if (msg_body >= (msg_buffer + MsgLength - 5)) {
             msg_body = msg_buffer;
             break;
          }
       }
    }

   /* end of jms/rfh solution */
   
   MSG("got message: <<%s>>\n", msg_body);
   
   EAIMsg* msg = (EAIMsg*)malloc(sizeof(EAIMsg));
   if (msg == NULL) {
      MSG("msg mailloc failed\n");
      exit(EXIT_FAILURE);
   }
   msg->length = MsgLength - (msg_body - msg_buffer);
   msg->msg = msg_body; // be careful
   
   return msg;
}

void disconnect_qm(MQHCONN hCon) {

   MQLONG CompCode;
   MQLONG Reason;

   MSG("Disconnecting using MQDISC\n");
   
   MQDISC(&hCon, &CompCode, &Reason);

   MSG("MQDISC CompCode: %ld Reason: %ld\n", CompCode, Reason);

   if (msg_buffer) { free(msg_buffer); }
}
