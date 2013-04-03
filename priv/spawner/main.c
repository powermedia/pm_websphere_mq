/*
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
 * A small utility starting other application, that:
 * - first wirtes its system pid to stdout and stderr
 * - optionally redirects its stdin/stdout to each other or a given file
 * - and then loads given binary and execs (execvps) it.
 *
 * Command line format:
 *
 * spawner <app> [-out <outred>] [-err <errred>] [arg0 [arg1 ...]]
 *
 * where:
 * - app - name of binary to execute, searched for in PATH
 * - outred, errred - either stdout, stderr or a file to redirect to
 *   (program will exit with code 2 if file cannot be written to)
 * - arg0, arg1... - arguments to the app launched
 *
 * Exit codes:
 * - 901 - memory allocation error
 * - 902 - exec error
 * - 903 - stream redirect error 1
 * - 904 - stream redirect error 2
 * - any other code comes from exec'ed application
 */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void print_usage_info_and_exit(const char* progname, int code);
void redirect(int oldfd, const char* target);

int main(int argc, char** argv) {
   
   // // print pid

   pid_t pid = getpid();
   printf("%d\n", pid);
   fflush(NULL);
   
   // // read cmdline args and redirect streams
   if (argc <= 1) { print_usage_info_and_exit(argv[0], EXIT_SUCCESS); }

   int    aargc = 0;
   char** aargv;

   aargv = malloc(sizeof(char*) * (argc + 1));
   if (NULL == aargv) { exit(901); }

   int i = 1;
   int testargs = 1;
   while (i < argc) {
      if (testargs && (0 == strcmp("-out", argv[i]))) {
         redirect(fileno(stdout), argv[i+1]);
         i += 2;
      } else if (testargs && (0 == strcmp("-err", argv[i]))) {
         redirect(fileno(stderr), argv[i+1]);
         i += 2;
      } else if (testargs && (0 == strcmp("--", argv[i]))) {
         testargs = 0;
         i += 1;
      } else {
         aargv[aargc] = strdup(argv[i]);
         aargc += 1;
         i += 1;
      }
   }

   if (aargc == 0) { print_usage_info_and_exit(argv[0], EXIT_SUCCESS); }

   aargv[aargc] = (char*) NULL;
   
   // // execute app
   execvp(aargv[0], aargv);

   // end of control
   exit(902);
}

void print_usage_info_and_exit(const char* progname, int code) {
   printf("Usage info:\n"
          "\t%s <application to start>\n"
          "\t\t[-out <stderr|path to file>] [-err <stdin|path to file>]\n"
          "\t\t[arg0 [arg1 ...]]\n\n",
          progname);
   exit(code);
}

void redirect(int oldfd, const char* target) {
   int targetfd;

   if (0 == strcmp("stdout", target)) {
      targetfd = STDOUT_FILENO;
   } else if (0 == strcmp("stderr", target)) {
      targetfd = STDERR_FILENO;
   } else {
      targetfd = open(target, O_WRONLY | O_APPEND | O_CREAT, 00640);
      if (-1 == targetfd) { exit(903); }
   }
   
   if (-1 == dup2(targetfd, oldfd)) { exit(904); }
}
