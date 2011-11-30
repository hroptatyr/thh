/*** thhcc-wrap.c -- lest non-lispers won't be blown away
 *
 * Copyright (C) 2011 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <freundt@ga-group.nl>
 *
 * This file is part of thh/dateutils.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of any contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 **/
#if defined HAVE_CONFIG_H
# include "config.h"
#endif	/* HAVE_CONFIG_H */
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/stat.h>

#if !defined LIBEXECDIR
# define LIBEXECDIR	"../libexec"
#endif	/* !LIBEXECDIR */

static int
test_thhcc(char *rpath, size_t rpsz, size_t bsz, const char *p)
{
	struct stat st;

	snprintf(rpath + rpsz, bsz - rpsz, "%s/thhcc.bin", p);
	return stat(rpath, &st) == 0 &&
		st.st_mode & S_IXUSR;
}

static char*
find_thhcc(const char *cur_argv)
{
	static char exe[] = "/proc/self/exe";
	char path[1024];
	char *last_slash;
	size_t size;

	if ((size = readlink(exe, path, sizeof(path) - 1)) < 0 ||
	    (last_slash = strrchr(path, '/')) == NULL) {
		return NULL;
	}

	size = last_slash - path + 1;
	if (test_thhcc(path, size, sizeof(path), LIBEXECDIR) ||
	    test_thhcc(path, size, sizeof(path), ".")) {
		return strdup(path);
	}
	return NULL;
}


#if defined __INTEL_COMPILER
# pragma warning (disable:593)
# pragma warning (disable:181)
#endif	/* __INTEL_COMPILER */
#include "thhcc-clo.h"
#include "thhcc-clo.c"
#if defined __INTEL_COMPILER
# pragma warning (default:593)
# pragma warning (default:181)
#endif	/* __INTEL_COMPILER */

int
main(int argc, char *argv[])
{
	struct gengetopt_args_info argi[1];
	int res = 0;

	if (cmdline_parser(argc, argv, argi)) {
		res = 1;
		goto out;
	} else if (argi->inputs_num < 1) {
		res = 1;
		fputs("need definition file.\n", stderr);
		goto out;
	}

	/* prepare the execve */
	{
		char *bin = find_thhcc(argv[0]);
		char *file = argi->inputs[0];
		char *sym = argi->inputs[1];
		char *new_argv[16] = {
			bin, argi->inputs[0], argi->inputs[1], NULL
		};

		if (bin == NULL) {
			res = 1;
			goto out;
		} else if (argi->year_given && argi->year_arg[0] >= 1900) {
			char year[8];
			snprintf(year, sizeof(year), "%d", argi->year_arg[0]);
			new_argv[3] = year;
		}
		execv(bin, new_argv);
		free(bin);
		res = 1;
	}
out:
	cmdline_parser_free(argi);
	return res;
}

/* thhcc-wrap.c ends here */
