#!/bin/sh

LISPFLAV="${1}"
shift 1

case "${LISPFLAV}" in
*"clisp")
	for arg; do
		cat <<EOF
(load #P"${arg}")
EOF
	done | "${LISPFLAV}"
	;;
*"sbcl")
	for arg; do
		echo -e "--load '${arg}'"
	done | xargs -n 4096 "${LISPFLAV}"
	;;
*)
	echo "cannot link ${OUTFILE}" >&2
esac

