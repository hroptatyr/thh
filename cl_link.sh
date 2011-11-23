#!/bin/sh

LISPFLAV="${1}"
OUTFILE="${2}"
shift 2

case "${LISPFLAV}" in
*"clisp")
	"${LISPFLAV}" "${OUTFILE}"
	;;
*"sbcl")
	for arg; do
		cat <<EOF
(load #P"${arg}")
EOF
	done | "${LISPFLAV}"
	;;
*)
	echo "cannot link ${OUTFILE}" >&2
esac

