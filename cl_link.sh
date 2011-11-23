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
		srcfile=$(strings "${arg}" | \
			grep -F "compiled from" | \
			cut -d'"' -f2)
		cat <<EOF
(load #P"${srcfile}")
EOF
	done | "${LISPFLAV}"
	;;
*)
	echo "cannot link ${OUTFILE}" >&2
esac

