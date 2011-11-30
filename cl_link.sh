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
		if test "${arg}" = "thhcc.o"; then
			cat <<EOF
--load '$(strings "${arg}" | grep -F "compiled from" | cut -d\" -f2 | head -n1)'
EOF
		else
			echo "--load '${arg}'"
		fi
	done | xargs -n 4096 "${LISPFLAV}"
	;;
*)
	echo "cannot link ${OUTFILE}" >&2
esac

