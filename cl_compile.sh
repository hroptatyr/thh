#!/bin/sh

LISPFLAV="${1}"
INFILE=$(readlink -f "${2}")
OUTFILE=$(readlink -f "${3}")

sbcl_compile()
{
	if test $(basename "${INFILE}") != "package.lisp"; then
		cat <<EOF
(require "package" #P"package.fas")
EOF
	fi
	cat <<EOF
(compile-file "${INFILE}" :output-file "${OUTFILE}")
(quit)
EOF
}

case "${LISPFLAV}" in
*"clisp")
	"${LISPFLAV}" -c "${INFILE}" -o "${OUTFILE}"
	;;
*"sbcl")
	sbcl_compile | "${LISPFLAV}"
	;;
*)
	echo "cannot compile ${INFILE}" >&2
esac

