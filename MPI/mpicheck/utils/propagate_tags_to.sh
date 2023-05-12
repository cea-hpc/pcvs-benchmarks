#!/bin/bash
LISTING="no"
FUNC_REGEX=
TAG=

err()
{
	>&2 echo "$@"
	exit 42
}

for arg in $@; do
	case $arg in
		-l|--list)
			LISTING="yes"
			;;
		-f=*)
			user_file=$(echo $arg | sed -e "s/-f=//g")
			test -f $user_file || err "File $user_file not found !"
			FUNC_REGEX="$FUNC_REGEX $(cat $user_file)"
			;;
		-h|--help)
			echo "Usage: ./script [FUNC_REGEX] [TAG]"
			echo ""
			echo "#List functions matching regex"
			echo ./script [FUNC_REGEX] --list
			exit 0
			;;
		*)
			if test -z "$FUNC_REGEX"; then
				FUNC_REGEX=$arg
			else
				TAG=$arg
			fi
			;;
	esac
done

test -z "$FUNC_REGEX" && err "Not MPI function to alter. Provide a first argument"
test -z "$TAG" -a "$LISTING" = "no" && err "Not tag list to add. Provide a second argument (& more)"

for func in $FUNC_REGEX; do
	funcs=$(grep -oRE "^$func\$" *.dat | cut -f2 -d":" | sort -u)

	if test "$LISTING" = "yes"; then
		echo "$funcs"
	else
		echo "$funcs" > ./tags/$TAG.list
	fi
done
