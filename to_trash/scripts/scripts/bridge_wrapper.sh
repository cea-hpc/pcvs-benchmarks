#!/bin/sh

# $1 var name
# $2 var value
# set $bridge_value
bridge_set_if_defined()
{
	bridge_value=""
	prefix="`eval echo "$"${1}_PREFIX`"
	value=$2

	if test -z "$prefix"; then
		eval "type ${1}_TRANSLATE" > /dev/null 2>&1
		if test "$?" != "0"; then
			return;
		else
			prefix="`eval ${1}_TRANSLATE \"$value\"`"
		fi	
	fi

	if test -z "$prefix"; then
		bridge_value=""
	else
		bridge_value="$prefix$value"
	fi

	eval "$1=$bridge_value"
	echo "$bridge_value"
}

#$1 : var name
bridge_is_defined()
{
	prefix="`eval echo "$"${1}_PREFIX`"
	func="`eval type ${1}_TRANSLATE > /dev/null`"

	# prefix not empty or 'type' is empty (no errors) => OK
	test -n "$prefix" && echo "PREFIX" && return
	test -z "$func" && echo "FUNCTION" && return
	
	echo "NO" && return
}
