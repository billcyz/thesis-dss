#!/bin/bash
# Shell interface for KnK system

# Basic Help command
help()
{
	echo "----- KiNgaKopuku System (KNK) -----"
	echo "-AUT-"
	echo ""
	echo "Usage: `basename $0` [OPTIONS]"
	echo "	help	System help information"
	echo "	dev	Compile system code"
	echo "	start	Start system"
	echo "	stop	Stop system"
	echo "	check	Check system"
	exit 0
}

# Development function
compile_knk()
{
	# check pre-requirements: erlang
	erl_status=`which erl`
	if [[ -z "$erl_status" ]]
	then
		echo "No required language detected"
	else
		echo "Detected required language"
		`which make`
	fi
}

# Start function
start_knk()
{
	num=$1
	echo $num
	echo "Start knk system"
}

# Stop function
stop_knk()
{
	echo "Stop knk system"
}

# Check function
check_knk()
{
	echo "Check knk system"
}

case "$1" in
	-h | help)
		help
	;;
	dev)
		compile_knk
	;;
	start)
		start_knk 3
	;;
	stop)
		stop_knk
	;;
	check)
		check_knk
	;;
esac