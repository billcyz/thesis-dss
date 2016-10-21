#!/bin/bash
# Startup script to start whole dss system,
# start dss server & dss node

# Basic Help command
display_help()
{
	echo "DSS, Version - [1.0]; Author - [AUT Team]"
	echo "Date - [21/10/2016]"
	echo
	echo "Usage: `basename $0` [OPTIONS]"
	echo "	start		start DSS system"
	echo "	stop		stop DSS System"
	echo "	update		update DSS System"
	echo "	init		initialize DSS System"
	echo "	manage_server add 		add server nodes to system"
	echo "	manage_server remove	remove server nodes in system"
	echo "	manage_server edit		alter server nodes status"
	echo "	manage_server check		check system status information"
	exit 0
}

# Initialize function
init_dss()
{
	# Check pre-requirements: erlang
	erl_status=`which erl`
	if [[ -z "$erl_status" ]]
	then
		echo "No required Language detected"
		# Install ErLang
		sudo apt-get -y install erlang
	else
		echo "Detected required language"
		#touch src/abc
		#touch ../test/abc
	fi

	echo "Start DSS system..."
	hostName=`hostname`
	#echo "The hostname is $hostName"
}

# Startup function
start_dss() 
{
	echo "start_dss function"
	erl_status=`which erl`
	if [[ -z "$erl_status" ]]
	then
		echo "ErLang is not detected, please run ./start.sh init to initialize the system"
		exit 0
	else
		# Start system
		echo "Start system"
		# Compile erl files (compilation and make in erlang)
	fi
}

dss_add()
{
	echo "dss_add function"
}

# Start function based on arguments
case "$1" in
	-h | --help)
		display_help
	;;
	start)
		start_dss
	;;
	stop)
		stop_dss
	;;
	init)
		init_dss
	;;
	update)
		update_dss
	;;
	manage_server)
		case "$2" in
			add)
				dss_add
			;;
		esac
	;;
esac



# Start DSS node
#`which erl` -sname $hostName

# Start DSS server
