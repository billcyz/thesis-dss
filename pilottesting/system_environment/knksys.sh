#!/bin/bash
# Shell interface for KnK system

# read config file and get variables -> source knk.cnf

# Basic Help command
help()
{
	echo "----- KiNgaKopuku System (KNK) -----"
	echo "-AUT-"
	echo ""
	echo "Usage: `basename $0` [OPTIONS]"
	printf "%-40s %s\n" "help :" "........ System help information"
	printf "%-40s %s\n" "dev :" "........ Compile system code"
	printf "%-40s %s\n" "start :" "........ Start system"
	printf "%-40s %s\n" "stop :" "........ Stop system"
	printf "%-40s %s\n" "check :" "........ Check system"
	
	echo "-----------------------------------------------------------"
	
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
		echo "[`date +%Y-%m-%d~%T`] Error: No required language detected"
	else
		echo "[`date +%Y-%m-%d~%T`] Info: Detected required language"
		`which make`
	fi
}

# Check required language
check_erl()
{
	erl_status=`which erl`
	if [[ -z "$erl_status" ]] 
	then
		# erl is not ready
		echo "erl_0"
	else
		# erl is ready
		echo "erl_1"
	fi
}

check_erl_call()
{
	erl_call_status=`which erl_call`
	if [[ -z "$erl_call_status" ]]
	then
		# erl_call is not ready
		echo "erl_call_0"
	else
		# erl_call is ready
		echo "erl_call_1"
	fi
}

check_file_exis()
{
	target_file=$1
	if [ -f $target_file ]
	then
		echo "has_file"
	else
		echo "no_file"
	fi
}

# Start function
start_knk()
{
	# Check required language
	erl_result=$(check_erl)
	erl_call_result=$(check_erl_call)
	if [[ "$erl_result" == "erl_0" ]]
	then
		echo "[`date +%Y-%m-%d~%T`] Error: ErLang is not ready. Please use 'apt-get install erlang' to install Erlang"
		exit
	else
		if [[ "$erl_call_result" == "erl_call_0" ]]
		then
			echo "[`date +%Y-%m-%d~%T`] Error: erl_call is not ready"
			exit
		else
			# check config file knk.cnf existence
			RESULT=$(check_file_exis knk.cnf)
			if [[ "$RESULT" == "has_file" ]]
			then
				echo "[`date +%Y-%m-%d~%T`] Info: ErLang is ready"
				echo "[`date +%Y-%m-%d~%T`] Info: Reading config file knk.cnf"
				source knk.cnf
				# check log file knk.log existence
				RESULT=$(check_file_exis $log_dir)
				if [[ "RESULT" == "has_file" ]]
				then
					echo "[`date +%Y-%m-%d~%T`] Info: Logging file exists"
					
				else
					# bring system output into user defined debug information
					RESULT=$(touch $log_dir &> /dev/null ; echo $?)
					
				fi
				# Get ip address and Mac address of host
				server_ip=$()
				# Get system current timestamp
				server_cur_time=`date +'%s'`
				echo "[`date +%Y-%m-%d~%T`] Info: Ready to start KNK system...."
				# Start KNK system
				# run KNK start file
				#`which escript` sys_interface start
			
			
				# Start knk_main NODE (should use -name option), can use ip address or the hostname.
				# reads the $HOME/.erlang.cookie file to set up the cookie
				`which erl` -name knk_main@$ip -detached
				# Start knk_twin NODE (hidden node)
				#`which erl` -setcookie knktwin -sname knk_twin@localhost -detached
			else
				echo "Can't find knk config file (knk.cnf)..."
				exit
			fi
		fi
	fi

	

	echo "Start knk system"
	
	# Start KNK system node
	`which erl_call` -s -c abc -sname knk_main
	# call function in erlang node
	#erl_call -a 'erlang time' -c abc -sname knk_main
	# Start KNK system twin node. It is used for monitoring and data backup.
	`which erl_call` -hidden -name knk_twin
	# Current system time in epoch format
	sys_time=`date +%s`
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
		start_knk
	;;
	stop)
		stop_knk
	;;
	check)
		check_knk
	;;
esac