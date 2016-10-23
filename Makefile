## Draft version of makefile of dss system
## reference website: https://github.com/fogfish/makefile/blob/master/Makefile
## reference website: https://pdincau.wordpress.com/2010/12/20/how-to-manage-your-erlang-software-with-a-simple-makefile/

ERLC=$(shell which erlc)
ERLCFLAGS=-o
SRCDIR=src
##LOGDIR=/var/log/mysoftware
LOGDIR=$(SRCDIR)/mysoftware
CONFDIR=/etc/mysoftware
BEAMDIR=./ebin

all:
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
	@ mkdir -p $(CONFDIR) ;
	@ mkdir -p $(LOGDIR) ;
	@ cp conf/mysoftware.conf $(CONFDIR)/mysoftware.conf-example

clean: 
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crush.dump
	
