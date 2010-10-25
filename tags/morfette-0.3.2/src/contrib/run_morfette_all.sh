#!/bin/sh

if test $1 == "-h" ; then
	echo "run_morfette_all.sh [-d lexicon] TRAIN TEST GOLD [SUFF]"
    exit 0
fi


if test $1 == "-d" ; then
	shift
	KK=`basename $1`
	KK="+$KK"
	LEXICON="-d $1"
	shift
else
	LEXICON=""
fi	

TRAIN=$1
TEST=$2
GOLD=$3
SUFF=$4

MODEL="$TRAIN.model$KK$SUFF"


morfette_train.sh $LEXICON $TRAIN $MODEL
morfette_run_eval.sh $MODEL $TEST $GOLD $TRAIN
 
