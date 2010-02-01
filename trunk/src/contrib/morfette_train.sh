#!/bin/sh


#  morfette train --gaussian-prior=1 --dict-file=../lexicon.ftb4.4morfette.csv train train-model+lexicon
if test $1 == "-h" ; then
        echo "morfette_train.sh [-h] [-g Gaussian-Prior] [-d lexicon] TRAINING_FILE TRAINED_MODEL"
        echo "where Guaussion-Prior is a real value from 0 to infinity used for smoothing (default 1)" 
        echo "lexicon is a lexicon used for unseen words, with format FORM \"LEMMA1 MORPHO-TAG1 LEMMA2 MORPHO-TAG2\" "
        echo "TRAINING_FILE is the file used for training, format : \"form	lemma	pos\""
        echo "TRAINED_MODEL is the resulting model"
        exit 0
fi

if test $1 == "-g" ; then
	shift
	GAUSSIAN="--gaussian-prior=$1"
	shift
else
	GAUSSIAN="--gaussian-prior=1"
fi

if test $1 == "-d" ; then
	shift
	LEXICON="--dict-file=$1"
	shift
else
	LEXICON=""
fi	


TRAININGFILE=$1
MODEL=$2


echo "(Training) running morfette train $GAUSSIAN $LEXICON $TRAININGFILE $MODEL" > /dev/stderr

morfette train $GAUSSIAN $LEXICON $TRAININGFILE $MODEL
 