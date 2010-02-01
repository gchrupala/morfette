#!/bin/sh

if test $1 == "-h" ; then
        echo "morfette_run_eval.sh [-h]  MODELDIR TEST GOLD TRAIN"
        echo "where MODEL is the trained model" 
        echo "TEST is the file to be used for prediction (doesn't matter if it's raw or not) "
        echo "GOLD the gold standard file"
        echo "TRAIN the file used for training, here used to calculate unknown words accuracy"
        exit 0
fi

MODEL=$1
TEST=$2
GOLD=$3
TRAIN=$4

SUFFIXE=`basename $MODEL`
OUTFILE="$TEST.$SUFFIXE"

# run prediction

echo "(Prediction) running cat $TEST | morfette predict $MODEL > $OUTFILE" > /dev/stderr
cat $TEST | morfette predict $MODEL > $OUTFILE


# run evaluation
echo "(Evaluation) running morfette eval --ignore-case $TRAIN $TEST $OUTFILE | tee $OUTFILE.morfette.eval" > /dev/stderr
morfette eval --ignore-case $TRAIN $TEST $OUTFILE | tee $OUTFILE.morfette.eval



 