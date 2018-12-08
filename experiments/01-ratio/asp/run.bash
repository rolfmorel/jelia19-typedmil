#!/usr/bin/env bash

DATE=$(date --iso-8601=seconds)
DIR="data-$DATE"
PREDS=20
TRIALS=10
CLAUSES=3

mkdir -p $DIR
for i in $(seq 0 $PREDS); do
    for j in $(seq $TRIALS); do
        python gen_test.py $CLAUSES $i $PREDS typed > $DIR/prog-$CLAUSES-$i-$PREDS-$j-typed.hex
        python gen_test.py $CLAUSES $i $PREDS untyped > $DIR/prog-$CLAUSES-$i-$PREDS-$j-untyped.hex
    done
done

mkdir -p $DIR-results
DATATYPED=data-typed-$DATE.csv
DATAUNTYPED=data-untyped-$DATE.csv
echo clauses,total,well,trial,typed,time >>$DATATYPED
echo clauses,total,well,trial,typed,time >>$DATAUNTYPED
for i in $(seq 0 $PREDS); do
    for j in $(seq $TRIALS); do
        echo -n $CLAUSES,$PREDS,$i,$j,1, >>$DATATYPED
        echo -n $CLAUSES,$PREDS,$i,$j,0, >>$DATAUNTYPED
        /usr/bin/time -p hexlite $DIR/prog-$CLAUSES-$i-$PREDS-$j-typed.hex 2>&1 | tee $DIR-results/result-$CLAUSES-$i-$PREDS-$j-typed.txt
        sed -n "s/real \([0-9.]*\)/\1/p" <$DIR-results/result-$CLAUSES-$i-$PREDS-$j-typed.txt >>$DATATYPED
        /usr/bin/time -p hexlite $DIR/prog-$CLAUSES-$i-$PREDS-$j-untyped.hex 2>&1 | tee $DIR-results/result-$CLAUSES-$i-$PREDS-$j-untyped.txt
        sed -n "s/real \([0-9.]*\)/\1/p" <$DIR-results/result-$CLAUSES-$i-$PREDS-$j-untyped.txt >>$DATAUNTYPED
    done
done

echo $DATE
