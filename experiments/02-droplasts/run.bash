#!/usr/bin/env bash

DATE=$(date --iso-8601=seconds)
DIR="data-$DATE"
EXAMPLES=2
CLAUSES=3
PREDS=15
TRIALS=10

mkdir -p $DIR
for i in $(seq 0 $PREDS); do
    for j in $(seq $TRIALS); do
        python3 gen_test.py $CLAUSES $i $DIR/prog-$CLAUSES-$i-$j-untyped.pl $DIR/prog-$CLAUSES-$i-$j-typed.pl
        echo "print_pos_examples($EXAMPLES)." |  swipl gen_examples.pl  2>/dev/null| grep 'p(' | tee -a $DIR/prog-$CLAUSES-$i-$j-untyped.pl >> $DIR/prog-$CLAUSES-$i-$j-typed.pl
    done
done

mkdir $DIR-results
DATATYPED=data-typed-$DATE.csv
DATAUNTYPED=data-untyped-$DATE.csv
echo clauses,preds,trial,typed,time >>$DATATYPED
echo clauses,preds,trial,typed,time >>$DATAUNTYPED
for i in $(seq 0 $PREDS); do
    for j in $(seq $TRIALS); do
        echo -n $CLAUSES,$i,$j,1, >>$DATATYPED
        echo -n $CLAUSES,$i,$j,0, >>$DATAUNTYPED
        echo run. | swipl $DIR/prog-$CLAUSES-$i-$j-typed.pl 2>/dev/null | tee $DIR-results/result-$CLAUSES-$i-$j-typed.txt
       	sed -n "s/%data,time,\([0-9.]*\)/\1/p" <$DIR-results/result-$CLAUSES-$i-$j-typed.txt >>$DATATYPED
        echo run. | swipl $DIR/prog-$CLAUSES-$i-$j-untyped.pl 2>/dev/null | tee $DIR-results/result-$CLAUSES-$i-$j-untyped.txt
       	sed -n "s/%data,time,\([0-9.]*\)/\1/p" <$DIR-results/result-$CLAUSES-$i-$j-untyped.txt >>$DATAUNTYPED
	sleep 1
    done
done

echo $DATE
