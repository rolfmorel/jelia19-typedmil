#!/usr/bin/env bash

DATE=$(date --iso-8601=seconds)
DIR="data-$DATE"
EXAMPLES=2
NEG_EXAMPLES=2
CLAUSES=3
PREDS=20
TRIALS=10

mkdir -p $DIR
for i in $(seq 0 $PREDS); do
    for j in $(seq $TRIALS); do
        python3 gen_test.py $CLAUSES $i $DIR/prog-$CLAUSES-$i-$j-untyped.hex $DIR/prog-$CLAUSES-$i-$j-typed.hex
	#echo 'pos_ex(f,(("a",("b",("c",()))),(("d",("e",("g",()))),())), (("a",("b",())),(("d",("e",())),()))).' >> $DIR/prog-$CLAUSES-$i-$j-untyped.hex
	#echo 'pos_ex_t(f,(list(list(char)),list(list(char))),(("a",("b",("c",()))),(("d",("e",("g",()))),())), (("a",("b",())),(("d",("e",())),()))).'  >> $DIR/prog-$CLAUSES-$i-$j-typed.hex

	echo "print_pos_ex_asp_both($EXAMPLES)." | swipl gen_examples.pl 2>/dev/null | grep 'pos_ex\|neg_ex' | sed "s/l(/(/g;s/'/\"/g" | tee -a $DIR/prog-$CLAUSES-$i-$j-untyped.hex >> $DIR/prog-$CLAUSES-$i-$j-typed.hex
    done
done 
exit

mkdir -p $DIR-results
DATATYPED=data-typed-$DATE.csv
DATAUNTYPED=data-untyped-$DATE.csv
echo clauses,preds,trial,typed,time >>$DATATYPED
echo clauses,preds,trial,typed,time >>$DATAUNTYPED
for i in $(seq 0 $PREDS); do
    for j in $(seq $TRIALS); do
        echo -n $CLAUSES,$i,$j,1, >>$DATATYPED
        echo -n $CLAUSES,$i,$j,0, >>$DATAUNTYPED
	/usr/bin/time -p timeout --signal=KILL 600 hexlite $DIR/prog-$CLAUSES-$i-$j-typed.hex --flpcheck=none -n=1 --plugin lists --pluginpath . 2>&1 | tee >(sed -n "s/real \([0-9.]*\)/\1/p" >>$DATATYPED) | python model-to-pprint.py | tee $DIR-results/result-$CLAUSES-$i-$j-typed.txt
#        sed -n "s/real \([0-9.]*\)/\1/p" <$DIR-results/result-$CLAUSES-$i-$j-typed.txt >>$DATATYPED
	/usr/bin/time -p timeout --signal=KILL 600 hexlite $DIR/prog-$CLAUSES-$i-$j-untyped.hex --flpcheck=none -n=1 --plugin lists --pluginpath . 2>&1 | tee >(sed -n "s/real \([0-9.]*\)/\1/p" >>$DATAUNTYPED) | python model-to-pprint.py | tee $DIR-results/result-$CLAUSES-$i-$j-untyped.txt
#        sed -n "s/real \([0-9.]*\)/\1/p" <$DIR-results/result-$CLAUSES-$i-$j-untyped.txt >>$DATAUNTYPED
    done
done

echo $DATE
