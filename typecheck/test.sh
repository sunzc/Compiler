#!/bin/sh

# diff(arg1, arg2)
# arg1: input filename
# arg2: output filename
diff_demo_and_out() {
			outfname=`echo $1 | sed 's/in/out/g'`
			demofname=`echo $1 | sed 's/in/demo/g'`
			#echo "./demo $1 > $demofname 2>&1"
			./demo $1 > $demofname 2>&1
			echo "==== $1 ===" >> $2
			diff -w $demofname $outfname >> $2
			echo "" >> $2
}

# regression_test(arg1, arg2)
# arg1: input, like in02 
# arg2: output filename, like diff_res
regression_test() {
	echo "" > $2
	for x in `ls $3/in0*`
	do
		if [ "$x" != "$3/$1" ]
		then
			#echo "diff_demo_and_out $x $2 "
			diff_demo_and_out $x $2 
		else
			#echo "diff_demo_and_out $x $2 "
			diff_demo_and_out $x $2 
			break
		fi
	done
}


regression_test $1 diff_res test
cat diff_res
