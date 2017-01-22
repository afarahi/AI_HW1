#!/bin/sh

foo=`cat ./master_lens.csv`
# echo "$foo"

# Delete a pattern
foo=`echo "$foo" | sed 's/" ""//g'`
foo=`echo "$foo" | sed 's/"""//g'`
foo=`echo "$foo" | sed 's/ NaN//g'`
echo "$foo"

# Delete all spaces in front of every line of foo
foo=`echo "$foo" | sed 's/^[ ^t]*//'`
echo "$foo" > master_lens_clean.csv

