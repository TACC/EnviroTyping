#! /bin/bash

#read csv filenames into an array and them pass them to a file ready for launcher module on stampede2

# change to directory with csv's
mkdir randcsv
mkdir output
mkdir error

Rscript ./randcsvGen.R

cd randcsv/

i=0
while read line
do
    array[ $i ]="$line"
    (( i++ ))
done < <(ls)


for i in "${array[@]}"
do
    # substitues csv filename for $i and sends output and error from R to respective files
    echo "Rscript --vanilla --verbose headDF.R > output/$i.Rout 2> error/$i.Rerr randcsv/$i"
done > ../launcherFile
