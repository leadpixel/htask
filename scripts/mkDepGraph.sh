#!/usr/bin/env zsh

echo $PWD

mkdir "docs"
DOT="$PWD/docs/deps.dot"
JSON="$PWD/docs/deps.json"

echo "{" > $JSON

for k in ./packages/**/*.hs ; do
  MODULE=$(cat $k | grep "module" | sed "s/(.*$//" | sed "s/module \(\S\+\).*/\1/")
  DATA=$(cat $k | grep "import" | sed "s/qualified //" | sed "s/import\s\+/import /" | sed "s/\sas .*$//" | sed "s/(.*$//" | sed "s/import \(\S\+\).*/\1/" | sed 's/^\(.*\)$/"\1",/g')
  echo "\"$MODULE\": [ $DATA ]," >> $JSON
  echo $DATA >> $DOT
done

sed -i 's/, ]/ ]/' $JSON
sed -i 's/\],\n}/]/' $JSON

echo "}" >> $JSON

echo "strict digraph deps {" > $DOT
sed -i '/Control/d' $DOT
sed -i '/Options/d' $DOT
sed -i '/Generic/d' $DOT
sed -i '/Data/d' $DOT
# echo "}" >> $DOT

# sed -i 's/^import \(\S*\)\s*/  "PREV" -> "\1";/' $DOT
# sed -i 's/^module \(\S*\)\s*/  "\1" [style="solid"];/' $DOT
