#!/bin/bash
PREVIOUS=0.0.8
NEXT=0.0.9

while read p; do
  echo "Changing $p"
  sed -i s/$PREVIOUS/$NEXT/g $p
done < scripts/version-list.txt

git diff
