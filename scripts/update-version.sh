#!/bin/bash
PREVIOUS=0.0.6
NEXT=0.0.7

while read p; do
  echo "Changing $p"
  sed -i s/$PREVIOUS/$NEXT/g $p
done < scripts/version-list.txt

git diff
