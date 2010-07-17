#!/bin/bash
PREVIOUS=0.0.4
NEXT=0.0.5

while read p; do
  echo "Changing $p"
  sed -i s/$PREVIOUS/$NEXT/g $p
done < scripts/version-list.txt

git diff