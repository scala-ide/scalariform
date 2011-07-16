#!/bin/bash
cd `dirname $0`/..
cloc --exclude-dir=src_managed,target,docs '--exclude-lang=XML,XSLT,HTML,CSS,Bourne Shell' .
