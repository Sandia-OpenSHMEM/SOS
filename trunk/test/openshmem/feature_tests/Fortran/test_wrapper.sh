#!/bin/sh

a=`$*`
b=$?
echo "$a"
if test $b -eq 0 ; then
  if test "`echo $a | grep 'more PE'`" != "" ; then
    exit 77
  elif test "`echo $a | grep 'skipped'`" != "" ; then
    exit 77
  elif test "`echo $a | grep Passed`" != "" ; then
    exit 0
  else
    exit 1
  fi
else
  exit $b
fi
