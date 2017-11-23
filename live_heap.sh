#!/bin/sh
head -`fgrep -n END_SAMPLE cs4223-as2-exe.hp | tail -1 | cut -d : -f 1` cs4223-as2-exe.hp \
  | hp2ps > cs4223-as2-exe.ps
gv cs4223-as2-exe.ps &
gvpsnum=$!
while [ 1 ] ; do
  sleep 10
  head -`fgrep -n END_SAMPLE cs4223-as2-exe.hp | tail -1 | cut -d : -f 1` cs4223-as2-exe.hp \
    | hp2ps > cs4223-as2-exe.ps
  kill -HUP $gvpsnum
done
