#!/usr/bin/env python

import sys
(last_key, count, mean) = (None, 0.0, 0.0)

for line in sys.stdin:
  (key, val) = line.strip().split("\t")
  val = float(val)
  if last_key and last_key != key:
    print "%s\t%f" % (last_key, mean)
    (last_key, mean, count) = (key, val, 1.0)
  else:
    (last_key, mean) = (key, mean * (count/(count+1)) + val/(count + 1))
    count = count + 1
    
if last_key:
      print "%s\t%f" % (last_key, mean)
