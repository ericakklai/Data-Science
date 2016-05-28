#!/usr/bin/env python

import sys
(last_key, max_val) = (None, -sys.maxint)
count = 0
for line in sys.stdin:
  (key, val) = line.strip().split("\t")
  if last_key and last_key != key:
    print "%s\t%s" % (last_key, count)
    (last_key, count) = (key, 1)
  else:
    last_key = key
    count = count + 1
    
if last_key:
      print "%s\t%s" % (last_key, count)
