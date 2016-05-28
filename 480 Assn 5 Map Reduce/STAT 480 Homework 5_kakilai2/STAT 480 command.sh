# Command line code segments for STAT 480 Homework 5
# Ka Ki Lai (kakilai2)
# Mar 7, 2016

# Code reference from lecture resources
# Hadoop: The Definitive Guide, Fourth Edition by Tom White
# covered in the Spring 2016 section of 
# Stat 480: Data Science Foundations at the University of 
# Illinois at Urbana-Champaign
#
# The base code in hadoop-book-4e is from Tom White's github, 
# and modifications are by Darren Glosemeyer, based on modifications 
# to the third edition code made by Darren Glosemeyer and James Balamuta
# for the pilot offering as Stat 430: Big Data Analysis Foundations in Spring 2015
# and updated for Hadoop version 2 and the new environment (Cloudera's quickstart container) 
# being used for the Spring 2016 offering of Stat 480.
#

docker exec -i -t <container ID> bash 

# Switch to cloudera user when working in the container.
su cloudera

# The following assumes that the Hadoop Book files have already been 
# unzipped and are available on the quickstart container in 
# /home/host-data/hadoop-book-4e .
cd /home/host-data

# In the /home/host-data/ directory, make directory where we will copy and 
# modify Hadoop Book code and data. We will work directly in the files in 
# hb-workspace so we can maintain the original files as a backup.
mkdir hb-workspace

# Change to hadoop book directory. 
cd hadoop-book-4e



#  and input files from hadoop-book-4e 
# to the hb-workspace directory.
cp -r ch02-mr-intro input /home/host-data/hb-workspace

# Change to hb-workspace directory 
cd /home/host-data/hb-workspace

# Change directory further to a directory containing 
# the Unix Tools script. 
cd ch02-mr-intro/src/main/awk 

# View the files in the current directory
ls


# 
# Also add a comment at the top of the file noting the 
# original source and the modification made
vi max_temperature.sh

# When done, press the Esc key to turn off insert mode, then
# type :wq and press enter to save changes and exit the file.


# Run the modified max temperature script.
# Note: only the 1901 and 1902 data is included 
# in the input data files.
./max_temperature.sh


# Make a directory structure on HDFS where we will put our data.
# This will be placed in your home directory on HDFS. 
hadoop fs -mkdir -p input/ncdc/all


# The default directory shown is your home directory, 
# so following is the same as previous. Change the user name to your 
# user name before executing if you are a different user.
hadoop fs -ls /user/cloudera

# This should show the file we just copied to the distributed file system.
hadoop fs -ls input/ncdc

#######################################################################################

# p1

hadoop jar /usr/lib/hadoop-mapreduce/hadoop-streaming.jar \
  -files /home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py,\
/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/min_temperature_reduce.py \
  -input input/ncdc/all \
  -output p1outputpy \
  -mapper "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py" \
  -reducer "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/min_temperature_reduce.py" 


# See result files.
hadoop fs -ls p1outputpy

# View results.
hadoop fs -cat p1outputpy/part*


#######################################################################################
#p2
hadoop jar /usr/lib/hadoop-mapreduce/hadoop-streaming.jar \
  -files /home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py,\
/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/count.py \
  -input input/ncdc/all \
  -output p2outputpy \
  -mapper "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py" \
  -reducer "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/count.py" 


# See result files.
hadoop fs -ls p2outputpy

# View results.
hadoop fs -cat p2outputpy/part*

#######################################################################################

#p3

hadoop jar /usr/lib/hadoop-mapreduce/hadoop-streaming.jar \
  -files /home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py,\
/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/combine.py \
  -input input/ncdc/all \
  -output p3outputpy \
  -mapper "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py" \
  -reducer "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/combine.py" 


# See result files.
hadoop fs -ls p3outputpy

# View results.
hadoop fs -cat p3outputpy/part*

hadoop fs -rm -r -f p3outputpy


#######################################################################################
#p4

hadoop jar /usr/lib/hadoop-mapreduce/hadoop-streaming.jar \
  -files /home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py,\
/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/findmean.py \
  -input input/ncdc/all \
  -output p4outputpy \
  -mapper "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/data_map.py" \
  -reducer "/home/host-data/hb-workspace/ch02-mr-intro/src/main/python/findmean.py" 


# See result files.
hadoop fs -ls p4outputpy

# View results.
hadoop fs -cat p4outputpy/part*

#hadoop fs -rm -r -f p4outputpy

#######################################################################################
# To get the files out of the container
hadoop fs -put ./ch02-mr-intro/src/main/python/data_map.py
hadoop fs -put ./ch02-mr-intro/src/main/python/min_temperature_reduce.py
hadoop fs -put ./ch02-mr-intro/src/main/python/count.py
hadoop fs -put ./ch02-mr-intro/src/main/python/combine.py
hadoop fs -put ./ch02-mr-intro/src/main/python/findmean.py



