# Script to change file extension of LPS data files
# Hannah Buckland
# 21/01/2022
# modified 25/02/2022


import os
import sys
import shutil

src_dir = os.getcwd() # gets current working directory

dest_dir = "Coulter_files" # set name of new directory

shutil.copytree(src_dir,dest_dir)

for filename in os.listdir(src_dir):
  infilename = os.path.join(src_dir,filename)
  base_file, ext = os.path.splitext(filename)
  if ext == ".xls":
    output = os.rename(infilename, infilename.replace('.$ls.xls', '.txt'))
  

