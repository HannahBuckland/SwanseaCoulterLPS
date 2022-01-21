# Script to change file extension of LPS data files
# Hannah Buckland
# 21/01/2022


import os
import sys
folder = '../data/20220119/'

for filename in os.listdir(folder):
  infilename = os.path.join(folder,filename)
  base_file, ext = os.path.splitext(filename)
  if ext == ".xls":
    output = os.rename(infilename, infilename.replace('.$ls.xls', '.txt'))
  
