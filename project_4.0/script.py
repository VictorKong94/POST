from __future__ import absolute_import, division, print_function
import numpy as np
import os, sys

# Variables to be defined by user
file = input("Drop txt file of SDS output here:")
layout = input("Drop csv file of qPCR plate layout here:")

# Read in names of genes and samples


# Read in data file exported from SDS software
data = np.genfromtxt(file, dtype=str, skip_header=11, usecols=(0))
