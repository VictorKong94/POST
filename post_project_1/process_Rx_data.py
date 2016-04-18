##############################
# Compatibility Arrangements #
##############################

from __future__ import absolute_import, division, print_function
from builtins import *
import numpy as np

FILE = input('Enter filepath to prescription data: ')
assert FILE.endswith('.csv'), 'File type must be .csv'

###################
# Data Extraction #
###################

# Method search() returns the index of the first element containing a string
def search(string, list):
    return [string in s for i, s in enumerate(list)].index(True)

# Import and analyze header from the raw data
header = open(FILE, 'r').readline().split(',')
IDs_col = search('id', header)
days_col = search('days', header)
gennm_col = search('gennm', header)
mgs_col = search('mgs', header)
rxdose_col = search('rxdose', header)
begin = min(days_col, gennm_col, mgs_col, rxdose_col)
end = search('tot', header)

# Prepare for extraction of important data
Rx_cols = [index for index in range(begin, end) if index % 4 != gennm_col % 4]
drug_cols = np.setdiff1d(range(begin, end), Rx_cols)

# Indices of third dimension in `Rx` containing `mgs` and `days`
mgs = sorted([days_col, mgs_col, rxdose_col]).index(mgs_col)
days = sorted([days_col, mgs_col, rxdose_col]).index(days_col)

# Extract Rx from file to three objects:
# - `IDs`: contains subjects' unique ID numbers
# - `Rx`: prescription data where first dimension corresponds to subject and
#         second dimension corresponds to visit
# - `drug`: array of strings that correspond to prescribed statin
IDs = np.genfromtxt(FILE, dtype=int, delimiter=',', skip_header=1,
                    usecols=IDs_col)
Rx = np.genfromtxt(FILE, delimiter=',', skip_header=1, filling_values=0,
                   usecols=Rx_cols).reshape(len(IDs), -1, 3)
drug = np.genfromtxt(FILE, dtype='U2', delimiter=',', skip_header=1,
                     usecols=drug_cols)

# Scale dosages by DDD of different drugs
scaling = np.zeros_like(Rx[:,:,mgs])
scaling[(drug=='at') + (drug=='At') + (drug=='AT')] = 1/20
scaling[(drug=='fl') + (drug=='Fl') + (drug=='FL')] = 1/160
scaling[(drug=='lo') + (drug=='Lo') + (drug=='LO')] = 1/80
scaling[(drug=='pi') + (drug=='Pi') + (drug=='PI')] = 1/4
scaling[(drug=='pr') + (drug=='Pr') + (drug=='PR')] = 1/80
scaling[(drug=='ro') + (drug=='Ro') + (drug=='RO')] = 1/5
scaling[(drug=='si') + (drug=='Si') + (drug=='SI')] = 1/40
Rx[:,:,mgs] = Rx[:,:,mgs] * scaling

# Compute frequencies of visits to pharmacist (units: number of times)
Visits = (Rx[:,:,days] != 0).sum(1)

# Compute changes in prescription over time
daily_Rx = Rx[:,:,np.setdiff1d(range(0, 2), days)].prod(2)
delta_Rx = np.hstack((daily_Rx[:,range(1, daily_Rx.shape[1])] -
                      daily_Rx[:,range(0, daily_Rx.shape[1]-1)],
                      np.zeros((daily_Rx.shape[0], 1))))
delta_Rx[range(daily_Rx.shape[0]),Visits-1] = 0

#########################
# Variables of Interest #
#########################

# Compute PDD:DDD ratio by subject (units: DDD/day)
PDD_per_DDD = Rx.prod(2).sum(1) / Rx[:,:,days].sum(1)

# Compute frequencies of prescription changes (units: number of times)
Rx_Change = (delta_Rx != 0).sum(1)
Rx_Increase = (delta_Rx > 0).sum(1)
Rx_Decrease = (delta_Rx < 0).sum(1)
Total_Change = daily_Rx[range(daily_Rx.shape[0]),Visits-1] - daily_Rx[:,0]
Rx_Change_Denominator = Rx_Change
Rx_Change_Denominator[Rx_Change == 0] = 1
Ratio_Inc_Change = Rx_Increase / Rx_Change_Denominator

# Compute frequencies of drugs prescribed (units: number of days)
# Assumes drugs of interest are statins
Atorva = (Rx[:,:,days] * np.any((drug=='at', drug=='At', drug=='AT'), 0)).sum(1)
Fluva = (Rx[:,:,days] * np.any((drug=='fl', drug=='Fl', drug=='FL'), 0)).sum(1)
Lova = (Rx[:,:,days] * np.any((drug=='lo', drug=='Lo', drug=='LO'), 0)).sum(1)
Pitava = (Rx[:,:,days] * np.any((drug=='pi', drug=='Pi', drug=='PI'), 0)).sum(1)
Prava = (Rx[:,:,days] * np.any((drug=='pr', drug=='Pr', drug=='PR'), 0)).sum(1)
Rosuva = (Rx[:,:,days] * np.any((drug=='ro', drug=='Ro', drug=='RO'), 0)).sum(1)
Simva =(Rx[:,:,days] * np.any((drug=='si', drug=='Si', drug=='SI'), 0)).sum(1)
drugs = np.vstack((Atorva, Fluva, Lova, Pitava, Prava, Rosuva, Simva)).T
Main_Drug = np.array(['Atorva', 'Fluva', 'Lova', 'Pitava', 'Prava', 'Rosuva',
                      'Simva'])[drugs.argmax(1)]

# Save data to plaintext file
Head = np.array(['IDs', 'PDD/DDD', 'Number of Visits', 'PDD/DDD Changes',
                 'PDD/DDD Increases', 'PDD/DDD Decreases',
                 'Total PDD/DDD Change', 'Increase/Change Ratio', 'Atorva',
                 'Fluva', 'Lova', 'Pitava', 'Prava', 'Rosuva', 'Simva',
                 'Primary Drug'])
data = np.vstack((Head, np.vstack((IDs, PDD_per_DDD, Visits, Rx_Change,
                                   Rx_Increase, Rx_Decrease, Total_Change,
                                   Ratio_Inc_Change, drugs.T, Main_Drug)).T))
filename = FILE.split('.')[0] + '_processed.' + FILE.split('.')[1]
np.savetxt(filename, data, fmt='%s', delimiter=',')
