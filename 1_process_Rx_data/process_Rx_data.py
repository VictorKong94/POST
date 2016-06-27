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

# Standardize drug names
Drugs_dict = {'AT': 'Atorva', 'FL': 'Fluva', 'LO': 'Lova', 'PI': 'Pitava',
              'PR': 'Prava', 'RO': 'Rosuva', 'SI': 'Simva'}
drug = drug.astype('U6')
for i in range(drug.shape[0]):
    for j in range(drug.shape[1]):
        drug[i,j] = drug[i,j].upper()
for abbrev, full_name in Drugs_dict.items():
    for i in range(drug.shape[0]):
        for j in range(drug.shape[1]):
            drug[i,j] = drug[i,j].replace(abbrev, full_name)

# Scale dosages by DDD of different drugs
scaling = np.zeros_like(Rx[:,:,mgs])
scaling[drug=='Atorva'] = 1/20
scaling[drug=='Fluva'] = 1/160
scaling[drug=='Lova'] = 1/80
scaling[drug=='Pitava'] = 1/4
scaling[drug=='Prava'] = 1/80
scaling[drug=='Rosuva'] = 1/5
scaling[drug=='Simva'] = 1/40
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
PDD_per_DDD_factor = np.array(['[6-inf)'] * len(PDD_per_DDD), dtype='U11')
PDD_per_DDD_factor[PDD_per_DDD < 6.00] = '[5.00-6.00)'
PDD_per_DDD_factor[PDD_per_DDD < 5.00] = '[4.00-5.00)'
PDD_per_DDD_factor[PDD_per_DDD < 4.00] = '[3.50-4.00)'
PDD_per_DDD_factor[PDD_per_DDD < 3.50] = '[3.00-3.50)'
PDD_per_DDD_factor[PDD_per_DDD < 3.00] = '[2.50-3.00)'
PDD_per_DDD_factor[PDD_per_DDD < 2.50] = '[2.00-2.50)'
PDD_per_DDD_factor[PDD_per_DDD < 2.00] = '[1.75-2.00)'
PDD_per_DDD_factor[PDD_per_DDD < 1.75] = '[1.50-1.75)'
PDD_per_DDD_factor[PDD_per_DDD < 1.50] = '[1.25-1.50)'
PDD_per_DDD_factor[PDD_per_DDD < 1.25] = '[1.00-1.25)'
PDD_per_DDD_factor[PDD_per_DDD < 1.00] = '[0.75-1.00)'
PDD_per_DDD_factor[PDD_per_DDD < 0.75] = '[0.50-0.75)'
PDD_per_DDD_factor[PDD_per_DDD < 0.50] = '[0.25-0.50)'
PDD_per_DDD_factor[PDD_per_DDD < 0.25] = '[0.00-0.25)'

# Compute frequencies of prescription changes (units: number of times)
Rx_Change = (delta_Rx != 0).sum(1)
Rx_Increase = (delta_Rx > 0).sum(1)
Rx_Decrease = (delta_Rx < 0).sum(1)
Total_Change = daily_Rx[range(daily_Rx.shape[0]),Visits-1] - daily_Rx[:,0]
Rx_Change_Denominator = Rx_Change.copy()
Rx_Change_Denominator[Rx_Change_Denominator == 0] = 1
Ratio_Inc_Change = Rx_Increase / Rx_Change_Denominator

# Compute frequencies of drugs prescribed (units: number of days)
# Assumes drugs of interest are statins
Atorva = (Rx[:,:,days] * (drug=='Atorva')).sum(1)
Fluva = (Rx[:,:,days] * (drug=='Fluva')).sum(1)
Lova = (Rx[:,:,days] * (drug=='Lova')).sum(1)
Pitava = (Rx[:,:,days] * (drug=='Pitava')).sum(1)
Prava = (Rx[:,:,days] * (drug=='Prava')).sum(1)
Rosuva = (Rx[:,:,days] * (drug=='Rosuva')).sum(1)
Simva =(Rx[:,:,days] * (drug=='Simva')).sum(1)
drugs = np.vstack((Atorva, Fluva, Lova, Pitava, Prava, Rosuva, Simva)).T
Main_Drug = np.array(['Atorva', 'Fluva', 'Lova', 'Pitava', 'Prava', 'Rosuva',
                      'Simva'])[drugs.argmax(1)]
Initial_Drug = drug[:,0]
Final_Drug = drug[range(0, drug.shape[0]), (scaling != 0).sum(1) - 1]

# Save data to plaintext file
Head = np.array(['IDs', 'PDD/DDD', 'PDD/DDD Factor', 'Number of Visits',
                 'PDD/DDD Changes', 'PDD/DDD Increases', 'PDD/DDD Decreases',
                 'Total PDD/DDD Change', 'Increase/Change Ratio', 'Atorva',
                 'Fluva', 'Lova', 'Pitava', 'Prava', 'Rosuva', 'Simva',
                 'Primary Drug', 'Initial Drug', 'Final Drug'])
data = np.vstack((Head, np.vstack((IDs, PDD_per_DDD, PDD_per_DDD_factor,
                                   Visits, Rx_Change, Rx_Increase, Rx_Decrease,
                                   Total_Change, Ratio_Inc_Change, drugs.T,
                                   Main_Drug, Initial_Drug, Final_Drug)).T))
filename = FILE.split('.')[0] + '_processed.' + FILE.split('.')[1]
np.savetxt(filename, data, fmt='%s', delimiter=',')
