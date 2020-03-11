#  read_excel.py
#
#  This Python script will read an Excel file and write the in
#  contents as a CSV file to "dpst1f.dat".  The name of the Excel
#  file is retrieved from dpst5f.dat.
#
#  It requires that the "pandas" package be installed.
#
#  Step 1: Import needed packages
#
import pandas as pd
from pandas import ExcelWriter
from pandas import ExcelFile
#
#  Step 1: Open the "dpst5f.dat" file and extract the names
#          for the Excel file and the sheet to write to.
#
#print('In read_excel.py module')

try:
    fhand = open('dpst5f.dat','r')
except:
    print('File dpst5f.dat cannot be opened in write_excel.py')
    exit()

try:
    excel_name = fhand.readline()
    excel_name = excel_name.rstrip()
    #print('Name of Excel File: ',excel_name)
except:
    print('Unable to read the name of the Excel file in dpst5f.dat')
    exit()

try:
    excel_sheet = fhand.readline()
    excel_sheet = excel_sheet.rstrip()
    #print('Name of Excel sheet: ',excel_sheet)
except:
    excel_sheet = 'Sheet1'

fhand.close()

#
#  Step 2: Read the "dpst1f.dat" file with Pandas
#
df = pd.read_excel(excel_name,sheet_name=excel_sheet)

#
#  Step 3: Now use Pandas to write the Excel file
df.to_csv("dpst1f.dat")
#

