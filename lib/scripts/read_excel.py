#  read_excel.py
#
#  This Python script will read an Excel file and write the in
#  contents as a CSV file to "dpst1f.dat".  The name of the Excel
#  file is retrieved from dpst5f.dat.
#
#  It requires that the "pandas" package be installed.
#
#
#  2021/07: Option for header added
#
#  Step 1: Import needed packages
#
import pandas as pd
from pandas import ExcelWriter
from pandas import ExcelFile
#
iflagr = 0
iflagc = 0
nskip = 0
nlines = 0
cols = 'None'
iheader = "'None'"
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

#
#   Read name of Excel file
#
try:
    excel_name = fhand.readline()
    excel_name = excel_name.rstrip()
#   print('Name of Excel File: ',excel_name)
except:
    print('Unable to read the name of the Excel file in dpst5f.dat')
    exit()

#
#   Read name of Excel sheet
#
try:
    excel_sheet = fhand.readline()
    excel_sheet = excel_sheet.rstrip()
#   print('Name of Excel sheet: ',excel_sheet)
except:
    excel_sheet = 'Sheet1'

#
#   Read number of lines to skip
#
try:
    iline = fhand.readline()
    nskip = int(iline)
    if nskip > 0:
       iflagr = 1

except:
    print('Unable to read the number of lines to skip in the Excel file in dpst5f.dat')
    print('iline: ',iline)
    print('nskip: ',nskip)
    exit()

#
#   Read number of lines to process
#
try:
    iline = fhand.readline()
    nread = int(iline)
    if nread > 0:
       iflagr= 2
       if nskip < 0:
          nskip = 0

except:
    print('Unable to read the number of lines to process in the Excel file in dpst5f.dat')
    print('iline: ',iline)
    print('nread: ',nread)
    exit()

#
#   Option for header line (0 for first line being header, -1
#   for no header
#
try:
    iline = fhand.readline()
    iheader = 0
    if int(iline) < 0:
       iheader = -1

except:
    print('Unable to read the header option for the Excel file in dpst5f.dat')
    print('iline: ',iline)
    print('iheader: ',iheader)
    exit()

#
#   Read column list
#
#   2020/05: Having an issue getting the usecols option to work, so
#            comment this out for now.
#
#try:
#    iline = fhand.readline()
#    if iline == "None":
#       iflagc = 0
#    else:
#       if iline[0:0] == "[":
#          iflagc = 1
#          cols = iline
#       else:
#          iflagc = 1
#          cols = iline
#
#except:
#    print('Unable to read the column list for the Excel file in dpst5f.dat')
#    exit()

iflagc = 0

#
#   Close dpst5f.dat file
#
fhand.close()

#
#  Step 2: Read the "dpst1f.dat" file with Pandas
#
#print ('iflagr, iflagc = ',iflagr,iflagc)
#print ('nskip,nread = ',nskip,nread)
#print ('cols = ',cols)

if iflagr == 0:
    if iflagc == 0:
       if iheader  == -1:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,header=None,usecols=None)
       else:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,usecols=None)
    elif iflagc == 1:
       if iheader == -1:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,header=None,usecols={cols})
       else:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,usecols={cols})
elif iflagr == 1:
    if iflagc == 0:
       if iheader == -1:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,header=None,skiprows=nskip,usecols=None)
       else:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,usecols=None)
    elif iflagc == 1:
       if iheader == -1:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,header=None,usecols={cols})
       else:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,usecols={cols})
elif iflagr == 2:
    if iflagc == 0:
       if iheader == -1:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,nrows=nread,header=None,usecols=None)
       else:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,nrows=nread,usecols=None)
    elif iflagc == 1:
       if iheader == -1:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,nrows=nread,header=None,usecols={cols})
       else:
          df = pd.read_excel(excel_name,sheet_name=excel_sheet,skiprows=nskip,nrows=nread,usecols={cols})

#
#  Step 3: Now use Pandas to write the Excel file
df.to_csv("dpst1f.dat")
#

