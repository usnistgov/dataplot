#  write_excel.py
#
#  This Python script will read the CSV file in
#  "dpst1f.dat" and write it to the specified
#  Excel file.  This Excel file name is saved in
#  dpst5f.dat.
#
#  It requires that the "csv", "xlswriter" and "pandas" packages
#  be installed.
#
#  Step 1: Import needed packages
#
import pandas as pd
#
#  Step 1: Open the "dpst5f.dat" file and extract the names
#          for the Excel file and the sheet to write to.
#
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
df = pd.read_csv("dpst1f.dat")

#
#  Step 3: Now use Pandas to write the Excel file
#
df.to_excel(excel_name,sheet_name=excel_sheet,engine='xlsxwriter')

