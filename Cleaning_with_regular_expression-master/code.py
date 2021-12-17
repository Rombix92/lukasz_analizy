import pandas as pd
import re
from numpy import NaN

#downloading datasets
url='https://assets.datacamp.com/production/course_2023/datasets/dob_job_application_filings_subset.csv'
df=pd.read_csv(url, sep=',')

#this is how two columns, the main actor of this code, looks like
df[['Initial Cost','Total Est. Fee']].head()

#Initial Cost Total Est. Fee
#0    $75000.00        $986.00
#1        $0.00       $1144.00
#2    $30000.00        $522.50
#3     $1500.00        $225.00
#4    $19500.00        $389.50

#firstly i want to check each of the cell,whether it fulfill the specification of how currency value looks like
#if value has dollar sign on the begining, then some series of digits, then period, and the two digits, it okey

pattern = re.compile('^\$\d*\.\d{2}$')

#otherwise I want to transform every other value into missing value
#finally I want to compute the difference between those to column and prescribe this value to new variable


#in this order new function in created

def diff_money(row, pattern):
     icost = row['Initial Cost']
     tef = row['Total Est. Fee']
""" checking whether the value fullfil prescribed pattern"""
     if bool(pattern.match(icost)) and bool(pattern.match(tef)):
         icost = icost.replace("$","")
         tef = tef.replace("$","")
           
         icost = float(icost)
         tef = float(tef)
         return icost - tef
     else:
         return(NaN)

diff = df.apply(diff_money, axis=1,pattern=pattern)
print(diff)
