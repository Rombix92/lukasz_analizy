#Aim of this code is to present how life expectancy was changing within last two centuries

import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt

#downloading gapminder dataset
url='https://assets.datacamp.com/production/course_2023/datasets/gapminder.csv'
df = pd.read_csv(url)

#gapminder dataset is presented in quiet uncomfortable way. The variable year is splited within several column, additionally
#additionally each counry posses 3 rows, duplicating information about each country

#firstly I melt all years column to one
df=df.melt(id_vars='Life expectancy')
df.columns=['Country','Year','Life_expectancy']

#then remove missing data cels
df=df.dropna()

#remove unimportant information about each country index
df=df[df['variable']!='Unnamed: 0']

############belows I present code responsibe for creating two part plot

# Add first subplot
plt.subplot(2, 1, 1) 


# Create a histogram of life_expectancy
df['Life_expectancy'].plot(kind='hist')

# Group gapminder: df
df = df.groupby('Year')['Life_expectancy'].mean()

# Add second subplot
plt.subplot(2, 1, 2)

# Create a line plot of life expectancy per year
df.plot()

# Add title and specify axis labels
plt.title('Life expectancy over the years')
plt.ylabel('Life expectancy')
plt.xlabel('Year')

# Display the plots
plt.tight_layout()
plt.show()
plt.savefig('gapminder.png')



