import pandas as pd

url = 'https://assets.datacamp.com/production/course_2023/datasets/ebola.csv'

ebola = pd.read_csv(url, sep=',')

print(ebola.head())

# Melt ebola: ebola_melt, other column can be considered on two columns, but to split data on those to columns firstly we need to melt all data
ebola_melt = pd.melt(ebola, id_vars=['Date', 'Day'], var_name='type_country', value_name='counts')

# Create a column called 'str_split' by splitting the 'type_country' column of ebola_melt on '_'. Note that you will first have to access the str attribute of type_country before you can use .split().
ebola_melt['str_split'] = ebola_melt.type_country.str.split('_')

# Create a column called 'type' by using the .get() method to retrieve index 0 of the 'str_split' column of ebola_melt.
ebola_melt['type'] = ebola_melt.str_split.str.get(0)

# Create a column called 'country' by using the .get() method to retrieve index 1 of the 'str_split' column of ebola_melt.
ebola_melt['country'] = ebola_melt.str_split.str.get(1)

# Print the head of ebola_melt
print(ebola_melt.head())
