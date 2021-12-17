

# Import necessary modules
import glob
import pandas as pd

#imporitng dataset
url='https://assets.datacamp.com/production/course_2023/datasets/nyc_uber_2014.csv'
df=pd.read_csv(url, sep=',')
df=pd.DataFrame(df)
df.shape

#creating two dataset in working directory, which consist of the same columns but different observations
df[0:150].to_csv(r'uber1.csv')
df[150:].to_csv(r'uber2.csv')

#I will show how to create function allows for searching in working directory files which fulfill some name pattern,
#in order to concatenate them all together

# Write the pattern: pattern
pattern = '*.csv'

# Save all file matches: csv_files. Glob functions screen the working directory in search for names fulfilling given pattern.
csv_files = glob.glob(pattern)

# Create an empty list: frames
frames = []


#creating function which creates a list of data frames, which fulfill earlier set pattern
#  Iterate over csv_files
for csv in csv_files:

    #  Read csv into a DataFrame: df
    df = pd.read_csv(csv)
    
    # Append df to frames
    frames.append(df)

# Concatenate frames into a single DataFrame: uber
uber = pd.concat(frames)

# Print the shape of uber
print(uber.shape)

# Print the head of uber
print(uber.head())
