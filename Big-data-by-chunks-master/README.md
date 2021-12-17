# Big-data-by-chunks
Exploring big data by chunks

I'am going to use the data from 'ind_pop_data.csv' which can be find on:
https://www.kaggle.com/worldbank/world-development-indicators


This is going to be about processing a large dataset in chunks. 
I will show how to process the data with a single function so that you can reuse the code without having to rewrite 
the same things all over again.

I'am going to define the function plot_pop() which takes two arguments: the filename of the file to be processed, 
and the country code of the rows I want to process in the dataset.

Function  does the following:

Loading of the file chunk by chunk,
Creating the new column of urban population values, and
Plotting the urban population data.

Function makes it convenient to repeat the same process for whatever file and country code you want to process and visualize!

