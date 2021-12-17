# -*- coding: utf-8 -*-
"""
Created on Sun Sep 15 14:10:05 2019

@author: Łukasz
"""
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
data_all = pd.read_csv("https://assets.datacamp.com/production/repositories/1765/datasets/ae888d00f9b36dd7d50a4afbc112761e2db766d2/turnover.csv")

department = pd.get_dummies(data_all.department)
department = department.drop("technical",axis=1)
data = data_all.drop("department",axis=1)

# Change the type of the "salary" column to categorical
data.salary = data.salary.astype('category')
#Provide the correct order of categories
data.salary = data.salary.cat.reorder_categories(['low', 'medium', 'high'])
#Encode observations as codes, not labels
data.salary = data.salary.cat.codes

corr_matrix= data.corr()

#Firstly I need to separate DV drom INDV
target = data.churn
features = data.drop("churn",axis=1)

from sklearn.model_selection import train_test_split
#Then using scikit learn package I can separate traning and test datasets
target_train, target_test, features_train, features_test = train_test_split(target,features,test_size=0.25,random_state=42)

from sklearn.tree import DecisionTreeClassifier
# Apply Decision Tree model to fit Features to the Target
model = DecisionTreeClassifier(random_state=42)
model_depth_5 = DecisionTreeClassifier(max_depth=5, random_state=42)
model_sample_100 = DecisionTreeClassifier(min_samples_leaf=100, random_state=42)


model.fit(features_train,target_train)
model_depth_5.fit(features_train,target_train)
model_sample_100.fit(features_train,target_train)

#model with balanced probabilities
model_depth_5_b = DecisionTreeClassifier(max_depth=5,class_weight="balanced",random_state=42)


# Import the graphical visualization export function
from sklearn.tree import export_graphviz

# Export the tree to a dot file
export_graphviz(model,"tree.dot")
#(paste this file into http://www.webgraphviz.com/)

#Calculating precision score, recall and specifity
from sklearn.metrics import precision_score, recall_score, roc_auc_score

#basing on the training model make prediction about test model
model.fit(features_train,target_train)
prediction = model.predict(features_test)
# Calculate precision score by comparing target_test with the prediction
precision_score(target_test, prediction)
recall_score(target_test, prediction)
roc_auc_score(target_test, prediction)

# Import the function for implementing cross validation, the same I can implement
# cross_val_predict
from sklearn.model_selection import cross_val_score
# Use that function to print the cross validation score for 10 folds
print(cross_val_score(model,features,target,cv=10))
 
# Generate values for maximum depth
depth = [i for i in range(5,21,1)]
 
# Generate values for minimum sample size
samples = [i for i in range(50,500,50)]
 
# Create the dictionary with parameters to be checked
parameters = dict(max_depth=depth, min_samples_leaf=samples)
#I can now use the sklearn GridSearchCV() function to find the best combination of all of the max_depth and min_samples_leaf values you generated in the previous exercise.
 
# import the GridSearchCV function
from sklearn.model_selection import GridSearchCV
 
# set up parameters: done
parameters = dict(max_depth=depth, min_samples_leaf=samples)
 
# initialize the param_search function using the GridSearchCV function, initial model and parameters above
param_search = GridSearchCV(model, parameters)
 
# fit the param_search to the training dataset
param_search.fit(features_train, target_train)
 
# print the best parameters found
print(param_search.best_params_)

max_depth=param_search.best_params_['max_depth']
min_sample=param_search.best_params_['min_samples_leaf']
model_best = DecisionTreeClassifier(max_depth=max_depth,min_samples_leaf=min_sample,random_state=42)
model_best.fit(features_train, target_train)

 
# Calculate feature importances
feature_importances = model_best.feature_importances_
 
# Create a list of features: done
feature_list = list(features)
 
# Save the results inside a DataFrame using feature_list as an index
relative_importances = pd.DataFrame(index=feature_list, data=feature_importances, columns=["importance"])
 
# Sort values to learn most important features
relative_importances.sort_values(by="importance", ascending=False)
 
 
 
# select only features with relative importance higher than 1%
selected_features = relative_importances[relative_importances.importance>0.01]

# create a list from those features: done
selected_list = selected_features.index

# transform both features_train and features_test components to include only selected features
features_train_selected = features_train[selected_list]
features_test_selected = features_test[selected_list]

#last model
model_best.fit(features_train_selected,target_train)
export_graphviz(model_best,"tree.dot")

model_best.feature_importances_


sum(data_all[(data_all.satisfaction<=0.115) & (data_all.evaluation<=2.5)].churn)
