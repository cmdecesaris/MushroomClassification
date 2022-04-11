# Mushroom Classification

## Project Introduction 

Mushrooms are diverse, complex structures with a multitude of overlapping and contrasting 
characteristics between species. To better understand how mushroom characteristics affect edibility, a 
previously unanalyzed dataset containing 173 detailed mushroom profiles was processed and fit with a 
logistic regression model using edibility status as a binary outcome (Wagner et al., 2021). Model 
inference revealed mushrooms with white stems, winter growing season, and brown caps have a lower
probability of being inedible, while mushrooms with bell shaped caps, and green caps have a higher 
chance of being inedible. The overall model fit was sufficient, but future studies would benefit from 
datasets with increased sample sizes across all predictors.

## Data Description

This dataset includes 61069 hypothetical mushrooms with caps based on 173 species (353 mushrooms
per species). Each mushroom is identified as definitely edible, definitely poisonous, or of
unknown edibility and not recommended (the latter class was combined with the poisonous class).

Original Owner and Doner: D. Wagner

For more about the data generation see: https://mushroom.mathematik.uni-marburg.de/files/

## Running the Project

1. Preprocessing.R is sourced to run before the Graphs_Tables.R and Logistic_Regression.R files
2. The Graphs_Tables.R and Logistic_Regression.R files can be run independently  

## Files in this Repository 
1. primary_data.csv: data donated by D. Wangner used in this project
2. Preprocessing.R: File which cleans and wrangles the data into a suitable form for visualizaiton and modeling approaches
3. Graphs_Tables.R: File which generates the *majority* of graphs and tables found in the Report.pdf and Summary_Poster.pdf
4. Logistic_Modeling.R: File contains the modeling approach, diagnostics, and evaluation of models
5. Report.pdf: A detailed report of the project and methods used in code files. Reference this file for model and data interpretations
6. Summary_Poster.pdf: Summary of the report and key parts of the project


## References
