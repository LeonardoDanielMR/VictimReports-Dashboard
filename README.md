# Security-Dashboard-in-Peru (Group 3)
## Data 
The idea of this project is to demonstrate the viability of the public policy **"Implementation of a crime prevention program in vulnerable communities in Perú"**.  
In order to this we used two datasets of the year 2017 related with police reporsts provided by **INEI**.  
- First Dataset: Contains information about the police reports
- Second Dataset: Contains information about the victims

## Creating Database
We use SQL Server to create the database, the diagram model of the database and the respective dimensions. In this case we use a star model. The dimensions created were:
- Place
- Type
- Victims
- Time
- reports

## Data Processing (ETL)
Both datasets had empty and incomplete variables, also the datatype was wrong asigned. So we transformed the variables that had an incorrect datatype to the correct one, delete the variables with more than 15% of missing values. And for the rest of variables with missing values we used imputation techniques. After that, all the transformed data were loaded to the database.

## Exploratory Analysis
We applied a exploratory analysis of the data using pie charts, bar plots and graphics of tendency. 

## Machine Learning 
We also applied machine learning like classification techniques (decision trees) and prediction (Logistic Regression Model).

## Power Bi Dashboard
Finally we analyce and represent important indicators and graphics to see which are the most dangerous and insecure zones of Perú.


