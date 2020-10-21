# R-Shiny-Application
My R Shiny application and code for an assignment in which I was required to develop a R Shiny application using a housing data set.
https://ernesto-nava.shinyapps.io/HousePriceApp/
# Objective 
The objective of this assignment was to create and deploy a R Shiny application that visualizes a housing data set. 
# Application and R Code
Using the tabsetPanel() function, a Shiny application with four tabs, with four different visualizations and charts, was created. In order to successfully visualize the housing data set, I performed data preprocessing by replacing missing values with NA, log transforming the price variable and calculating the median value of the price variable. 
Tab 1: a histogram of the distribution of house (log) prices based on zip code(s). 
Tab 2: a table that displays the median home price (in descending order) by zip code for the Single Family Residential and Townhouse property types. 
Tab 3: a box plot that displays the median house price based on the number of beds selected. 
Tab 4: a map that displays all of the Dallas area houses, with hover-over text for each house mark, contained in the data set. 
