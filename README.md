
# Indian Startup Funding Analysis

This repository is part of the Programming for Data Science Project.
It focuses on analyzing the Indian startup ecosystem, with an emphasis on Venture Capital Funding trends.

Data was collected from PDF sources, news articles, and Reddit.
We conducted Exploratory Data Analysis (EDA) to identify funding patterns and built a Random Forest Regression Model to predict the amount of funding based on:
Industry,Funding Round,Last Date of Funding

The Shiny web app includes four sections:

1)Explore Data – Displays trends in the Indian funding ecosystem.

2)Predict Funding – Uses a Random Forest model to predict funding amounts.

3)Social Media and News – Shows relevant articles and Reddit discussions.

4)View Data Table – Presents the dataset used for analysis.

Libraries Used

Data Scraping: chromote, rvest, dplyr, httr

EDA: plotly, ggplot2, tidyverse, lubridate, scales

Model & App: scales, caret, randomForest, DT, plotly, shiny, shinydashboard
