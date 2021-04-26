# RShinyApp_Covid19inUS
View the app [here](https://chiangtenghui.shinyapps.io/rshinyapp_covid19inus/).

The aim of the app is to provide an interactive dashboard to visualise and analyze the COVID-19 situation in the US with the use of descriptive and inferential statistics, as well as mathematical modelling.

## How to play with it
### Time-Series tab
- Select which model to perform prediction (ARIMA/SIR) on the left
- Click on states on the map or select from dropdown on the left to visualise the line plots of historical and predicted cases in selected state
- Change the date from the time slider at the bottom of the map to view how number of positive cases have changed over time for all US states
### MLR Model tab
- Check out the distribution of selected variable for selected state(s) in the EDA tab
- Find out the correlation between number of positive cases and selected variable
- Build your own model using stepwise regression and your selected variables
- Change the parameters and find out the predicted total number of positive cases based on our best regression model
### SIR Model tab
- Delve deeper into the SIR model and find out the parameters we use in this mathematical model

## About
This app was created as part of the requirements for the Applied Statistical Analysis with R module for Master of IT in Business program at Singapore Management University (SMU).

### Data Sources
Data is cut off at 8th February 2021. 
- [The Covid Tracking Project Dataset](https://covidtracking.com/data) for data on COVID-19 cases in each U.S. state
- [Kaiser Family Foundation COVID-19 Report](https://www.kff.org/report-section/state-covid-19-data-and-policy-actions-policy-actions/#socialdistancing) for data on each U.S. state's social distancing policies
- [U.S. Census Bureau](https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates.html) for population data in each U.S. state
- [U.S. Bureau of Economic Analysis](https://www.bea.gov/data/income-saving/personal-income-by-state) for economic data in each U.S. state
- [Gallup Polling](https://news.gallup.com/poll/226643/2017-party-affiliation-state.aspx) for political affliation of each U.S. state
- [U.S. National Centre for Environmental Information](https://www.ncdc.noaa.gov/cag/divisional/mapping) for temperature data of each U.S. state

### The Team
Chiang Teng Hui, Do Thanh Tu, Le Duc Bao, Lee Suling, Wang Qinyu, Xu Dan
