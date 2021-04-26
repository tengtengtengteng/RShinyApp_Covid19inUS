library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyverse)
library(MASS)

source('app_ServerFunctions.R')

# read in data
combined_data <- read_csv('./data/combined.csv')
arima_positive_data <- read_csv('./data/combined_positive_ARIMA.csv')
arima_death_data <- read_csv('./data/combined_death_ARIMA.csv')
sir_infected_data <- read_csv('./data/combined_SIR_infected.csv')
sir_removed_data <- read_csv('./data/combined_SIR_removed.csv')
sir_param <- read_excel('./data/outputSIR_combined.xlsx', sheet='Sheet1')
state_code_name <- read_csv('./data/state_code_name_mapping.csv')

# data manipulation for arima
arima_positive_data$date <- dmy(arima_positive_data$date)
arima_positive_data <- arima_positive_data %>% filter(date <= '2021-03-31')
arima_death_data$date <- dmy(arima_death_data$date)
arima_death_data <- arima_death_data %>% filter(date <= '2021-03-31')

# data manipulation for sir
sir_infected_data <- sir_infected_data[,-c(1,2)]
sir_removed_data <- sir_removed_data[,-c(1,2)]
sir_infected_data$date <- dmy(sir_infected_data$date)
sir_removed_data$date <- dmy(sir_removed_data$date)

# data manipulation for map
map_df <- combined_data
map_states <- sort(c(unique(map_df$state), 'HI', 'OR'))

# data manipulation for MLR
positive_df <- combined_data %>%
  dplyr::select(c('positive', 'death', 'recovered', 'totalTestResultsIncrease', 'Income per capital', 
                  'Democratic advantage', 'POP_DENS', 'POP', 'Temp', 'Stay at Home Order', 
                  'Mandatory Quarantine for Travelers', 'Face Covering Requirement', 'date', 'State')) %>%
  filter(date < ymd('2020-12-14') & date > ymd('2020-06-24')) %>%  # first date of vaccine in USA and policy data
  drop_na() # drop row if any variable has na

# update col names to have no spaces and cap first letter
colnames(positive_df) <- c('Positive', 'Death', 'Recovered', 'TotalTestResultsIncrease', 'Income_per_Capita', 
                           'Democratic_Advantage', 'Population_Density', 'Population', 'Temp', 'Stay_at_Home_Order', 
                           'Mandatory_Quarantine_for_Travelers', 'Face_Covering_Requirement', 'Date', 'State')
colnames(arima_positive_data) <- c('Date', 'Positive', 'State', 'Is_Predict')
colnames(arima_death_data) <- c('Date', 'Death', 'State', 'Is_Predict')
colnames(sir_infected_data) <- c('Infected', 'Date', 'State', 'Is_Predict')
colnames(sir_removed_data) <- c('Removed', 'Date', 'State', 'Is_Predict')

# State choices
state_choices <- unique(positive_df[['State']])
sir_state_choices <- sort(unique(sir_infected_data[['State']]))
arima_state_choices <- unique(arima_positive_data[['State']])

# date choices
sir_date_choices <- unique(sir_infected_data[['Date']])
map_date_choices <- sort(unique(combined_data[['date']]))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Time-Series" ,tabName = "Main", icon = icon("chart-line")),
    menuItem("MLR Model", tabName = "MLR", icon = icon("project-diagram")),
    menuItem("SIR Model", tabName = "SIR", icon = icon("virus")),
    menuItem("About", tabName = "About", icon = icon("address-card"))
  )
)

body <- dashboardBody(
  includeCSS('app_styles.css'), # include CSS here
  tabItems(
    tabItem(tabName = 'Main',
            fluidPage(
              titlePanel('Time-Series Predictions'),
              #htmlOutput('main_text'),
              sidebarPanel(
                width = 2,
                radioButtons("main_model", "Select which model to perform prediction",
                             choices=c('ARIMA', 'SIR'),
                             selected = 'ARIMA'),
                textOutput('model_text'),
                selectInput('main_state', 'Select state:',
                            choices = c('All', arima_state_choices),
                            selected = 'All'),
              ),
              mainPanel(
                width = 10,
                box(
                  width = 6,
                  textOutput('positive_text'),
                  plotlyOutput('main_positive_plot', height = '400')
                ),
                box(
                  width = 6,
                  textOutput('death_text'),
                  plotlyOutput('main_death_plot', height = '400')
                ),
                box(
                  width = 12,
                  textOutput('map_text'),
                  plotlyOutput('main_map_plot', height = '500')
                ),
                box(width=12,
                    sliderTextInput('main_map_date', 'Change the date',
                                    choices = map_date_choices, selected = '2020-10-01'))
              )
            )
    ),
    tabItem(tabName = 'MLR',
            fluidPage(
              titlePanel('Multiple Linear Regression'),
              fluidRow(
                column(width = 12,
                       height = 100,
                       tabsetPanel(
                         tabPanel(
                           title = 'EDA',
                           textOutput('EDA_text'),
                           box(
                             width = 4,
                             selectInput('eda_x', 'Select a factor:',
                                         choices = c('Death', 'Recovered', 'TotalTestResultsIncrease', 'Income_per_Capita', 
                                                     'Democratic_Advantage', 'Population_Density', 'Population', 'Temp', 'Stay_at_Home_Order', 
                                                     'Mandatory_Quarantine_for_Travelers', 'Face_Covering_Requirement'),
                                         selected = 'totalTestResultsIncrease'),
                             selectizeInput('eda_state', 'Select state(s):',
                                                choices = state_choices, multiple=TRUE,
                                                selected = state_choices[1:10]),
                             h4('Descriptive Statistics:'),
                             textOutput('desc_stat')
                           ),
                           box(
                             width = 8,
                             textOutput('EDA_plot_text'),
                             plotlyOutput('EDA_plot', height='600')
                           )
                         ),
                         tabPanel(
                           title = 'Correlation Coefficient', # Continuous Factors Affecting Number of Positive Cases
                           textOutput('cor_text'),
                           box(
                             width = 4,
                             selectInput('cor', 'Select a factor:',
                                         choices = c('Death', 'Recovered', 'TotalTestResultsIncrease', 'Income_per_Capita', 
                                                     'Democratic_Advantage', 'Population_Density', 'Population', 'Temp'),
                                         selected = 'TotalTestResultsIncrease'),
                             h4('Correlation coefficient r:'),
                             verbatimTextOutput('cor_result')
                          ),
                           box(
                             width = 8,
                             textOutput('cor_plot_text'),
                             plotlyOutput('cor_scatter', height='600')
                           )
                        ),
                        tabPanel(
                          title = 'Model Building',
                          textOutput('model_build_text'),
                          box(
                            width = 12,
                            selectInput("mlr_X", "Independent Variables:",
                                        choices = c('TotalTestResultsIncrease', 'Income_per_Capita', 'Democratic_Advantage',
                                                    'Population_Density', 'Population', 'Temp', 'Stay_at_Home_Order', 
                                                    'Mandatory_Quarantine_for_Travelers', 'Face_Covering_Requirement'),
                                        multiple = TRUE,
                                        selected = c('TotalTestResultsIncrease', 'Income_per_Capita', 'Democratic_Advantage',
                                                     'Population_Density', 'Population', 'Temp', 'Stay_at_Home_Order', 
                                                     'Mandatory_Quarantine_for_Travelers', 'Face_Covering_Requirement')
                            ),
                            selectInput("strategy", "Strategies of Stepwise:",
                                        choices =  list("Forward Selection", "Backward Selection", "Stepwise Selection")
                            )),
                          box(
                            width = 12,
                            h4('Regression Summary:'),
                            verbatimTextOutput("mlr_output"),
                          )
                        ),
                        tabPanel(
                          title = 'Prediction',
                          textOutput('prediction_text'),
                          sidebarPanel(
                            sliderInput('totaltest','Enter how many test done today', min = 0, max = 100000, value = 50000),
                            sliderInput('pci','Enter Income per Capita of the state', min = 0, max = 1000, value = 500),
                            sliderInput('political','Enter PoliticalPpreference (more positive = more democratic)', min = -50, max = 50, value = 0),
                            sliderInput('popdens','Enter Population Density', min = 500, max = 5000, value = 2750),
                            sliderInput('pop','Enter Population Size', min = 500000, max = 30000000, value = 15000000, step=500000),
                            sliderInput('temp','Enter Temperature in fahrenheit', min = 0, max = 120, value = 60),
                            sliderTextInput('stayhome','Enter Stay at Home Order category', 
                                            choices=c('No Restrictions', 'During Curfew Hours', 'High Risk Groups', 'Statewide'), 
                                            selected=c('No Restrictions'), grid=TRUE, force_edges=TRUE),
                            sliderTextInput('quarantine','Enter Travelers Quarantine category', 
                                            choices=c('No Restrictions', 'From Certain States', 'International Travelers', 'All Travelers'), 
                                            selected=c('No Restrictions'), grid=TRUE, force_edges=TRUE),
                            sliderTextInput('facecover','Enter Face-Covering Restriction', 
                                            choices=c('No Restrictions', 'Required For Certain Employees', 'Required For General Public'), 
                                            selected=c('No Restrictions'), grid=TRUE, force_edges=TRUE)
                          ),
                          mainPanel(
                            h4('The predicted total number of positive cases is:'),
                            verbatimTextOutput('mlr_predict')
                          )
                        )
                      )
              )
            )
          )
    ),
    tabItem(tabName = 'SIR',
            fluidPage(
              titlePanel('SIR Model'),
              textOutput('sir_text'),
              sidebarPanel(
                selectInput("sir_state", "Select which state to show",
                            choices = sir_state_choices,
                            multiple = FALSE,
                            selected = "Iowa"),
                sliderInput('sir_date', 'Select date', min = min(sir_date_choices), max = max(sir_date_choices), value = mean(sir_date_choices)),
                h4('Predicted Infected Percentage'),
                verbatimTextOutput('sir_infected'),
                h4('Predicted Removed Percentage'),
                verbatimTextOutput('sir_removed')
              ),
              mainPanel(
                box(
                  width = 12,
                  textOutput('sir_plot_text'),
                  plotlyOutput('sir_plot', height = '600')
                ),
                box(width = 6, color='black',
                    verbatimTextOutput('sir_beta')
                ),
                box(width = 6, color='black',
                    verbatimTextOutput('sir_gamma')
                )
              )
            )
    ),
    tabItem(tabName = 'About',
            fluidPage(
              titlePanel('About'),
              box(
                width = 12,
                htmlOutput('about_team_text')
              ),
              h2('Assumptions and Limitations'),
              box(
                width = 12,
                htmlOutput('about_limit_text')
              ),
            )
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(skin='black',
  dashboardHeader(title = "COVID-19 in U.S."),
  sidebar,
  body
)


# create the server functions for the dashboard  
server <- function(input, output, session) { 

  output$main_text <- renderUI({
    HTML('The Coronavirus Disease 2019 (COVID-19) emerged in late 2019 and has spread rapidly ever since. 
    About 25% of total reported cases came from the United States (U.S.).
    </br>
    This interactive dashboard serves to provide a geospatial view of the COVID-19 statistics across all U.S. states
    and perform inferential statistical and mathematical modelling of the pandemic.')
  })
  
  output$model_text <- renderText({
    if(input$main_model=='SIR'){
      'Susceptible-Infected-Removed (SIR) is a mathematical model based on the compartmental
      modelling approach. People may progress between the 3 compartments. This technique is 
      commonly used to predict how a disease spreads.'
    } else {
      'Auto Regressive Integrated Moving Average (ARIMA) is a popular and widely
      used statistical method for time series forecasting. It makes use of its own
      past values and forecast errors, as well as differencing to make the time series
      stationary.'
    }
  })
  
  output$positive_text <- renderText({
    if(input$main_model=='SIR'){
      text = 'Percentage of Infected Cases over Time in'
    } else {
      text = 'Number of Positive Cases over Time in'
    }
    if(input$main_state=='All'){
      state = 'All States'
    } else {
      state = deframe(state_code_name[state_code_name$Code==input$main_state,'State'])
    }
    paste(text, state)
  })
  
  output$death_text <- renderText({
    if(input$main_model=='SIR'){
      text = 'Percentage of Removed Cases over Time in'
    } else {
      text = 'Number of Deaths over Time in'
    }
    if(input$main_state=='All'){
      state = 'All States'
    } else {
      state = deframe(state_code_name[state_code_name$Code==input$main_state,'State'])
    }
    paste(text, state)
  })
  
  output$map_text <- renderText({
    'Number of Positive COVID-19 Cases in US States'
  })
  
  output$main_positive_plot <- renderPlotly({
    if(input$main_model=='SIR'){
      plot_main(sir_infected_data, 
                state_name = ifelse(input$main_state=='All', 'All', 
                                    deframe(state_code_name[state_code_name$Code==input$main_state,'State'])), 
                measure = 'Infected')
    } else {
      plot_main(#selectedPositive(), 
                arima_positive_data,
                state_name = input$main_state, 
                measure = 'Positive', 
                group = 'Is_Predict')
    }
  })
  
  output$main_death_plot <- renderPlotly({
    if(input$main_model=='SIR'){
      plot_main(sir_removed_data, 
                state_name = ifelse(input$main_state=='All', 'All', 
                                    deframe(state_code_name[state_code_name$Code==input$main_state,'State'])), 
                measure = 'Removed')
    } else {
      plot_main(#selectedDeath(), 
                arima_death_data,
                state_name = input$main_state, 
                measure = 'Death', 
                group = 'Is_Predict')
    }
  })
  
  output$main_map_plot <- renderPlotly({
    map_df <- map_df %>% 
      filter(date == input$main_map_date) %>% 
      select(state, positive) %>% 
      deframe()
    map_df <- c(map_df, 'HI' = 0)
    map_df <- c(map_df, 'OR' = 0)
    map_df <- map_df[sort(names(map_df))]
    plotly_map(map_df)
  })
  
  # monitor clicking on plots and check if changed
  observe({
    event.data <- event_data('plotly_click')
    #print(event.data)
    #print(event.data[['pointNumber']] + 1)
    updateSelectInput(session, 
                      'main_state', 
                      selected = map_states[event.data[['pointNumber']] + 1])
  })
  
  output$EDA_text <- renderText({
    'Let us check out the distribution of the data!'
  })
  
  output$desc_stat <- renderText({
    print_summary(positive_df, x=input$eda_x)
  })
  
  output$EDA_plot_text <- renderText({
    paste('Distribution of', input$eda_x, 'over Time')
  })
  
  output$EDA_plot <- renderPlotly({
    plot_descriptive(positive_df[positive_df$State%in%input$eda_state,], input$eda_x)
  })
  
  
  output$cor_result <- renderText({
    print_correlation(positive_df,
                      y = 'Positive',
                      x = input$cor)
  })
  
  output$cor_text <- renderText({
    'Correlation coefficient measures the strength of the relationship between two variables.'
  })
  
  output$cor_plot_text <- renderText({
    paste('Correlation between Positive and', input$cor)
  })
  
  output$cor_scatter <- renderPlotly({
    plotly_scatterplot(df=positive_df, x=input$cor, y='Positive')
  })
  
  output$model_build_text <- renderText({
    'Try building your own model!'
  })
  
  output$mlr_output <- renderPrint({
    print_stepwise_MLR(positive_df,
                       y = "Positive",
                       X = input$mlr_X,
                       strategy = input$strategy)
  })
  
  output$prediction_text <- renderText({
    'Try out some numbers and see the predicted number of cases!'
  })
  
  output$mlr_predict <- renderText({
    predict_MLR(inputs = c(input$totaltest,
                           input$pci,
                           input$political,
                           input$popdens,
                           input$pop,
                           input$temp,
                           input$stayhome,
                           input$quarantine,
                           input$facecover))
  })
  
  output$sir_text <- renderText({
    'Check out the predictions from our SIR model!'
  })
  
  output$sir_infected <- renderText({
    sir_infected_data %>% filter(State==input$sir_state & Date==input$sir_date) %>% pull(Infected)
  })
  
  output$sir_removed <- renderText({
    sir_removed_data %>% filter(State==input$sir_state & Date==input$sir_date) %>% pull(Removed)
  })
  
  output$sir_plot_text <- renderText({
    paste('Percentage of Infected and Removed over Time in', input$sir_state)
  })
  
  output$sir_plot <- renderPlotly({
    ggplotly(
      sir_infected_data %>% 
        left_join(sir_removed_data) %>%
        filter(State==input$sir_state) %>% 
        select(c('Date', 'Infected', 'Removed')) %>%
        gather(key, value, Infected, Removed) %>%
        rename(Measure = key, Percentage = value) %>%
        ggplot(aes(x=Date, y=Percentage, colour=Measure)) + 
        geom_line() + theme_bw() + theme(legend.title = element_blank())
    )
  })
  
  output$sir_beta <- renderText({
    beta <- deframe(sir_param[sir_param$State==input$sir_state, 'beta'])
    paste('Infection Rate:', beta)
  })
  
  output$sir_gamma <- renderText({
    gamma <- deframe(sir_param[sir_param$State==input$sir_state, 'gamma'])
    paste('Removal Rate:', gamma)
  })
  
  output$about_team_text <- renderUI({
    HTML('The Coronavirus Disease 2019 (COVID-19) emerged in late 2019 and has spread rapidly ever since. 
          About 25% of total reported cases came from the United States (U.S.). 
          This interactive dashboard serves to provide a geospatial view of the COVID-19 statistics across all U.S. states
          and perform inferential statistical and mathematical modelling of the pandemic.
          </br>
          The Team: Chiang Teng Hui, Do Thanh Tu, Le Duc Bao, Lee Suling, Wang Qinyu, Xu Dan
         </br>
         This app was created as part of the requirements for the Applied Statistical Analysis 
         with R module for Master of IT in Business program at Singapore Management University (SMU).')
  })
  
  output$about_limit_text <- renderUI({
    HTML('In this project, we have assumed the following:
          <ul>
            <li> Multiple Linear Regression 
              <ul>
                <li> Each predictor used is independent from each other. </li>
                <li> The relationship between each predictor and the number of positive cases is linear. </li>
                <li> Vaccinations are not available yet. (Data has excluded observations after 14 December 2020 when U.S. vaccination program started) </li>
              </ul>
            </li>
            <li> ARIMA
              <ul>
                <li> The time series is stationary. </li>
                <li> The time series is not seasonal. </li>
              </ul>
            </li>
            <li> SIR
              <ul>
                <li> All individuals in the population are assumed to have an equal probability of coming in contact with one another. </li>
                <li> The population of each state is closed and hence it has no migration, births, or deaths from causes other than COVID-19. </li>
                <li> There is no incubation period for individuals who get infected with COVID-19. </li>
                <li> Individuals who have recovered from the virus are assumed to be immune. </li>
              </ul>
            </li>
          </ul>
          <br/>
          In addition, here are some limitations of our project and recommendations on future improvement work.
          <ul>
            <li> The data is drawn from multiple sources and contain data completeness issues. Missing observations were either dropped or imputed with 0. </li>
            <li> As the data is in the form of panel data, panel regression is a more suitable method than multiple linear regression. </li>
            <li> The time-series forecast based on ARIMA only depends on its past values to generate predictions. 
            Government measures such as state policies to encourage social distancing and rolling out of vaccination programs will disrupt the trend and cause our forecast to be inaccurate. </li>
            <li> The SIR model has the tendency to underestimate Infected at the start of the epidemic and overestimate when measures are implemented. 
            Other modified SIR models like SEIR (E represents Exposed) could be more accurate.
            </li>
          </ul>')
  })
  
}  

shinyApp(ui, server)

