#ShinyDashboard
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)

#wordcloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(twitteR)
library(rtweet)
library(wordcloud2)

#ggplot+Regression
library(highcharter)
library(ggplot2)
library(plotly)
library(dplyr) #filter
library(countrycode)
library(plotrix)
library(stringr)
library(tidyverse)
library(gridExtra)
library(data.table)
library(caret)
library(ranger)
library(scales)
library(tidyr)

#forecast
library(forecast)

#maps
library(rworldmap)
library(maps)
options(scipen = 999)
library(ggmap)
library(rgdal)
library(scales)
library(maptools)
library(gridExtra)
library(rgeos)

# Add dataset
ind<-read.csv("C:/Users/shraddha/Documents/Msc_Project/SuicideAnalysis/india.csv",header = T,sep = ',')
data<-read.csv("C:/Users/shraddha/Documents/Msc_Project/SuicideAnalysis/master1.csv",header = T,sep = ',')

#add continents to dataset
data$continent <- countrycode(sourcevar = data[, "all_country"],
                            origin = "country.name",
                          destination = "continent")

ui<-dashboardPage(
    dashboardHeader(title = "Suicide Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction",tabName="intro",icon = icon("question")),
            menuItem("Text Analytics", tabName = "basic",icon = icon("fas fa-cloud")),
            menuItem("India",tabName="india",icon=icon("fas fa-info-circle"),
                     menuSubItem("Gender-wise Suicides", tabName = "sex"),
                     menuSubItem("Yearly Trend of Suicides", tabName = "yeartrend"),
                     menuSubItem("Age Group-wise Suicides", tabName = "agewise"),
                     menuSubItem("Education level And Suicides", tabName = "education"),
                     menuSubItem("Major Causes Of Suicides", tabName = "cause"),
                     menuSubItem("Social-Professional Status", tabName = "social"),
                     menuSubItem("State-wise Suicides", tabName = "state"),
                     menuSubItem("Top-10 Bottom-10 States", tabName = "topbottom"),
                     menuSubItem("Means-Adopted", tabName = "means")),
            menuItem("Global Analysis", tabName = "home", icon = icon("fas fa-globe"),
                     menuSubItem("Country-wise Suicides", tabName = "subitem1"),
                     menuSubItem("Sex-wise Suicides", tabName = "subitem2"),
                     menuSubItem("Age-Group and Suicides", tabName = "subitem3"),
                     menuSubItem("Generation-wise Suicides", tabName = "subitem4"),
                     menuSubItem("Highest-Lowest", tabName = "subitem5"),
                     menuSubItem("Rates by Continent", tabName = "yt"),
                     menuSubItem("Indicators",tabName = "gdp_hdi"),
                     menuSubItem("Animation-GDP", tabName = "ani")),
            menuItem("Time-Series Forecasting", tabName = "time", icon = icon("line-chart"),
                     menuSubItem("India",tabName = "indiaf"),
                     menuSubItem("Global",tabName = "globalf")),
            menuItem("Map Representation", tabName = "maps", icon = icon("map-marker"),
                     menuSubItem("Global", tabName = "map1"),
                     menuSubItem("India", tabName = "map2")),
            menuItem("Linear Regression",tabName = 'linearR',icon = icon("expand")),
            menuItem("Twitter-Analysis",tabName = "twitter",icon = icon("fab fa-twitter"))
)),
dashboardBody(
    shinyDashboardThemes(
        theme = "blue_gradient"
    ),
    tags$head(tags$style(HTML(''))),
    tabItems(
        tabItem(tabName = "basic",
                
                h2("Text Analytics : Automating Wordcloud"),
                fluidRow(box(title="Choose Attributes", collapsible = TRUE,status = "primary",solidHeader = T,
                             column(8,fileInput('wc', 'Choose File',multiple = F, accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.pdf',
                    '.tsv'
                )),
                sliderInput("wordfreq","Select the min frequency of Words",1,10,1),
                sliderInput("maxword","Select the max number of Words",1,500,100),
                checkboxInput("random","Random Order?"),
                radioButtons("color","Select the wordcloud color theme",c("Accent","Dark"),selected = "Accent"),
                actionButton("update","Create Word Cloud"))),
                column(6,plotOutput("wcplot")))
                ),
        tabItem(tabName="intro",
                 intro_row1 <- fluidRow(
                     box(
                         title = "WHY DO WE CARE ABOUT SUICIDES",
                         textOutput("intro1"),
                         width = 12
                     )),
                 
                 intro_row2 <- fluidRow(
                     box(
                         title = "References & More Information"
                         ,status = "primary"
                         ,solidHeader = TRUE 
                         ,collapsible = TRUE
                         ,textOutput("intro2"),
                         width = 12
                     )
                 )
                 
                 
                 ),
        #India
        tabItem(tabName = "sex",
                h2("Who Makes More Suicide Attempts Male or Female?"),
                fluidRow(box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,
                    selectInput("Select State",inputId = "select_state",label = "",choices = unique(ind$State)))),
                fluidRow(plotlyOutput('gender',width = 650))),
        tabItem(tabName = "yeartrend",
                h2("Suicide Trend Over The Years "),
                br(),
                plotlyOutput('year')),
        tabItem(tabName = "agewise",
                h2("Which Age Group Makes More Suicide Attempts?"),
                br(),
                fluidRow(column(8,plotOutput('age', width = 500,height = 400)),
                         column(4,valueBoxOutput("g1",width = 6),valueBoxOutput("g2",width = 6),
                            valueBoxOutput("g3",width = 6),valueBoxOutput("g4",width = 6),valueBoxOutput("g5",width = 6)))),
        tabItem(tabName= "education",
                h2("Education Level and Suicides"),
                fluidRow(plotlyOutput('education',width = 900,height = 500)),
                h2("Gender-Education"),
                fluidRow(column(6,plotlyOutput('education1',width = 700,height = 500)))), 
        tabItem(tabName= "cause",
                h2("Analysing The Major Causes Of The Suicides."),
                plotlyOutput('causes'),
                br(),
                plotlyOutput('cause1')),
        tabItem(tabName= "social",
                h2("Social Status"),
                plotlyOutput('social'),
                h2("Professional-Profile"),
                plotlyOutput('prof')),
        tabItem(tabName= "state",
                h2("Which State In India Has Got Highest Number Of Suicides."),
                highchartOutput('statewise',height =500)),
        tabItem(tabName= "topbottom",
                h2("Finding the top 10 and bottom 10 States in India."),
                plotlyOutput('topbot1'),
                plotlyOutput('topbot2')),
        tabItem(tabName = "means",
                h2("Means-adopted for Suicide"),                                                                              
                plotlyOutput('mean',width = 900,height = 500)),
        
        #Global
        tabItem(tabName= "subitem1",
                headerPanel("Countrywise Suicides"),
                global_row2 <- fluidRow(
                  box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,
                    selectInput(inputId = "select_country",
                                label = "Select country",
                                choices = unique(data$all_country)))),
                plotlyOutput('global_plot'),
                plotlyOutput('plots')),
        tabItem(tabName= "subitem2",
                h2("Gender and Suicides"),
                global_row2 <- fluidRow(column(6,
                  box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,width = 400,
                    selectInput(inputId = "select_year",
                                label = "Select year",
                                choices = unique(data$year)),
                    plotlyOutput('suicide_rate'))),
                  column(6,box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,width=400,
                    selectInput(inputId = "select_age",
                                label = "Select Age group",
                                choices = unique(data$age)
                    ),
                    plotlyOutput('suicide_by_age'))))),
        tabItem(tabName= "subitem3",
                h2("Age-Group Suicides"),
                plotOutput('menwomen')
        ),  
        tabItem(tabName = "yt",
                h2("Continent Analysis"),
                plotlyOutput('con'),
                h2("Suicide By continent and Sex"),
                plotlyOutput('con1')
        ),
        tabItem(tabName= "subitem4",
                h2("Suicide Rates-Generation"),
                plotlyOutput('generation',height = 600,width = 800)
        ),   
        tabItem(tabName= "subitem5",
                headerPanel("Countries with the *highest* and *lowest* suicides"),
                fluidRow(box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,
                selectInput(inputId="year2",label ="year",choices=1985:2015))),
                br(),
                fluidRow(column(6,plotlyOutput(outputId ="top5more" )),
                         column(6,plotlyOutput(outputId = "top5less")))),
    tabItem(tabName= "globalf",
            h2("Global Suicides Forecasting"),
            plotOutput('times')
           # h2("Accuracy"),
           # verbatimTextOutput('acc1'))
    ),
    tabItem(tabName = "indiaf",
            h2("India's Suicide Forecasting"),
            plotOutput('indf')
          #  h2("Accuracy"),
           # verbatimTextOutput('acc2')
    ),
    
    #GDP
    tabItem(
        tabName = "gdp_hdi",
        gdp_row1 <- fluidRow( 
            box(
                title = "GDP vs. Suicides per capita"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("gdp_plot")
            ),
            box(
                title = "HDI vs. Suicides per capita"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("hdi_plot")
            )),
        
        gdp_row2 <- fluidRow( 
            box(
                title = "Richer ??? More suicides"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,textOutput("gdp_text"),
                tags$head(tags$style("#gdp_text{font-size: 10px}"))
            ),
            box(
                title = "More developed, more suicides"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,textOutput("hdi_text"),
                tags$head(tags$style("#hdi_text{font-size: 10px}"))
            ))
    ),
    #animation
    tabItem(tabName = "ani",
            plotlyOutput('animation')),
    #maps
    tabItem(tabName = "map1",
            h2("Visualizing Global Suicides on Map."),
            plotOutput('mymap',height = 500)),
    tabItem(tabName = "map2",
            h2("Visualizing Suicides in India on Map."),
            plotlyOutput('indiamap',height = 600)),
    
    tabItem(tabName = "m",
            h2("Test if the MALE suicide rate changes as the female population changes."),
            plotlyOutput('lm1')),
    tabItem(tabName = "linearR",
            h2("Linear Regression."),
            fluidRow(box(title="Description", collapsible = TRUE,status = "primary",solidHeader = T,
                textOutput('re'),width = 12)),
            fluidRow(plotlyOutput('linearR1'))),
    tabItem(tabName = "twitter",
                h4("Twitter Hashtag Analysis "),
                # Enter a hashtag that interests you.
                fluidRow(box(title="Enter Hashtag", collapsible = TRUE,status = "primary",solidHeader = T,
                             column(8,textInput(inputId = "word",
                                   label = "Hashtag",
                                   value='covid'),
                
                #Select sample size
                sliderInput(inputId='n',
                                     label='Sample Size',
                                     value=200,
                                     min=100,
                                     max=500))),
            
                column(6,(wordcloud2Output('wc2')))))
))
)
