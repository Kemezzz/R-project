library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(MASS)
library(DT)
library(ggplot2)
library(jtools)
library(gridExtra) #use for grid.arrange
library(readxl)
library(readr)
library(fmsb) #for radar plot
library(plotly)

select <- dplyr::select

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Data Files ####
medaltally <- read_csv('Olympics_Medal_Tally_1896_2020.csv', col_types = cols(
  Year = col_integer(),
  Host_country = col_character(),
  Host_city = col_character(),
  Country_Name = col_character(),
  Country_Code = col_character(),
  Gold = col_integer(),
  Silver = col_integer(),
  Bronze = col_integer()
))

athleteevent <- read_csv('Olympics_Athlete_Events_1896_2016.csv', col_types = cols(
  ID = col_character(),
  Name = col_character(),
  Sex = col_factor(levels = c("M","F")),
  Age =  col_integer(),
  Height = col_double(),
  Weight = col_double(),
  Team = col_character(),
  NOC = col_character(),
  Games = col_character(),
  Year = col_integer(),
  Season = col_factor(levels = c("Summer","Winter")),
  City = col_character(),
  Sport = col_character(),
  Event = col_character(),
  Medal = col_factor(levels = c("Gold","Silver","Bronze"))
))

NOCs <- read_csv("noc_regions.csv", col_types = cols(
  NOC = col_character(),
  region = col_character()
))


developeding <- read_csv("Dev_Undev_17c.csv")
country_attributes <- read.csv("Country_Attributes.csv") #this is the exact same file written from data cleaning code

medaltally <- as.data.frame(medaltally)
athleteevent <- as.data.frame(athleteevent)
country_attributes <- as.data.frame(country_attributes)
developeding <- as.data.frame(developeding)

fit_total <- lm(Total_Medal~Height+Weight+Age+GDP+Pop_Count
                +Gold_Reward+Silver_Reward+Bronze_Reward+Host_binary, data = country_attributes)
regression_total <- stepAIC(fit_total, direction ="both")

country_attributes$gold_prop <- country_attributes$Gold/country_attributes$Total_Medal
country_attributes$silver_prop <- country_attributes$Silver/country_attributes$Total_Medal
country_attributes_rounded <- country_attributes %>% mutate_if(is.numeric, round)


####UI####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Preamble", tabName = 'Preamble', icon = icon("sticky-note")),
    menuItem("Overview", tabName = 'Descriptive', icon = icon("eye")),
    menuItem("Interesting Findings", tabName = 'Findings', icon = icon("lightbulb")),
    menuItem("Performance Prediction", tabName = "MLR", icon = icon("chart-line"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'Preamble',
            fluidPage(
              fluidRow(
                column(width = 12,tabsetPanel(
                  tabPanel("Intro to Olympics", HTML('<iframe width="1120" height="630" src="https://www.youtube.com/embed/ENMhrEVa4XQ" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                  
                  tabPanel("Where did the medals go?",
                           box(
                             width = 12,
                             height = 700,
                             solidHeader = TRUE,
                             status = "primary",
                             collapsible = FALSE,
                             collapsed = FALSE,
                             title = "Distribution of Medals across Countries to-date",
                             plotlyOutput(outputId  = "map", 
                                          height = "600")
                           ),
                           
                           box(
                             width = 12,
                             height = 800,
                             solidHeader = TRUE,
                             status = "primary",
                             collapsible = FALSE,
                             collapsed = FALSE,
                             title = "Top 10 Countries with Highest Medal Count by Year",
                             selectInput(
                               inputId = "Year", 
                               label = "Select Year:", 
                               choices = setdiff(seq(2020, 1896, by = -4),c(1916,1940,1944))),
                             plotOutput(outputId  = "histogram", 
                                        height = "600"),
                             textOutput(outputId = "textGer")
                           )
                  ),
                  
                  tabPanel("Scope of Project",
                           box(
                             width = 8,
                             height = 450,
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             collapsed = FALSE,
                             status = "primary",
                             title = "Choice of countries to focus on",
                             tags$img(src='countries.png')
                              )
                          )
                )))
            )),
    
    tabItem(tabName = 'Descriptive',
            fluidPage(
              fluidRow(
                column(width = 12,tabsetPanel(
                  tabPanel("Factors affecting Medal Count",
                           box(width = 12,
                               height = 850,
                               solidHeader = TRUE, 
                               status = "primary",
                               title = 'Possible Factors that affect Medal Count',
                               radioButtons('xcol',
                                            label = tags$strong('Select Variables:'),
                                            choices = c('GDP' = 'GDP',
                                                        'Population' = 'Pop_Count',
                                                        'Mean Height of Athletes' = 'Height',
                                                        'Mean Weight of Athletes' = 'Weight',
                                                        'Mean Age of Athletes' = 'Age'),
                                            inline = FALSE),
                               sliderInput("Year1", "Year",
                                           min = 1896, max = 2020, value = 2020, step=4, sep=""
                               ),
                               textOutput(outputId = "textFac"),
                               plotOutput('countrystats', height = 500)
                               
                           )
                  ),
                  tabPanel("Breakdown by Sports",
                           box(
                             width = 12,
                             height = 1100,
                             solidHeader = TRUE,
                             status = "primary",
                             title = 'Proportion of Medals by Sport to-date',
                             collapsible = FALSE,
                             collapsed = FALSE,
                             selectInput(
                               inputId = "Team", 
                               label = "Select Country:", 
                               choices = list("Australia", "Brazil", "Canada",
                                              "China", "France", "Germany", "Hong Kong",
                                              "Hungary", "Indonesia", "Italy", "Japan",
                                              "Malaysia", "Netherlands", "Singapore",
                                              "South Africa", "Thailand", "United States")),
                             plotlyOutput(outputId  = "piechart", 
                                          height = "900")
                           )
                  )
                  
                )))
            )),
    
    tabItem(tabName = 'Findings',
            fluidPage(
              fluidRow(
                column(width = 12,tabsetPanel(
                  tabPanel("GDP per Capita & Rewards",
                           box(
                             width = 12,
                             height = 850,
                             solidHeader = TRUE,
                             status = "primary",
                             title = 'Relationship between GDP per Capita, Rewards & Medal Count in Tokyo 2020',
                             collapsible = FALSE,
                             collapsed = FALSE,
                             selectInput(
                               inputId = "Medal", 
                               label = "Select Medal Type:", 
                               choices = c("Gold","Silver","Bronze")),
                             plotlyOutput(outputId  = "bubblegraph", 
                                          height = "700")
                           )
                  ),
                  
                  tabPanel("Developed vs Developing Countries",
                           box(
                             width = 12,
                             height = 900,
                             solidHeader = TRUE,
                             status = "primary",
                             title = 'Proportion of Medal Count in Developed vs Developing Countries to-date',
                             collapsible = FALSE,
                             collapsed = FALSE,
                             plotOutput(outputId  = "radarchart", 
                                        height = "600"),
                             verbatimTextOutput("t_test_d"),
                             imageOutput(outputId = "image1")
                           )
                           
                           
                  )
                  
                )))
            )),
    
    tabItem(tabName = "MLR",
            fluidPage(
              #tags$head(
                #tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')), #remove horizontal scroll bar in slider
              #),
              fluidRow(
                column(
                  width = 12,
                  height = 100,
                  selectInput("country_selection", "Select Country",
                              choices =  list("Australia", "Brazil", "Canada",
                                              "China", "France", "Germany", "Hong Kong",
                                              "Hungary", "Indonesia", "Italy", "Japan",
                                              "Malaysia", "Netherlands", "Singapore",
                                              "South Africa", "Thailand", "United States")
                  )),

                  tabsetPanel(
                    
                    tabPanel("2024 Olympics Scorecard",
                             
                             box(width = 12, solidHeader = TRUE, status="primary", title="Adjustable Variables",
                                 sliderInput('weight','Mean Weight of Athletes in kg', min=40, max=120, value=50),
                                 
                                 splitLayout(
                                   numericInput("GDPgrowth", 'GDP Annual Growth Rate (from 2021 to 2024)',value=0),
                                   sliderInput('GDP', 'Country GDP in millions', min=0, max=30000000, value=15000000),
                                   cellWidths = c("25%","75%")
                                 ),
                                 
                                 splitLayout(
                                   numericInput("popgrowth", 'Population Annual Growth Rate (from 2021 to 2024)',value=0),
                                   sliderInput('pop', 'Country Population in thousands', min=0, max=3000000, value=1000000),
                                   cellWidths = c("25%","75%")
                                 ),
                                 
                                 sliderInput('Gold_reward', 'Reward for Gold Medalist in USD', min=0, max=1000000, value=50000),
                                 radioButtons('Host','Host Country',choices=c('Yes','No'),selected='No')
                             ),
                             
                             box(
                               width = 12, solidHeader = TRUE, status="primary", title="Medals Prediction in 2024 Olympics",
                               splitLayout(
                                 h4("Total Number of Medals:"),
                                 verbatimTextOutput("2024total"),
                                 cellWidths = c("15%", "15%")
                               ),
                               
                               h5("Total Number of Medals = -3.460e01 + 7.470e-01 x Mean Weight of Atheletes in kg + 5.004e-12 x Country GDP in millions + 2.505e-08 x Country Population in thousands - 2.225e-05 x Reward for Gold Medalist in USD + 4.877e+01 x Host,
                                  where Host is categorical with 1 = Host Country for Olympics and 0 = Not a Host Country")
                             ),
                             
                             box(
                               width = 12, solidHeader = TRUE, status="primary", title="Relative Ranking in 2024 Olympics",
                               plotOutput("medals_2024_graph")
                             ),
                             
                             box(
                               width = 12, solidHeader = TRUE, status="primary", title="Country Performance across Time",
                               plotOutput("country_line")
                             ),
                             
                             box(
                               width = 12, solidHeader = TRUE, status="primary", title="Scatter Plot",
                               plotOutput("country_each"),
                             )
                             
                    ),
                    
                    tabPanel(("Data for Prediction"),
                            box(width=12, solidHeader = TRUE, status="primary", title="Data to Display",
                                selectInput("data_var", "Select Independent Variables",
                                        choices = c("Mean Height" = "Height",
                                                    "Mean Weight" = "Weight",
                                                    "Mean Age" = "Age",
                                                    "GDP",
                                                    "Population" = "Pop_Count",
                                                    "Host Country" = "Host_country",
                                                    "Reward in USD for Gold Medalist" = "Gold_Reward",
                                                    "Reward in USD for Silver Medalist" = "Silver_Reward",
                                                    "Reward in USD for Bronze Medalist" = "Bronze_Reward"),
                                        multiple = TRUE)
                            ),
                            
                            box(width=12, solidHeader = TRUE, status="primary", title="Data Table",
                              DT::dataTableOutput("country_attributes_data")
                              ),
                             ),
                    
                    tabPanel(("Regression Model (Output in R)"),
                             box(
                               width = 12,
                               verbatimTextOutput("reg_total"),
                             )
                             
                    )
                )
              )
            )
    )
    
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "Summer Olympics"),
  sidebar,
  body
)


####Server####
server <- function(input, output) {
  
  output$textGer <- renderText({ 
    if (input$Year==1968){
      'East and West Germany participated as two different teams'
    }
    else if (input$Year==1972){
      'East and West Germany participated as two different teams'
    }
    else if (input$Year==1976){
      'East and West Germany participated as two different teams'
    }
  })
  
  output$map <- renderPlotly({
    medalCounts <- medaltally
    colnames(medalCounts)[5] <- "NOC"
    medalCounts <- left_join(medalCounts, NOCs, by= "NOC" ) %>% 
      select(region, NOC, Gold, Silver, Bronze)
    medalCounts <- medalCounts %>%
      filter(!is.na(region))%>%
      group_by(region) %>%
      summarise(Total=sum(Gold, Silver, Bronze))
    earth <- map_data("world")
    earth <- left_join(earth, medalCounts, by="region")
    
    plotlymap <- ggplot(earth, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = Total, label= region)) +
      labs(x = "", y = "") +
      guides(fill=guide_colourbar(title="medals")) +
      scale_fill_gradient(low="grey",high="blue") +
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
    ggplotly(plotlymap)
  })
  
  output$histogram <- renderPlot({
    
    medal_counts <- medaltally
    colnames(medal_counts)[5] <- "NOC"
    
    NOCs$region[NOCs$NOC=="FRG"] <- "West Germany"
    NOCs$region[NOCs$NOC=="GDR"] <- "East Germany"
    
    medal_counts <- left_join(medal_counts, NOCs, by= "NOC" ) %>%
      filter(!is.na(region))%>%
      filter(Year == input$Year)%>%
      group_by(region, Gold,Silver, Bronze)%>%
      summarise(Total=sum(Gold, Silver, Bronze))%>%
      arrange(desc(Total))
    medal_counts1<- medal_counts[1:10,]
    
    medal_counts_long <- pivot_longer(medal_counts1, Gold:Bronze, names_to = "Color", values_to = "Medals")  
    medal_counts_long$Color[medal_counts_long$Color=='Gold'] <- '1_gold_medal'
    medal_counts_long$Color[medal_counts_long$Color=='Silver'] <- '2_silver_medal'
    medal_counts_long$Color[medal_counts_long$Color=='Bronze'] <- '3_bronze_medal'
    
    ggplot(medal_counts_long, aes(x = reorder(region,Total), y = Medals))+
      labs(y= "Medal Count", x = "Country") + 
      geom_col(aes(fill = Color), width = 0.6)+
      geom_text(aes(x = region, y = Medals, label = Medals, group = Color),
                position = position_stack(vjust = .5))+
      
      scale_fill_manual(values=c("yellow","grey","brown"),
                        name='Medal Type',
                        breaks=c('1_gold_medal', '2_silver_medal', '3_bronze_medal'),
                        labels=c('Gold', 'Silver', 'Bronze')) +
      
      coord_flip()
  })
  
  output$piechart <- renderPlotly({
    athleteevent %>%
      filter(Team == input$Team, Season=='Summer') %>%
      filter(`Medal` != 'NA') %>%
      group_by(`Year`) %>%
      distinct(Event, .keep_all = TRUE)%>%
      group_by(`Sport`)%>%
      summarise(count = length(`Medal`)) %>%
      arrange(desc(count)) %>%
      plot_ly(labels = ~Sport, values = ~count, type = 'pie')%>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  
  output$bubblegraph <- renderPlotly({
    dataforplot <- country_attributes %>%
      filter(Year == 2020)
    if (input$Medal=="Gold"){
      dfp <- subset(dataforplot,select=c('Country_Name', 'GDP','Pop_Count', 'Gold', "Gold_Reward"))
      dfp$Gold_Reward <- as.numeric(as.character(dfp$Gold_Reward))
      ggplotly(ggplot(dfp, aes(x = GDP/Pop_Count, y = Gold, size = Gold_Reward,
                               color = Country_Name))+
                 geom_point(alpha = 0.7)+
                 scale_size(range = c(0.1, 50), name = "Gold Reward")+
                 ggtitle("Size of bubble denotes amount of reward")+
                 theme(plot.title = element_text(hjust = 0.5)))%>%
        layout(xaxis = list(title = 'GDP per Capita'),
               yaxis = list (title = 'Number of Medals'))
    } else if (input$Medal=="Silver"){
      dfp <- subset(dataforplot,select=c('Country_Name', 'GDP','Pop_Count', 'Silver', "Silver_Reward"))
      dfp$Silver_Reward <- as.numeric(as.character(dfp$Silver_Reward))
      ggplotly(ggplot(dfp, aes(x = GDP/Pop_Count, y = Silver, size = Silver_Reward,
                               color = Country_Name))+
                 geom_point(alpha = 0.7)+
                 scale_size(range = c(0.1, 50), name = "Silver Reward")+
                 ggtitle("Size of bubble denotes amount of reward")+
                 theme(plot.title = element_text(hjust = 0.5)))%>%
        layout(xaxis = list(title = 'GDP Per Capita'),
               yaxis = list (title = 'Number of Medals'))
    } else{
      dfp <- subset(dataforplot,select=c('Country_Name', 'GDP','Pop_Count', 'Bronze', "Bronze_Reward"))
      dfp$Bronze_Reward <- as.numeric(as.character(dfp$Bronze_Reward))
      ggplotly(ggplot(dfp, aes(x = GDP/Pop_Count, y = Bronze, size = Bronze_Reward,
                               color = Country_Name))+
                 geom_point(alpha = 0.7)+
                 scale_size(range = c(0.1, 50), name = "Bronze Reward")+
                 ggtitle("Size of bubble denotes amount of reward")+
                 theme(plot.title = element_text(hjust = 0.5)))%>%
        layout(xaxis = list(title = 'GDP Per Capita'),
               yaxis = list (title = 'Number of Medals'))
    }
  })
  
  output$radarchart <- renderPlot({
    devlopeding_data <- developeding[order(developeding$Classification),]
    devlopeding_data <- data.frame(devlopeding_data$Country_Name,devlopeding_data$Total_Medals)
    mm1 <- as.matrix(devlopeding_data)
    mm2 <- matrix(mm1, ncol = ncol(devlopeding_data), dimnames = NULL)
    mm3 <- t(mm2)
    mm3 <- as.data.frame(mm3)
    colnames(mm3) <- as.character(unlist(mm3[1,]))
    mm3 = mm3[-1, ]
    mm3 <- rbind(rep(2700,19) , rep(0,19) , mm3)
    rownames(mm3) <- 1:3
    mm3[ , c(1:17)] <- apply(mm3[ , c(1:17)], 2, function(x) as.numeric(as.character(x)))
    radarchart(mm3, axistype=1, cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,2700,500), cglwd=0.8,vlcex=0.8)
  })
  
  output$t_test_d <- renderPrint({
    Dev_Undev <- read.csv("Dev_Undev_17c.csv")
    
    all_medals <- sum(Dev_Undev$Total_Medals)
    
    developed <- filter(Dev_Undev, Classification == "Dev")
    dev_mean <- mean(developed$Total_Medals)
    dev_medals <- sum(developed$Total_Medals)
    
    undeveloped <- filter(Dev_Undev, Classification == "Undev")
    undev_mean <- mean(undeveloped$Total_Medals)
    undev_medals <- sum(undeveloped$Total_Medals)
    
    return (t.test(developed$Total_Medals, undeveloped$Total_Medals, alternative = "greater", 
                   sigma.x = sd(developed$Total_Medals), sigma.y = sd(undeveloped$Total_Medals), conf.level = 0.95))
  })
  
  output$textFac <- renderText({ 
    if (input$Year1==1916){
      'Olympics Games Cancelled due to World War I'
    }
    else if (input$Year1==1940){
      'Olympics Games Cancelled due to World War II'
    }
    else if (input$Year1==1944){
      'Olympics Games Cancelled due to World War II'
    }
  })
  
  
  output$countrystats <- renderPlot({
    analysis <- country_attributes %>%
      filter(Year == input$Year1)%>%
      select(Country_Name, input$xcol)
    col_name=input$xcol
    column <- input$xcol
    
    p <- ggplot(analysis, aes_string(x = names(analysis)[1], y = names(analysis)[2],fill=names(analysis)[1])) +
      geom_bar(stat = 'identity') +
      labs(y = input$xcol, x = 'Country')
    return(p)
    
  })
  
  
  
  
  
output$country_attributes_data <- DT::renderDataTable({
  DT::datatable(country_attributes_rounded[, c("Year", "Country_Name", "Total_Medal", 
                                       "Gold", "Silver", "Bronze", input$data_var), drop = FALSE],
                options = list(search = list(regex = TRUE, caseInsensitive = FALSE, search = input$country_selection)),
                rownames = FALSE
                )
  })


output$reg_total <- renderPrint({
  regression_total <- stepAIC(fit_total, direction ="both")
  return (summary(regression_total))
  })


output$country_each <- renderPlot({
  
  df2 <- country_attributes %>%
    filter(Country_Name %in% input$country_selection) %>%
    select(everything())
  model1 <- lm(df2$Total_Medal ~ df2$Weight)
  model2 <- lm(df2$Total_Medal ~ df2$GDP)
  model3 <- lm(df2$Total_Medal ~ df2$Pop_Count)
  #model4 <- lm(df2$Total_Medal ~ df2$Gold_Reward)
  #model5 <- lm(df2$Total_Medal ~ df2$Host_binary)
  par(mfrow=c(1,3))
  q1 <- plot(df2$Weight, df2$Total_Medal, xlab="Weight",ylab="Total medal") 
  abline(model1, col = "red") 
  legend("topleft",legend=paste("R2 is", format(summary(model1)$r.squared,digits=3)))
  q2 <- plot(df2$GDP, df2$Total_Medal, xlab="GDP",ylab="Total medal")
  abline(model2, col = "red")
  legend("topleft",legend=paste("R2 is", format(summary(model2)$r.squared,digits=3)))
  q3 <- plot(df2$Pop_Count, df2$Total_Medal, xlab="Population",ylab="Total medal") 
  abline(model3, col = "red") 
  legend("topleft",legend=paste("R2 is", format(summary(model3)$r.squared,digits=3)))
  #  q4 <- plot(df2$Gold_Reward, df2$Total_Medal, xlab="Gold Reward",ylab="Total medal")
  #    abline(model4, col = "red")
  #    legend("topleft",legend=paste("R2 is", format(summary(model4)$r.squared,digits=3)))
  #q5 <- plot(df2$Host_binary, df2$Total_Medal, xlab="Host country",ylab="Total medal")
  #abline(model5, col = "red")
  #legend("topleft",legend=paste("R2", format(summary(model5)$r.squared,digits=3)))
  
})

output$"2024total" <- renderPrint({
  olymp_binary <- if_else(input$Host == "Yes", 1, 0)
  temp <- max(0, (round(
    (coefficients(regression_total)[1]+coefficients(regression_total)[2]*input$weight + coefficients(regression_total)[3]*input$GDP*1000000 + coefficients(regression_total)[4]*input$pop*1000 
          +coefficients(regression_total)[5]*input$Gold_reward +coefficients(regression_total)[6]*olymp_binary))
  ))
  return (paste(temp, collapse = '\n') %>% cat())
  
  })


observeEvent(input$country_selection, {
  updateSliderInput(inputId = "weight",
                    value = mean(country_attributes$Weight[country_attributes$Country_Name == input$country_selection & country_attributes$Year >= 2012]))
  })  

observeEvent(c(input$country_selection, input$GDPgrowth), {
   updateSliderInput(inputId = "GDP",
                    value = country_attributes$GDP[country_attributes$Country_Name == input$country_selection & country_attributes$Year == 2020]
                      *((100+input$GDPgrowth)/100)^4 / 1000000)
  })

observeEvent(c(input$country_selection, input$popgrowth), {
  updateSliderInput(inputId = "pop",
                    value = country_attributes$Pop_Count[country_attributes$Country_Name == input$country_selection & country_attributes$Year == 2020]
                    *((100+input$popgrowth)/100)^4 / 1000)
})


observeEvent(input$country_selection, {
  updateSliderInput(inputId = "Gold_reward",
                    value = mean(country_attributes$Gold_Reward[country_attributes$Country_Name == input$country_selection &country_attributes$Year >= 2012]))
  }) 


observeEvent(input$country_selection, {
  if(input$country_selection == "France"){
    updateRadioButtons(inputId = "Host", selected = 'Yes')
  }
  else{updateRadioButtons(inputId = "Host", selected = 'No')}
}) 


data_2024 <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("Country_Name", "Year","Total_Medal"))))
data_2024$Country_Name <- as.character(data_2024$Country_Name)
data_2024$Total_Medal <- as.numeric(data_2024$Total_Medal)

countries <- c("Australia", "Brazil", "Canada",
               "China", "France", "Germany", "Hong Kong",
               "Hungary", "Indonesia", "Italy", "Japan",
               "Malaysia", "Netherlands", "Singapore",
               "South Africa", "Thailand", "United States")


output$"medals_2024_graph" <- renderPlot({
  
olymp_binary <- if_else(input$Host == "Yes", 1, 0)

for (i in countries) {
  
  if(i == input$country_selection){
    data_2024 <- data_2024 %>%
      add_row(Country_Name=i, Year=2024, Total_Medal=max(0,(round(coefficients(regression_total)[1]+coefficients(regression_total)[2]*input$weight + coefficients(regression_total)[3]*input$GDP*1000000 + coefficients(regression_total)[4]*input$pop*1000 
                                                + coefficients(regression_total)[5]*input$Gold_reward +coefficients(regression_total)[6]*olymp_binary)
             )))
  }
  
  else if (i == "France") {
    data_2024 <- data_2024 %>%
      add_row(Country_Name=i, Year=2024, Total_Medal=max(0,(round(coefficients(regression_total)[1]+coefficients(regression_total)[2]*mean(country_attributes$Weight[country_attributes$Country_Name == i & country_attributes$Year >= 2012])
                                                + coefficients(regression_total)[3]*country_attributes$GDP[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                + coefficients(regression_total)[4]*country_attributes$Pop_Count[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                + coefficients(regression_total)[5]*mean(country_attributes$Gold_Reward[country_attributes$Country_Name == i &country_attributes$Year >= 2012])
                                                + coefficients(regression_total)[6]*1) 
      )))
  }
  
  else{
    data_2024 <- data_2024 %>%
      add_row(Country_Name=i, Year=2024,Total_Medal=max(0,(round(coefficients(regression_total)[1]+coefficients(regression_total)[2]*mean(country_attributes$Weight[country_attributes$Country_Name == i & country_attributes$Year >= 2012])
                                                                 + coefficients(regression_total)[3]*country_attributes$GDP[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                                 + coefficients(regression_total)[4]*country_attributes$Pop_Count[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                                 + coefficients(regression_total)[5]*mean(country_attributes$Gold_Reward[country_attributes$Country_Name == i &country_attributes$Year >= 2012])
                                                                 + coefficients(regression_total)[6]*0)
      )))
  }
}


data_2024 <- merge(data_2024, aggregate(gold_prop ~ Country_Name, country_attributes, mean), by = 'Country_Name')
data_2024 <- merge(data_2024, aggregate(silver_prop ~ Country_Name, country_attributes, mean), by = 'Country_Name')

data_2024$gold_medal <- round(data_2024$gold_prop * data_2024$Total_Medal)
data_2024$silver_medal <- round(data_2024$silver_prop * data_2024$Total_Medal)
data_2024$bronze_medal <- data_2024$Total_Medal - data_2024$gold_medal - data_2024$silver_medal
data_2024_long <- pivot_longer(data_2024, gold_medal:bronze_medal, names_to = "Color", values_to = "Medals")  
data_2024_long$Color[data_2024_long$Color=='gold_medal'] <- '1_gold_medal'
data_2024_long$Color[data_2024_long$Color=='silver_medal'] <- '2_silver_medal'
data_2024_long$Color[data_2024_long$Color=='bronze_medal'] <- '3_bronze_medal'

ggplot(data_2024_long, aes(x = reorder(Country_Name,Total_Medal), y = Medals))+
  
  labs(y= "Medal Count", x = "Country") + 
  
  geom_col(aes(fill = Color), width = 0.6, 
           color=(ifelse(data_2024_long$Country_Name==input$country_selection &
                           data_2024_long$Total_Medal!=0, "black", "gray")),
           size=(ifelse(data_2024_long$Country_Name==input$country_selection & 
                          data_2024_long$Total_Medal!=0, 1, 0.1)))+
  
  
  
  geom_text(aes(x = Country_Name, y = Medals, label = ifelse(Medals > 0, Medals, numeric(0)), group = Color),
            position = position_stack(vjust = .5))+
  
  scale_fill_manual(values=c("yellow","grey","brown"),
                    name='Medal Type',
                    breaks=c('1_gold_medal', '2_silver_medal', '3_bronze_medal'),
                    labels=c('Gold', 'Silver', 'Bronze')) +
  
  coord_flip()
})


output$country_line <- renderPlot({
  
  olymp_binary <- if_else(input$Host == "Yes", 1, 0)
  
  for (i in countries) {
    
    if(i == input$country_selection){
      data_2024 <- data_2024 %>%
        add_row(Country_Name=i, Year=2024, Total_Medal=max(0,(round(coefficients(regression_total)[1]+coefficients(regression_total)[2]*input$weight + coefficients(regression_total)[3]*input$GDP*1000000 + coefficients(regression_total)[4]*input$pop*1000 
                                                             +coefficients(regression_total)[5]*input$Gold_reward +coefficients(regression_total)[6]*olymp_binary)
        )))
    }
    
    else if (i == "France") {
      data_2024 <- data_2024 %>%
        add_row(Country_Name=i, Year=2024, Total_Medal=max(0,(round(coefficients(regression_total)[1]+coefficients(regression_total)[2]*mean(country_attributes$Weight[country_attributes$Country_Name == i & country_attributes$Year >= 2012])
                                                             + coefficients(regression_total)[3]*country_attributes$GDP[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                             + coefficients(regression_total)[4]*country_attributes$Pop_Count[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                             +coefficients(regression_total)[5]*mean(country_attributes$Gold_Reward[country_attributes$Country_Name == i &country_attributes$Year >= 2012])
                                                             + coefficients(regression_total)[6]*1) 
        )))
    }
    
    else{
      data_2024 <- data_2024 %>%
        add_row(Country_Name=i, Year=2024,Total_Medal=max(0,(round(coefficients(regression_total)[1]+coefficients(regression_total)[2]*mean(country_attributes$Weight[country_attributes$Country_Name == i & country_attributes$Year >= 2012])
                                                            + coefficients(regression_total)[3]*country_attributes$GDP[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                            + coefficients(regression_total)[4]*country_attributes$Pop_Count[country_attributes$Country_Name == i & country_attributes$Year == 2020]
                                                            +coefficients(regression_total)[5]*mean(country_attributes$Gold_Reward[country_attributes$Country_Name == i &country_attributes$Year >= 2012])
                                                            + coefficients(regression_total)[6]*0)
        )))
    }
  }
  
  country_attributes_2024 <- merge(country_attributes,data_2024,
                                   by=c("Country_Name"="Country_Name", "Year"="Year", "Total_Medal"="Total_Medal"),
                                   all=TRUE)
  
  
  
  ggplot(country_attributes_2024 %>% filter(Country_Name == input$country_selection),
         aes(x=Year,y=Total_Medal)) + 
    labs(y= "Medal Count", x = "Year") + 
    geom_line(data=subset(country_attributes_2024, Year<=2020 & Country_Name == input$country_selection), 
              size=0.8,color="darkblue") + 
    geom_line(data=subset(country_attributes_2024, Year>=2020 & Country_Name == input$country_selection), 
              size=0.8,color="darkblue",linetype=2) + 
    scale_x_continuous(breaks=country_attributes_2024$Year)
  
})


}

# Run the application 
shinyApp(ui = ui, server = server)
