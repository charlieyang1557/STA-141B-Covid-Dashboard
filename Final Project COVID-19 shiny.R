#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyWidgets)
library(httr)
library(tidyverse)
library(jsonlite)
library(scales)

country <- GET("https://api.covid19api.com/countries") %>% content(., as = "text") %>% fromJSON() %>% arrange(Country)


ui <- fluidPage(
    setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"),
    titlePanel("COVID-19 Infomation"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("country","Country",choices = c("-",country$Country %>% set_names(country$name))),
                        dateRangeInput("date", "Year Month Date",
                                       start = NULL, end = NULL, min = "2020/03/01", max = Sys.Date() -1 ,format = "yyyy/mm/dd",
                                       separator = " - "),
                        selectInput("status","Status",choices = c("confirmed","deaths","recovered"))
                    ),
                    mainPanel(h4("Status vs. Date"),  plotOutput("scatter"),
                              h4("Status Summary During the Time Period"),tableOutput("sum_stats"),
                              h4("Global/Country Summary"),tableOutput("country_sum")
                        
                    )
                ))

server <- function(input, output,session){
   
    summary <- reactive({
        GET("https://api.covid19api.com/summary") %>%
        content(., as = "text") %>%
        fromJSON()
    })
    
    
    country_code <- reactive({
        code <- "afghanistan"
        if (input$country != "-") {
            code <- country$Slug[which(country$Country == input$country)]
        }
        code
    })
    
    covid <- reactive({
        GET(str_glue("https://api.covid19api.com/dayone/country/{input_country}/status/{input_status}",
                      input_country = country_code(),
                      input_status = input$status)) %>% 
            content(., as = "text") %>%
            fromJSON() %>% 
            mutate(Month_Date = substr(Date,6,10))})
    
    
    covid_time <- reactive({
        GET(str_glue("https://api.covid19api.com/total/country/{input_country}/status/{input_status}?from={input_time_1}T00:00:00Z&to={input_time_2}T00:00:00Z",
                     input_country = country_code(),
                     input_status = input$status,
                     input_time_1 = input$date[1],
                     input_time_2 = input$date[2])) %>%
            content(., as = "text") %>%
            fromJSON()%>% 
            mutate(Month_Date = substr(Date,6,10))
    }) 
    
    sum_status <- reactive({stats <- tibble(Total_status = covid_time()$Cases[nrow(covid_time())],
                           Average_status = covid_time()$Cases[nrow(covid_time())]/nrow(covid_time()),
                           status_growth = covid_time()$Cases[nrow(covid_time())] - covid_time()$Cases[1],
                           status_Growth_Rate = percent(status_growth/Total_status))
    names(stats) = c(paste("Total",input$status),paste("Average",input$status),paste(input$status, "Population Growth"),paste(input$status,"Growth Rate"))
    stats
    })

    country_summary <- reactive({
        summary()$Countries %>%
            filter(Country == input$country) %>%
            select(Country, NewConfirmed ,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered) %>%
            mutate(ConfirmedGrowthRate = percent(NewConfirmed/TotalConfirmed),DeathGrowthRate = percent(NewDeaths/TotalDeaths),RecoveredGrowthRate = percent(NewRecovered/TotalRecovered),DeathRate = percent(TotalDeaths/TotalConfirmed))
    })
    
    
    global_summary <- reactive({
        tibble(NewConfirmed = summary()$Global$NewConfirmed,
                   TotalConfirmed = summary()$Global$TotalConfirmed,
                   NewDeaths = summary()$Global$NewDeaths,
                   TotalDeaths = summary()$Global$TotalDeaths,
                   NewRecovered = summary()$Global$NewRecovered,
                   TotalRecovered = summary()$Global$TotalRecovered) %>% 
            mutate(ConfirmedGrowthRate = percent(NewConfirmed/TotalConfirmed),DeathGrowthRate = percent(NewDeaths/TotalDeaths),RecoveredGrowthRate = percent(NewRecovered/TotalRecovered),DeathRate = percent(TotalDeaths/TotalConfirmed))
    })
    
    
    output$scatter <- renderPlot({
        if (is.null(input$date) == TRUE) {ggplot(data = covid(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point()  +  
                theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + 
                ggtitle(paste("Total", input$status, "in 2020"))} 
        else{ggplot(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point()  +  
                theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) +
                ggtitle(paste("Total", input$status, "in", input$date[1], "to", input$date[2]))}
    })
    
    output$sum_stats <- renderTable({
        if(input$country == "-"){}
        else{sum_status()} 
            })
    
    output$country_sum <- renderTable({
        if(input$country == "-"){global_summary()}
        else{country_summary()}
    })
} 





shinyApp(ui = ui, server = server)
