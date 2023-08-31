# 1.1 Load libraries
require(shiny)
require(shinydashboard)
require(ggplot2)
require(dplyr)


# 1.2 Stop if session is not interactive
if (!interactive()) {
  stop("R session is not interactive. Haulting execution!")
}


# 1.3 Load data
dataset <- read.csv(file = "fy2023-property-assessment-data-1.csv",
                    header = TRUE,
                    na.strings = c(""," ","NA"))


# 1.4 Transform data
# Remove city neighborhoods with less than 100 properties
cities <- table(dataset$CITY) > 100
cities <- names(cities[cities == TRUE])

# Property types categorized by codes
cityProportions <- dataset %>%
  select(c(CITY, LUC)) %>%
  na.omit() %>%
  filter(CITY %in% cities) %>%
  mutate(TYPE =
           case_when((LUC <= 31) ~ 1,
                     ((LUC >= 101) & (LUC <= 110)) ~ 2,
                     ((LUC >= 111) & (LUC <= 211)) ~ 3,
                     ((LUC >= 300) & (LUC <= 399)) ~ 4,
                     ((LUC >= 400) & (LUC <= 465)) ~ 5,
                     (LUC >= 466) ~ 6)) %>%
  select(-LUC) %>%
  group_by(CITY, TYPE) %>%
  summarize(COUNT = n()) %>%
  transform(PROPORTION = ave(COUNT,
                             CITY,
                             FUN = prop.table))

# Replace invalid 'year built' value
dataset$YR_BUILT[dataset$YR_BUILT == 20198] <- 2018


# 2.1 Define dashboard UI

# 2.1.1 Define dashboard header
dashboard_header <- dashboardHeader(
  title = "Property Trends in the City of Boston",
  titleWidth = 400
)


# 2.1.2 Define dashboard sidebar (tabs)
dashboard_sidebar <- dashboardSidebar(
  disable = TRUE,
  sidebarMenu(
    menuItem(text = "Dashboard", tabName = "dashboard")
    )
)


# 2.1.3 Define dashboard body
dashboard_body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            # First row
            fluidRow(
              box(width = 12,
                  status = "primary",
                  
                  # Slider input
                  sliderInput(inputId = "slider1_oty",
                              label = "Select Range of Years:",
                              min = min(dataset$YR_BUILT, na.rm = TRUE),
                              max = 2022,
                              value = c(1973, 2022), step = 1,
                              sep = ""),
                  
                  # First update button
                  actionButton(inputId = "button1_uc",
                               label = "Update Chart",
                               class = "btn-default")),
              box(width = 12,
                  status = "primary",
                  
                  # First chart
                  plotOutput(outputId = "plot1_oty", height = 350))
              ),
            # Second row
            fluidRow(
              box(width = 3,
                  status = "success",
                  height = 475,
                  
                  # Radio buttons
                  radioButtons(inputId = "radiob1_type",
                               label = "Select Type of Properties:",
                               choices = list("Multi-use" = 1,
                                              "Residential" = 2,
                                              "Apartments" = 3,
                                              "Commercial" = 4,
                                              "Industrial" = 5,
                                              "Exempt/Other" = 6),
                               selected = 1),
                  
                  # Second update button
                  actionButton(inputId = "button2_uc",
                               label = "Update Chart",
                               class = "btn-default")),
              box(width = 9,
                  status = "success",
                  
                  # Second chart
                  plotOutput(outputId = "plot2_type", height = 450))
              ),
            fluidRow(
              box(width = 3,
                  height = 475,
                  
                  # Drop-down menu
                  selectInput(inputId = "selectb1_sp",
                              label = "Select City Neighborhood:",
                              choices = cities,
                              selected = "BOSTON"),
                  
                  # Third update button
                  actionButton(inputId = "button3_uc",
                               label = "Update Chart",
                               class = "btn-default")),
              box(width = 9,
                  height = 475,
                  
                  # Third chart
                  plotOutput(outputId = "plot3_sp", height = 450))
              )
            )
    )
  )


# 2.2 Define dashboard UI - Combine Sections 2.1.1, 2.1.2, 2.1.3
ui <- dashboardPage(
  header = dashboard_header,
  sidebar = dashboard_sidebar,
  body = dashboard_body
)


# 2.3 Define server logic for dashboard application
server <- function(input, output) {
  
  # First chart
  output$plot1_oty <- renderPlot({
    
    input$button1_uc
    
    # Remove rows with missing years
    years <- dataset$YR_BUILT[!is.na(dataset$YR_BUILT)]
    
    # Input values for years slider
    isolate({
      yearMin <- input$slider1_oty[1]
      yearMax <- input$slider1_oty[2]
    })
    
    # Filter years as per input
    condition1 <- yearMin <= years
    condition2 <- yearMax >= years
    years <- years[condition1 & condition2]
    plot1Title <- sprintf("Number of Properties Built in Boston Between %d and %d",
                          yearMin, yearMax)
    
    # Plot first chart
    ggplot(data = NULL,
           mapping = aes(x = years)) +
      geom_histogram(binwidth = 1,
                     color = "darkblue",
                     fill = "lightblue") +
      ggtitle(plot1Title) +
      xlab("Year") +
      ylab("Number of Properties Built")
  })
  
  # Second chart
  output$plot2_type <- renderPlot({
    
    input$button2_uc
    
    # Input values for radio buttons
    isolate({
      type <- input$radiob1_type
    })
    
    # Chart title as per input
    if (type == 1) {typeLabel = "Multi-use"}
    else if (type == 2) {typeLabel = "Residential"}
    else if (type == 3) {typeLabel = "Apartments"}
    else if (type == 4) {typeLabel = "Commercial"}
    else if (type == 5) {typeLabel = "Industrial"}
    else {typeLabel = "Exempt/Other"}

    plot2Title <- sprintf("Proportions of %s Properties by City Neighborhood",
                          typeLabel)
    
    # Plot second chart
    ggplot(data = cityProportions[cityProportions$TYPE == type, ],
           mapping = aes(x = reorder(x = CITY, PROPORTION), y = PROPORTION)) +
      geom_bar(stat = "identity",
               color = 'black',
               fill = 'gray',
               width = 0.6) +
      coord_flip() +
      ggtitle(plot2Title) +
      xlab("City Neighborhood") +
      ylab("Proportion")
  })
  
  # Third chart
  output$plot3_sp <- renderPlot({
    
    input$button3_uc
    
    isolate({
      city <- input$selectb1_sp
    })
    
    # Remove rows with missing values
    temp <- dataset[dataset$CITY == city, ] %>%
      select(c(CITY, GROSS_AREA, TOTAL_VALUE)) %>%
      na.omit()
    
    ga <- as.numeric(temp$GROSS_AREA)
    tv <- as.numeric(temp$TOTAL_VALUE)
    
    # Detect outliers for gross area and total value by city
    temp['GROSS_AREA_OUT'] <- !ga %in% boxplot.stats(ga)$out
    temp['TOTAL_VALUE_OUT'] <- !tv %in% boxplot.stats(tv)$out
    
    # Remove outliers
    newData <- temp %>%
      filter((GROSS_AREA_OUT == TRUE) & (TOTAL_VALUE_OUT == TRUE)) %>%
      select(-c(GROSS_AREA_OUT, TOTAL_VALUE_OUT))
    
    # Plot third chart
    ggplot(data = newData,
           mapping = aes(x = GROSS_AREA, y = TOTAL_VALUE)) +
      geom_point(color = "darkblue") +
      ggtitle("Gross Area vs Total Value of Property") +
      xlab("Gross Area") +
      ylab("Total Value")
    
  })
  
}


# 3 Run the dashboard application
shinyApp(ui = ui,
         server = server)
