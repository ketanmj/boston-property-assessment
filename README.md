# Boston Property Assessment Dashboard
---------------------------------------------------------------------

Dashboard to Display Property Assessment Trends in the City of Boston:

1. First chart shows number of properties built over the specified range of years - adjust the slider to specify range of years
2. Second chart shows proportion of selected property type in different city neighborhoods - select property type using radio buttons
3. Third chart shows scatter-plot for 'gross area of property' vs 'total value of property' for selected city neighborhood - select neighborhood using drop-down menu
---------------------------------------------------------------------

Commands to run the dashboard app using RStudio console:

library(shiny)

runGitHub('boston-property-assessment', 'ketanmj')
---------------------------------------------------------------------

Data Source:

https://data.boston.gov/dataset/property-assessment
---------------------------------------------------------------------
