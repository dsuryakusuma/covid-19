# covidhacks Dash + Plotly
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

library(readr)
library(httr)
library(tidyverse)
library(lubridate)

library(kableExtra)


########### DATA PRE-PROCESSING & GLOBAL VARIABLES #############

td <- Sys.Date()
## fetch and reshape data from DataHub
covid_combined <- as_tibble(read_csv(file = "https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv"))
covid_keycountries_counts <- as_tibble(read_csv("https://datahub.io/core/covid-19/r/key-countries-pivoted.csv"))


# normalize some data (US, UK)
covid_combined$`Country/Region`[which(covid_combined$`Country/Region` == "US")] <- "United States"
covid_keycountries_counts <- covid_keycountries_counts %>% rename(`United States` = US)
covid_keycountries_counts <- covid_keycountries_counts %>% rename(`United Kingdom` = `United_Kingdom`)


covid_keycountries_counts_pivoted <- covid_keycountries_counts %>%
  pivot_longer(cols = - c("Date"),
               names_to = "Regions",
               values_to = "Confirmed Counts")
                
# get death counts of key countries
keycountries <- covid_keycountries_counts_pivoted$Regions %>% unique()
    

covid_keycountries_deaths_pivoted <- covid_combined %>% 
  filter(`Country/Region` %in% keycountries) %>%
  select(c(Date, `Country/Region`, Deaths)) 

  covid_keycountries_deaths <- covid_keycountries_deaths_pivoted %>%
    pivot_wider(names_from = `Country/Region`,
                values_from = Deaths,
                values_fn = list(Deaths = max))

covid_keycountries_recovered_pivoted <- covid_combined %>% 
  filter(`Country/Region` %in% keycountries) %>%
  select(c(Date, `Country/Region`, Recovered)) 

  covid_keycountries_recovered <- covid_keycountries_recovered_pivoted %>%
    pivot_wider(names_from = `Country/Region`,
                values_from = Recovered,
                values_fn = list(Recovered = max))


    # helper function
    getTimeSeries <- function(country, col, df = covid_combined) {
      # country: i.e. 'United States'
      # col: time series column; 'Confirmed', 'Recovered', 'Deaths' 
      # df: covid_combined
      df %>% filter(`Country/Region` == country) %>% select(c(Date, col)) %>% return()
    }
    

# rank confirmed counts (also available for cross-checking on 'https://www.worldometers.info/coronavirus/#countries')
mostrecent <- max(covid_keycountries_counts$Date)
ranking_confirmed <- covid_keycountries_counts_pivoted %>% 
  filter(Date == mostrecent) %>% 
  select(c(Regions, `Confirmed Counts`)) %>%
  arrange( desc(`Confirmed Counts`) ) %>%
  pull('Regions')

# reorder columns in decreasing order:
covid_keycountries_counts <- covid_keycountries_counts %>% select(c(Date, ranking_confirmed))
# covid_keycountries_counts_pivoted$Regions <- covid_keycountries_counts_pivoted$Regions %>% factor(labels = ranking_confirmed)


# plot of confirmed cases
plotCases <- covid_keycountries_counts_pivoted %>% 
  plot_ly(
    x = ~ Date,
    y = ~ `Confirmed Counts`,
    color = ~ Regions,
    mode = 'lines'
  ) %>% layout(title = 'Confirmed Cases of COVID-19 by Region',
    legend = list(x = 0, y = 1, font = list(size = 12), bgcolor = "#F2F2F2"))

plotDeaths <- covid_keycountries_deaths_pivoted %>%
  plot_ly(
    x = ~ Date,
    y = ~ Deaths,
    color = ~ `Country/Region`,
    mode = 'lines'
  ) %>% layout(title = 'Deaths to COVID-19 by Region',
    legend = list(x = 0, y = 1, font = list(size = 12), bgcolor = "#F2F2F2"))




# rank confirmed counts (also available for cross-checking on 'https://www.worldometers.info/coronavirus/#countries')
mostrecent <- max(covid_keycountries_counts$Date)
ranking_confirmed <- covid_keycountries_counts_pivoted %>% 
  filter(Date == mostrecent) %>% 
  select(c(Regions, `Confirmed Counts`)) %>%
  arrange( desc(`Confirmed Counts`) ) %>%
  pull('Regions')

# reorder columns in decreasing order:
covid_keycountries_counts <- covid_keycountries_counts %>% select(c(Date, ranking_confirmed))
covid_keycountries_counts_pivoted$Regions <- covid_keycountries_counts_pivoted$Regions %>% factor(labels = ranking_confirmed)

# covid_keycountries_counts_pivoted %>%   ggplot() + geom_line(aes(x = Date, y = `Confirmed Counts`, color = Regions))


# general styling
colors <- list(
  background = '#FFFFFF', # white background
  text = '#343a40' # dark-grey text 
)



################### DASH WEB APP ###################

app <- Dash$new(external_stylesheets = 
  "https://files.dsury.com/css/lux/bootstrap.min.css"
  # "https://codepen.io/chriddyp/pen/bWLwgP.css"
                )


plotStyle <-  list(title='Deaths by Region',
                   plot_bgcolor = colors$background,
                   paper_bgcolor = colors$background,
                   font = list(color = colors$text))


## Region 

# top regions to display on dropdown
regions.list <- list(
  list(label = "Global Total", value = "Global"),
  list(label = "United States", value = "US"),
  list(label = "Italy", value = "Italy"),
  list(label = "China", value = "China"),
  list(label = "Spain", value = "Spain"),
  list(label = "Germany", value = "Germany"),
  list(label = "France", value = "France"),
  list(label = "Iran", value = "Iran"),
  list(label = "United Kingdom", value = "United Kingdom")
)

region <- "Global" # from drop-down (not functional currently from callback)

regions.dropdown <- list(
  dccDropdown(
    options = regions.list,
    value = region, # the default upon loading
    id = 'regions-dropdown',
    searchable = FALSE,
    placeholder = "Select a region..."
  ),
  htmlDiv(id = 'output-container')
)



appBlurb <- htmlDiv(children = "How important is social distancing at times like these with COVID-19? Why do we need travel restrictions, stay-at-home orders, and closures at restaurants and events? Our app helps you visualize the differences in risk and effects of close social interaction at this time. 

In this app, toggle through decisions like not attending large social events and see how these can drastically decrease the number of elderly and at-risk people indirectly harmed by you!
",
  style = list(
    "font-size" = "10pt",
    "color" = colors$text
  ))


casesPlot <- dccGraph(
  figure = plotCases,
  style = plotStyle
  # layout = plotStyle
)

deathPlot <- dccGraph(
  figure = plotDeaths,
  style = plotStyle
)


centerNarrow <- list(
  # backgroundColor = colors$background,
  # color = colors$text,
  textAlign = 'center',
  marginLeft = 'auto',
  marginRight = 'auto',
  # className = 'container',
  width = "70%"
  # padding = "30px"
)


if (region == 'Global') {
  tmp <- covid_combined %>%
    filter(Date == max(Date))
  N.confirmed <- tmp %>% pull(Confirmed) %>% sum()
  N.deaths <- tmp %>% pull(Deaths) %>% sum()
  N.recovered <- tmp %>% pull(Recovered) %>% sum()
} else {
  tmp <- covid_combined %>%
    filter(Date == max(Date)) %>%
    filter(`Country/Region` == region)
  N.confirmed <- tmp %>% pull(Confirmed) %>% sum()
  N.deaths <- tmp %>% pull(Deaths) %>% sum()
  N.recovered <- tmp %>% pull(Recovered) %>% sum()
}

# current death count
currentCounts <- htmlDiv( 
  list(  htmlH1(paste( N.deaths, 'deaths')),
         # htmlBr(),
         htmlH1(paste( N.confirmed, 'infected' )),
         # htmlBr(),
         htmlH4('from COVID-19 by'),
         # htmlBr(),
         htmlH3( paste(wday(td, label = TRUE),  month(td, label = TRUE), day(td), year(td) ) )),
  htmlBr())





#######################################################

######## HTML LAYOUT #############



app$layout(
  htmlBr(),
  htmlDiv(regions.dropdown, style = centerNarrow),
  htmlBr(),
  htmlDiv(currentCounts, style = centerNarrow),
  htmlBr(),
  htmlDiv(appBlurb, style = centerNarrow),
  htmlBr(),
  htmlDiv(casesPlot, style = centerNarrow),
  htmlBr(),
  htmlDiv(deathPlot, style = centerNarrow)
)


################################################


######### CALLBACK FUNCTIONS ###################


# change region from dropdown => change metrics
app$callback(
  output('output-container', 'children'),
  params = list(input('regions-dropdown', 'value')),
  
  function(dropdown_value) {
    region <- dropdown_value
    # return(dropdown_value)
  }
)



app$run_server()
