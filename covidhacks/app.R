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


########### DATA PRE-PROCESSING  #############

td <- Sys.Date()
## fetch and reshape data from DataHub

covid_combined <- as_tibble(read_csv(file =  "https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv"))
covid_keycountries_counts <- as_tibble( read_csv("https://datahub.io/core/covid-19/r/key-countries-pivoted.csv") )

covid_keycountries_counts <- covid_keycountries_counts %>% rename(United_States = US)

covid_keycountries_counts_pivoted <- covid_keycountries_counts %>% 
  pivot_longer(cols = - c("Date"), 
               names_to = "Regions",
               values_to = "Confirmed Counts")
                
# get death counts of key countries
keycountries <- covid_keycountries_counts_pivoted$Regions %>% unique()

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



plotConfirmed <- covid_keycountries_counts_pivoted %>% 
  plot_ly(
    x = ~ Date,
    y = ~ `Confirmed Counts`,
    color = ~ Regions,
    mode = 'lines'
  ) %>% layout(legend = list(x = 0, y = 1, font = list(size = 8), bgcolor = "#F2F2F2"))



covid_combined <- as_tibble(read_csv(file =  "https://datahub.io/core/covid-19/r/time-series-19-covid-combined.csv"))
covid_keycountries_counts <- as_tibble( read_csv("https://datahub.io/core/covid-19/r/key-countries-pivoted.csv") )

covid_keycountries_counts <- covid_keycountries_counts %>% rename(United_States = US)

covid_keycountries_counts_pivoted <- covid_keycountries_counts %>% 
  pivot_longer(cols = - c("Date"), 
               names_to = "Regions",
               values_to = "Confirmed Counts")

# get death counts of key countries
keycountries <- covid_keycountries_counts_pivoted$Regions %>% unique()

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

covid_keycountries_counts_pivoted %>%   ggplot() + geom_line(aes(x = Date, y = `Confirmed Counts`, color = Regions))


# general styling
colors <- list(
  background = '#111111',
  text = '#7FDBFF'
)



################### DASH WEB APP ###################

app <- Dash$new(external_stylesheets = 
                  # "https://files.dsury.com/css/lux/bootstrap.min.css"
                  "https://codepen.io/chriddyp/pen/bWLwgP.css"
                )


plotStyle <-  list(title='Deaths by Region',
                   plot_bgcolor = colors$background,
                   paper_bgcolor = colors$background,
                   font = list(color = colors$text))


regions.vec <- covid_combined %>% pull(`Country/Region`) %>% unique() # supply to dropdown
regions.vec <- c('Total', regions.vec)


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

regions.dropdown <- list(
  dccDropdown(
    options = regions.list,
    value = 'US' # the default upon loading
  )
)

region <- "United Kingdom" # from drop-down


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



### cleaner implementation of app


# current death count
deathCounts <- htmlDiv(  htmlH1(children = N.deaths),
  htmlBr(),
  htmlH1('deaths due to COVID-19 by'),
  htmlBr(),
  htmlH1( paste(wday(td, label = TRUE),  month(td, label = TRUE), day(td), ", ", year(td), "." ) ))


appTitle <- htmlDiv(children = "How important is social distancing at times like these with COVID-19? Why do we need travel restrictions, stay-at-home orders, and closures at restaurants and events? Our app helps you visualize the differences in risk and effects of close social interaction at this time. 

Toggle through decisions like not attending large social events and see how these can drastically decrease the number of elderly and at-risk people indirectly harmed by you!
")


casesPlot <- dccGraph(
  figure = plotConfirmed,
  style = plotStyle
  # layout = plotStyle
)

deathPlot <- dccGraph(
  figure=list(
    data=list(
      list(
        x=c(1, 2, 3), y=c(4, 1, 2),
        # type='bar',
        type= 'scatter', mode = 'lines', name='something'
      ),
      list(
        x=c(1, 2, 3),
        y=c(2, 4, 8),
        # type='bar',
        type = 'scatter',
        mode = 'lines',
        name='something else'
      )
    ),
    layout = plotStyle
  )
)



app$layout(
  htmlDiv(
    list(
      deathCounts,
      appTitle,
      # regions.dropdown,
      casesPlot,
      deathPlot
    ),
    style = list(
      # backgroundColor = colors$background, 
      # color = colors$text,
                 textAlign = 'center'
                 )
  )
)


app$run_server()