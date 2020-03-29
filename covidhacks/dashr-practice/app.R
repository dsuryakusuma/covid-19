library(tidyverse)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)


df <- read.csv(file = "https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv", stringsAsFactor=FALSE, check.names=FALSE) %>% as_tibble()

continents <- unique(df$continent)
years <- unique(df$year)


#################################

app <- Dash$new()


graph.and.slider <- list(
  dccGraph(id = 'graph-with-slider'), # has 'figure' property
  dccSlider(
    id = 'year-slider',
    min = 0,
    max = length(years) - 1,
    marks = years,
    value = 0 # <------ input of app (adjusted by slider)
  )
)

###########  slider  #############

# dccSlider starts from 0;
app$layout(
  htmlDiv(
    graph.and.slider
  )
)

app$callback(
  output = list(id='graph-with-slider', property='figure'),  ## puts it back into the figure?
  params = list(input(id='year-slider', property='value')), ## input params
  
  function(selected_year_index) {
    
    which_year_is_selected <- which(df$year == years[selected_year_index + 1])
    
    # recall lapply(lst, fun) just returns a list of the function evaluated at the values of that list
    traces <- lapply(
      continents,
      function(cont) { # for each cont in continents
           which_continent_is_selected <- which(df$continent == cont)
           
           df_sub <- df[intersect(which_year_is_selected, which_continent_is_selected), ]
           
           with(df_sub,
                 list(
                   x = gdpPercap, y = lifeExp,
                   opacity=0.5, text = country, mode = 'markers',
                   marker = list(size = 15, line = list(width = 0.5, color = 'white')),
                   name = cont
                 )
           )
     }
    )
    
    return(list(
      data = traces,
      layout= list(
        xaxis = list(type = 'log', title = 'GDP Per Capita'),
        yaxis = list(title = 'Life Expectancy', range = c(20,90)),
        margin = list(l = 40, b = 40, t = 10, r = 10),
        legend = list(x = 0, y = 1),
        hovermode = 'closest'
      )
    ))
  }
)

app$run_server()