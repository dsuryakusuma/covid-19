# library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()


thing1 <- list(
    dccInput(id='my-id', value='initial value', type='text'),
    htmlDiv(id='my-div'))

thing2 <- list(
  dccDropdown(
    options=list(
      list(label="New York City", value="NYC"),
      list(label="Montréal", value="MTL"),
      list(label="San Francisco", value="SF")
    ),
    value="MTL",
    id="my-dropdown"
  ),
  htmlDiv(id="output-container")
)

sliderthing <- list(
  dccSlider(
    id = 'my-slider',
    min = -20,
    max = 20, 
    step = 1,
    value = 5
  ),
  htmlDiv(id = 'slider-output-container')
)


app$layout(
  htmlDiv( thing1,
    # put this htmlDiv and dccInput together in a list 
      # INSIDE another htmlDiv
      # providing children to this would be overwritten by the callback anyway
    ),
  htmlDiv( thing2),
  htmlDiv( sliderthing)
)



# text box
app$callback(
  output = list(id='my-div', property='children'),
  params = list(input(id='my-id',
                      property='value')),
  function(input_value) {
    sprintf("textbox value is \"%s\"", input_value)
  })

# dropdown
app$callback(
  output('output-container', 'children'),
             params = list(input('my-dropdown', 'value')),
             function(dropdown_value) {
               sprintf("dropdown value is \"%s\"", dropdown_value)
             })

#slider callback


app$callback(
  output(id = 'slider-output-container', property = 'children'),
  params=list(input(id = 'my-slider', property = 'value')),
  function(value) {
    sprintf("slider value is %0.1f", value)
  })



app$run_server()