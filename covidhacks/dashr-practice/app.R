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


app$layout(
  htmlDiv( thing2,
           thing1
    # put this htmlDiv and dccInput together in a list 
      # INSIDE another htmlDiv
      # providing children to this would be overwritten by the callback anyway
    )
)


# dropdown
app$callback(output('output-container', 'children'),
             params = list(input('my-dropdown', 'value')),
             function(dropdown_value) {
               sprintf("You have selected \"%s\"", dropdown_value)
             })

# text box
app$callback(
  output = list(id='my-div', property='children'),
  params = list(input(id='my-id',
                      property='value')),
  function(input_value) {
    sprintf("You've entered \"%s\"", input_value)
  })

app$run_server()