# CDC_R_shiny

Deployed [here](https://mitkotak.shinyapps.io/CDC_R_shiny/)

# Functionality

- [Input insecticides and species from the user](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L72-L100)

- [Read in user input data](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L164-L176)

- [Validate the data and print out a message if mortality is too high](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L221-L240).

- [Run Abott's formula and create the plots](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L242-L293)

- [Print out resistant state](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L310-L328)

- [Prints out the recommended number of mosquitos based on hard coded e proportion in sample.size.prop function](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L331-L343)


- [Export screenshot](https://github.com/mitkotak/CDC_R_shiny/blob/11436f9a9fd7d0e3aa0b55aee0fa0355389f58db/app.R#L345-L347)
# TO DO LIST

- [ ] Resistant state doesn't print out the expected message for all of the different data sets in `data` folder.

- [ ] Remove default user input data matrix. Reading in data similar to `data/BottleBioassayEx.csv` right now.

- [ ] Recommendation function crashes for some of the insecticide-pesticide choices.

- [ ] Screenshot needs to returns a pdf instead of png.

# Deploy Pipeline

- `./github/workflows/main.yml` builds the `Dockerfile`.
- `Dockerfile` downloads all packages and calls `app.R` and `deploy.R`.
- `app.R` contains all of the shiny application code. The website can be tested locally but clicking `Run app` on `app.R`.
- `deploy.R` deploys the app using Github pages API.


