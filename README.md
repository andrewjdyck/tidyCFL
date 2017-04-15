# tidyCFL
A tidy data package for the Canadian Football League API


## API info
An API key is required and can be requested [here](http://api.cfl.ca/key-request).

Documentation for the CFL API can be found [here](http://api.cfl.ca/docs).

## Config
set the API key with tidyCFL.api_key('YOUR API KEY')

# Usage

```{r}
tidyCFL.api_key('testK3Y')
cfl_games(2016)
```
