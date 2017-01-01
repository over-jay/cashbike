# dplyr training
install.packages("dplyr")
# source http://www.datacarpentry.org/R-ecology-lesson/04-dplyr.html

library("dplyr")    ## load the package
install.packages("ggplot2")

data(survey)
select(surveys, plot_id, species_id, weight)

filter(surveys, year == 1995)

surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight)

surveys_sml
