# install.packages("usdata")
#
# library(usdata)
# library(dplyr)
#
#
# counties <- county_2019 %>%
#   select(state, name, fips, pop) %>%
#   filter(state %in% c("New York", "Connecticut", "New Jersey"))
#   #filter(state %in% c("California"))
#   #filter(state %in% c("Washington", "Oregon"))
#   group_by(state) %>%
#   summarise(n = n(),
#             pop = sum(pop))
#
#
# # I might be able to get data from tidycensus or from another dataset.
# library(tidycensus)
#
