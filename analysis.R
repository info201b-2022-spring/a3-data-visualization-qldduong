# Quang Lucas Duong
# A3 - Data Visualization (Incarceration)
# Section BD - TA: Darren Gunadharma
# 5/12/2022

#-----------------------------------



incarc_trends_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
# incarc_trends_data <- read.csv("C:/Users/tduon/a3-data-visualization-qldduong/incarceration_trends.csv")


# Variables of choice:
incarc_trends_2018 <- filter(incarc_trends_data, year == 2018)

# Region w/ highest Black incarceration proportion (avg over time)

black_incarc_all <- select(incarc_trends_data, year, state, region, county_name, black_pop_15to64, black_prison_pop)
black_incarc_clean <- na.omit(black_incarc_all)
black_incarc_clean <- mutate(black_incarc_clean, black_incarc_prop = black_prison_pop / black_pop_15to64)

grouped_black_incarc <- group_by(black_incarc_clean, region) 
clean_grouped_black_incarc <- filter(grouped_black_incarc, black_pop_15to64 != 0)
avg_black_prop <- summarize(clean_grouped_black_incarc, avg_props = mean(black_incarc_prop, na.rm = TRUE))

black_prop_max <- max(avg_black_prop$avg_props)

max_prop_row <- filter(avg_black_prop, avg_props == black_prop_max)

region_highest_black_jail_rate <- max_prop_row$region # SOL 1


# BIA is the most active in which state (in 2018)?   ## JAIL, NOT PRISON
incarc_trends_bia <- select(incarc_trends_2018, year, state, county_name, total_jail_from_bia)
clean_incarc_bia <- na.omit(incarc_trends_bia)
grouped_bia_2018 <- group_by(clean_incarc_bia, state)

sum_bia_jail_state <- summarize(grouped_bia_2018, state_sum = sum(total_jail_from_bia))
bia_max_sum <- max(sum_bia_jail_state$state_sum)
max_bia_row <- filter(sum_bia_jail_state, state_sum == bia_max_sum)
state_highest_bia <- max_bia_row$state # SOL 2

# State with highest avg Black prison pop rate
incarc_trends_black_prison_rate <- select(incarc_trends_data, year, state, county_name, black_prison_pop_rate)
clean_incarc_black_rate <- na.omit(incarc_trends_black_prison_rate)
grouped_black_trends_state <- group_by(clean_incarc_black_rate, state)

avg_black_prison_rate <- summarize(grouped_black_trends_state, black_prison_rate_avg = mean(black_prison_pop_rate))

black_rate_max_avg <- max(avg_black_prison_rate$black_prison_rate_avg)
black_max_rate_row <- filter(avg_black_prison_rate, black_prison_rate_avg == black_rate_max_avg)
state_highest_black_rate_avg <- black_max_rate_row$state # SOL 3


# County with highest latinx prison prop over time (average)


latinx_incarc_data <- select(incarc_trends_data, year, state, county_name, latinx_prison_pop, total_prison_pop)
clean_latinx_data <- na.omit(latinx_incarc_data)
clean_latinx_data <- mutate(clean_latinx_data, latinx_proportion = latinx_prison_pop / total_prison_pop)
clean_latinx_data <- filter(clean_latinx_data, total_prison_pop != 0)
clean_latinx_data <- mutate(clean_latinx_data, location = paste0(county_name, ", ", state)) # Adding a "location" column

grouped_latinx_data <- group_by(clean_latinx_data, location)
latinx_avg_table <- summarize(grouped_latinx_data, avg_latinx_prison_prop = mean(latinx_proportion))
latinx_county_max <- max(latinx_avg_table$avg_latinx_prison_prop)
latinx_county_max_row <- filter(latinx_avg_table, avg_latinx_prison_prop == latinx_county_max)
county_highest_latinx_prop <- latinx_county_max_row$location # SOL 4
# Oh my god, there were times and places where ONLY latinx people were in prison


# In which urbanicity do aapi prison admission make up the largest proportion of total prison admissions?

incarc_trends_aapi <- select(incarc_trends_data, year, state, region, county_name, urbanicity, total_prison_adm, aapi_prison_adm)
incarc_trends_aapi_2015 <- filter(incarc_trends_aapi, year == 2015)
aapi_2015_clean <- na.omit(incarc_trends_aapi_2015)
aapi_2015_cleaner <- filter(aapi_2015_clean, total_prison_adm != 0)

aapi_2015_prop_table <- mutate(aapi_2015_cleaner, aapi_prison_adm_prop = aapi_prison_adm / total_prison_adm)
grouped_aapi_table <- group_by(aapi_2015_prop_table, urbanicity)
aapi_avg_table <- summarize(grouped_aapi_table, avg_aapi_prison_adm = mean(aapi_prison_adm_prop))

aapi_avg_max <- max(aapi_avg_table$avg_aapi_prison_adm)
aapi_max_row <- filter(aapi_avg_table, avg_aapi_prison_adm == aapi_avg_max)
aapi_urban_max <- aapi_max_row$urbanicity # SOL 5


# Comparable over time: Proportion of nationwide black_prison_pop / total_prison_pop

# Finds the proportion of (black_prison_pop / total_prison_pop) in a given year
black_prison_prop_func <- function(given_year) {
  black_pop_table <- select(incarc_trends_data, year, state, black_prison_pop, total_prison_pop) 
  year_black_pops <- filter(black_pop_table, year == given_year)
  clean_year_pops <- na.omit(year_black_pops)
  black_yearly_prison_sum <- sum(clean_year_pops$black_prison_pop)
  total_yearly_prison_sum <- sum(clean_year_pops$total_prison_pop)
  end_sum_proportion <- black_yearly_prison_sum / total_yearly_prison_sum 
  rounded_result <- round(end_sum_proportion, digits = 4)
  # result <- paste0(rounded_result, "%")
  return(rounded_result)
}

# Black prison proportions, 2005-2015
black_prison_prop_vector <- c(black_prison_prop_func(2005),black_prison_prop_func(2006),
                                 black_prison_prop_func(2007), black_prison_prop_func(2008),
                                 black_prison_prop_func(2009), black_prison_prop_func(2010),
                                 black_prison_prop_func(2011), black_prison_prop_func(2012),
                                 black_prison_prop_func(2013), black_prison_prop_func(2014),
                                 black_prison_prop_func(2015))
years_vector <- (year = c(2005:2015))

# Visualizing the national Black prison proportion over time (2005 - 2015)
# Learned from: [Connected scatterplot with R and ggplot2](https://r-graph-gallery.com/connected_scatterplot_ggplot2.html)
prop_column <- matrix(black_prison_prop_vector)
year_column <- matrix(years_vector)
black_prop_df <- data.frame(year = year_column, proportion = prop_column)


# Plot
black_prop_graph <- black_prop_df %>%
  tail(11) %>%
  ggplot(aes(x=year, y=proportion, label = year)) +
  geom_line(aes(color = "red"))  + 
  geom_point() + ggtitle("Proportion of US Prison Population that is Black (2005 - 2015)") +
  xlab("Year") + ylab("Proportion (in decimal format)") + geom_text(vjust=2) +
  theme(plot.title = element_text(size=16))

############################################################################


# Finds the proportion of (black_pop_15to64 / total_pop_15to64) in a given year
black_total_pop_func <- function(given_year) {
  black_pop_df <- select(incarc_trends_data, year, state, black_pop_15to64, total_pop_15to64) 
  year_black_pops <- filter(black_pop_df, year == given_year)
  clean_year_pops <- na.omit(year_black_pops)
  black_yearly_pop_sum <- sum(clean_year_pops$black_pop_15to64)
  total_yearly_pop_sum <- sum(clean_year_pops$total_pop_15to64)
  end_sum_proportion <- black_yearly_pop_sum / total_yearly_pop_sum 
  rounded_result <- round(end_sum_proportion, digits = 4)
  return(rounded_result)
}

black_total_pop_vector <- c(black_total_pop_func(2005),black_total_pop_func(2006),
                             black_total_pop_func(2007), black_total_pop_func(2008),
                             black_total_pop_func(2009), black_total_pop_func(2010),
                             black_total_pop_func(2011), black_total_pop_func(2012),
                             black_total_pop_func(2013), black_total_pop_func(2014),
                             black_total_pop_func(2015))

pop_column <- matrix(black_total_pop_vector)
black_prop_df <- data.frame(year = year_column, proportion = prop_column)
new_pop_df <- mutate(black_prop_df, pop_proportion = pop_column)



# ggplot(economics, aes(x=date)) + 
#  geom_line(aes(y = psavert), color = "darkred") + 
#  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash") 


# Learned from: [HOW TO CREATE A GGPLOT WITH MULTIPLE LINES]("https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/")

new_pop_graph <- new_pop_df %>%
  tail(11) %>%
  ggplot(aes(x = year, color = group)) + 
  geom_line(aes(y = proportion, color = "darkred", size=0.25)) + 
  geom_line(aes(y = pop_proportion, color="steelblue", size=0.25)) +
  ggtitle("Proportion of US Total/Prison Population that is Black") +
  xlab("Year") + ylab("Proportion (in decimal format)") + 
  scale_color_manual(labels = c("Proportion of Prison Population", "Proportion of Total Population")
                     , values = c("darkred", "steelblue")) +
  theme(legend.key.size = unit(2, 'cm'), axis.title = element_text(size=12)
        , axis.text = element_text(size=11), plot.title = element_text(size=16))


# -------------------------------------------------------------
# Vis 3: Choropleth map, showcases the proportion of Black incarceration in each state 
# Specifically, in the year 2015
# Only 36 / 50 states provided prison population data without NA values


sep_black_pop_table <- select(incarc_trends_data, year, state, black_prison_pop, total_prison_pop)
sep_black_pop_table <- filter(sep_black_pop_table, year == 2015)
clean_black_pop_table <- na.omit(sep_black_pop_table)

grouped_black_pop_table <- group_by(clean_black_pop_table, state)
sum_black_states_table <- summarize(grouped_black_pop_table, black_prison_pop = sum(black_prison_pop), total_prison_pop = sum(total_prison_pop))
end_black_pop_table <- mutate(sum_black_states_table, proportion = black_prison_pop / total_prison_pop) # set the df up

#------------------------------------------------------
new_black_pop_table <- end_black_pop_table
# I DON'T KNOW HOW ELSE TO DO THIS

new_black_pop_table[1, 1] = "alabama"
new_black_pop_table[2, 1] = "arizona"
new_black_pop_table[3, 1] = "california"
new_black_pop_table[4, 1] = "colorado"
new_black_pop_table[5, 1] = "florida"
new_black_pop_table[6, 1] = "georgia"
new_black_pop_table[7, 1] = "hawaii"
new_black_pop_table[8, 1] = "iowa"
new_black_pop_table[9, 1] = "indiana"
new_black_pop_table[10, 1] = "kentucky"
new_black_pop_table[11, 1] = "massachusetts"
new_black_pop_table[12, 1] = "maryland"
new_black_pop_table[13, 1] = "maine"
new_black_pop_table[14, 1] = "michigan"
new_black_pop_table[15, 1] = "minnesota"
new_black_pop_table[16, 1] = "missouri"
new_black_pop_table[17, 1] = "mississippi"
new_black_pop_table[18, 1] = "north carolina"
new_black_pop_table[19, 1] = "north dakota"
new_black_pop_table[20, 1] = "nebraska"
new_black_pop_table[21, 1] = "new hamphshire"
new_black_pop_table[22, 1] = "new jersey"
new_black_pop_table[23, 1] = "nevada"
new_black_pop_table[24, 1] = "new york"
new_black_pop_table[25, 1] = "ohio"
new_black_pop_table[26, 1] = "oklahoma"
new_black_pop_table[27, 1] = "pennsylvania"
new_black_pop_table[28, 1] = "rhode island"
new_black_pop_table[29, 1] = "south carolina"
new_black_pop_table[30, 1] = "south dakota"
new_black_pop_table[31, 1] = "tennessee"
new_black_pop_table[32, 1] = "texas"
new_black_pop_table[33, 1] = "utah"
new_black_pop_table[34, 1] = "washington"
new_black_pop_table[35, 1] = "wisconsin"
new_black_pop_table[36, 1] = "wyoming"
# ----------------------------------------------
# Learned from: Class textbook, chapter 16

# Join eviction data to the U.S. shapefile
state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(new_black_pop_table, by="state") # join eviction data

map_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(),      # remove border around plot
    plot.title = element_text(hjust = 0.5)
  )

# Draw the map setting the `fill` of each state using its eviction rate
map_vis <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = proportion),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) + ggtitle("% of Prison Population that is Black in Each State") +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#ff6f08", high = "#310c3d") +
  labs(fill = "Percentage (as decimals)") +
  map_theme # variable containing map styles (defined in next code snippet)


