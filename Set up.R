# Section 1: Environment ----

#### packages installation ####
# to manually install new packages, use the template code below:
# install.packages("insert_package_name")
# devtools::install_github("r-dbi/bigrquery")

#### libraries ####
# rmd
library(knitr)
library(kableExtra)
# data wrangling
library(zoo)
library(broom)
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyverse)
# data visualisation
library(grid)
library(ghibli)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(ggstance)
library(ggridges)
library(ggthemes)
library(flextable)
library(wesanderson)
# google BigQuery
library(DBI)
library(httr)
library(dbplyr)
library(tinytex)
library(devtools)
library(bigrquery)
# statistics
library(pwr)
library(statmod)
library(effsize)
library(effectsize)
# prevent Google servers issue
httr::set_config(httr::config(http_version = 0))

####  other function ####
# to force R round 0.5 to 1
round2 <- 
  # x: the number to be rounded
  # n: digits
  function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(as.numeric(as.character(z)))
    z = z/10^n
    (z)*posneg
  }

#### session info ####
# sessioninfo::package_info()

#### BigQuery authentication ####
# authentication info
projectid = "########"
co_email = "#########"

# authentication in browser
bq_auth(
  email = co_email,
  path = NULL,
  scopes = c(
    "https://www.googleapis.com/auth/bigquery",
    "https://www.googleapis.com/auth/cloud-platform"
  ),
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

connection <-
  dbConnect(
    bigrquery::bigquery(),
    project = projectid,
    dataset = "abtest",
    billing = projectid,
    use_legacy_sql = FALSE
  )

# access tables
assignment <-
  tbl(connection, "assignment")

activity <-
  tbl(connection, "activity")

# Section 2: Data Pre-processing ----

#### assignment table ####
# read the complete assignment table into R
assignment_table <-
  assignment %>%
  collect()

# format date variables
assignment_table %<>%
  mutate_at(c("assignment_date", "install_date", "conversion_date"),
            as.Date)

# assignment_table %>%
#   summary()
#> playerid           abtest_group       assignment_date      install_date         conversion_date     
#> Min.   :      16   Length:10331056    Min.   :2017-05-04   Min.   :2016-01-01   Min.   :2016-01-01  
#> 1st Qu.:24682994   Class :character   1st Qu.:2017-05-04   1st Qu.:2016-09-03   1st Qu.:2016-08-25  
#> Median :39318044   Mode  :character   Median :2017-05-04   Median :2017-01-27   Median :2017-01-16  
#> Mean   :34980378                      Mean   :2017-05-05   Mean   :2016-12-13   Mean   :2016-12-06  
#> 3rd Qu.:47935775                      3rd Qu.:2017-05-04   3rd Qu.:2017-04-21   3rd Qu.:2017-04-13  
#> Max.   :51145423                      Max.   :2017-05-22   Max.   :2017-05-22   Max.   :2017-05-22  
#>                                                                                 NA's   :10045133    

assignment_table %<>%
  mutate(new_return =
          (install_date<as.Date("2017-05-04")) %>% 
           as.factor() %>% 
                      # Returning player: install before 2017-05-04
           fct_recode(`Returning Player`="TRUE",
                      # New player: install after or on 2017-05-04          
                      `New Player`="FALSE")) %>% 
  mutate(conversion_in_experiment =
           (conversion_date>as.Date("2017-05-03")) %>%
           as.factor() %>%
                      # converted after or on 2017-05-04
           fct_recode(yes = "TRUE",
                      # converted before 2017-05-04  
                      `converted before` = "FALSE") %>% 
           # never converted
           fct_explicit_na("no")) %>% 
  mutate(player_type = 
                     # on the basis of new/returning player definitions above:
                     # Returning payer: made at least one purchase before 2017-05-04
           case_when((!is.na(conversion_date) & conversion_date <as.Date("2017-05-04") & install_date<as.Date("2017-05-04")) ~ "Returning Payer",
                     # New payer: made at least one purchase by end of 2017-05-22
                     (!is.na(conversion_date) & install_date>as.Date("2017-05-03")) ~ "New Payer",
                     # Returning non-payer: made no purchase before 2017-05-04
                     (!is.na(conversion_date) & conversion_date >as.Date("2017-05-03") & install_date<as.Date("2017-05-04")) ~ "Returning Non-payer",
                     (is.na(conversion_date) & install_date<as.Date("2017-05-04")) ~ "Returning Non-payer",
                     # New payer: made no purchase by end of 2017-05-22
                     (is.na(conversion_date) & install_date>as.Date("2017-05-03")) ~ "New Non-payer") 
         )

#### activity table ####

# number of distinct player: more players than in assignment table
# need to filter out those who were not assigned to the experiment
activity_distinct_player_id <-
  activity %>%
  summarize(n = n_distinct(playerid)) %>%
  collect()
# activity_distinct_player_id$n - n_distinct(assignment_table$playerid)

activity_distinct_player_id <-
  activity %>%
  summarize(n = n_distinct(playerid)) %>%
  collect()

activity <- 
  activity %>%
  group_by(playerid) %>% 
  select(playerid,activity_date) %>% 
  collect()

####  common objects ####

# sample size
nA =
  assignment_table %>% 
  filter(abtest_group=="A") %$% 
  n_distinct(playerid)

nB =
  assignment_table %>% 
  filter(abtest_group=="B") %$% 
  n_distinct(playerid)

# segmentation sample size
## player type in group A
nA_newpayer =
  assignment_table %>% 
  filter(abtest_group=="A"& player_type=="New Payer") %$% 
  n_distinct(playerid)

nA_returnpayer =
  assignment_table %>% 
  filter(abtest_group=="A" & player_type=="Returning Payer") %$% 
  n_distinct(playerid)
  
nA_newnonpayer =
  assignment_table %>% 
  filter(abtest_group=="A"& player_type=="New Non-payer") %$% 
  n_distinct(playerid)

nA_returnnonpayer =
  assignment_table %>% 
  filter(abtest_group=="A" & player_type=="Returning Non-payer") %$% 
  n_distinct(playerid)

## player type in group B
nB_newpayer =
  assignment_table %>% 
  filter(abtest_group=="B"& player_type=="New Payer") %$% 
  n_distinct(playerid)

nB_returnpayer =
  assignment_table %>% 
  filter(abtest_group=="B" & player_type=="Returning Payer") %$% 
  n_distinct(playerid)

nB_newnonpayer =
  assignment_table %>% 
  filter(abtest_group=="B"& player_type=="New Non-payer") %$% 
  n_distinct(playerid)

nB_returnnonpayer =
  assignment_table %>% 
  filter(abtest_group=="B" & player_type=="Returning Non-payer") %$% 
  n_distinct(playerid)