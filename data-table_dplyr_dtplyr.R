# Efficient R Code: data.table vs. dplyr vs. dtplyr

# Source: https://iyarlin.github.io/2020/05/26/dtplyr_benchmarks/
# Note: Post published shortly before dplyr 1.0 was released to CRAN
# dtplyr was already in new version: in ca. mid-2019 it was completely re-written
# Now uses lazy evaluation instead of eager evaluation -> much better data.table translations

# Code adapted to use microbenchmark instead of system.time()
# Only 2 out of 5 tests: No. 1 and No. 5
# Code adapted to compare all computation versions in one call, not in separate scripts
# Only one data simulation instead of data generation in each script

library(data.table)
library(dplyr)
library(dtplyr)
library(microbenchmark)
library(ggplot2)

# Data simulation

N <- 1e7
K <- 100
set.seed(1)

DT <- data.table(
  id1 = sample(sprintf("id%03d", 1:K), N, TRUE),   # large groups (char)
  id5 = sample(N / K, N, TRUE),   # small groups (int)
  v1 = sample(5, N, TRUE),   # int in range [1,5]
  v2 = sample(5, N, TRUE),   # int in range [1,5]
  v3 = sample(round(runif(100, max = 100), 4), N, TRUE) # numeric, e. g. 23.5749
)

DF <- as_tibble(DT)

# Short descriptives

DF %>%
  summarise(id1_unique = n_distinct(id1),
            id5_unique = n_distinct(id5),
            v1_unique = n_distinct(v1),
            v2_unique = n_distinct(v2),
            v3_unique = n_distinct(v3))

# data.table part

DT_q1 <- function() {
  DT[, sum(v1), keyby = id1]
}

DT_q5 <- function() {
  DT[, lapply(.SD, sum), keyby = id5, .SDcols = 3:5]
}


# dtplyr part

dtplyr_q1 <- function() {
  DF %>%
    lazy_dt() %>%
    group_by(id1) %>% 
    summarise(sum(v1)) %>% 
    as_tibble()
}

dtplyr_q5 <- function() {
  DF %>% 
    lazy_dt() %>% 
    group_by(id5) %>% 
    # summarise(across(v1:v3), sum) %>%   # does not work yet, need old version
    summarise_at(vars(v1:v3), sum) %>% 
    as_tibble()
}


# dt_dtplyr
# Difference to dtplyr part: Start out with data.table
# I. e. use DT instead of DF

dt_dtplyr_q1 <- function() {
  DT %>%
    lazy_dt() %>%
    group_by(id1) %>% 
    summarise(sum(v1)) %>% 
    as.data.table()
}

dt_dtplyr_q5 <- function() {
  DT %>% 
    lazy_dt() %>% 
    group_by(id5) %>% 
    # summarise(across(v1:v3), sum) %>%   # does not work yet, need old version
    summarise_at(vars(v1:v3), sum) %>% 
    as.data.table()
}


# dplyr

dplyr_q1 <- function() {
  DF %>% 
    group_by(id1) %>% 
    summarise(sum(v1)) %>% 
    as_tibble()
}

dplyr_q5 <- function() {
  DF %>% 
    group_by(id5) %>% 
    summarise_at(vars(v1:v3), sum) %>%
    # summarise(across(v1:v3), sum) %>%   # extremely slow on my machine!!
    as_tibble()
}

# Test functions
str(DT_q1())
str(dtplyr_q1())
str(dt_dtplyr_q1())
str(dplyr_q1())

str(DT_q5())
str(dtplyr_q5())
str(dt_dtplyr_q5())
str(dplyr_q5())


# Benchmarking

q1 <- microbenchmark(
  DT_q1(),
  dtplyr_q1(),
  dt_dtplyr_q1(),
  dplyr_q1(),
  times = 10
)

q5 <- microbenchmark(
  DT_q5(),
  dtplyr_q5(),
  dt_dtplyr_q5(),
  dplyr_q5(),
  times = 10
)

autoplot(q1)
autoplot(q5)

boxplot(q1)
boxplot(q5)

q1
q5
