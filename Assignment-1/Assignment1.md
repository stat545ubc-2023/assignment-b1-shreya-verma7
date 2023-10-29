Assignment 1
================
Shreya
2023-10-29

## Task : Function to Summarize By Group Mean

Initial loading of the required packages needed for the assignment.

``` r
# Load required libraries
suppressPackageStartupMessages(library(devtools))       # Used for easier R function aiding with package development
suppressPackageStartupMessages(library(testthat))       # Used for testing function
suppressPackageStartupMessages(library(dplyr))          # Used for easier data manipulation
suppressPackageStartupMessages(library(tidyverse))      # Used for efficient data science workflow implementation
suppressPackageStartupMessages(library(palmerpenguins)) # Used for sample data to showcase examples
```

## Exercise 1 & 2: Make and Document Function

This section includes the creation, documentation, and fortification of
a non-trivial function. Documentation is carried out using ‘roxygen2’
tags.

``` r
#' @title: Summarize by Group Mean
#'
#' @description The function automates the calculation of arithmetic mean operating on and summarizing by the grouping chosen by the user
#' 
#' @details This is a generic function that can group by one or multiple levels passed by the user
#'          User can pass provide argument for grouping either by numerical index or column name because both ways are used in practice 
#'          Rows with N/A values are kept as it can still be valid grouping combinations. Thus N/A values are treated via omission in the calculation of mean
#' 
#' @param dataFrame dataframe_like_object input only accepts dataframe or tibbular data & hence the chosen name to indicate the inclusion of both
#' @param group_by vector should be non-empty, can either be numerical or string thus the chosen name is kept generic
#' 
#' @return tibble for the case where legal arguments are passed returning values of calculated mean for numeric columns not used in grouping
#' @return null for the case where invalid arguments are passed to function along with error message   

options(dplyr.summarise.inform = FALSE) # Suppressing default message using global option

Group_mean_summarization = function(dataFrame, group_by){
    if (! (is.vector(group_by) && length(group_by) > 0)){
      stop('Group by column parameter is invalid')
    }
    
    # if numeric col_ind passed check they don't have matching names in the tibble
    # then convert to col_ind to col_names of the tibble
    if (is.numeric(group_by) && sum(group_by %in% names(dataFrame))==0 ){
      group_by = names(dataFrame)[group_by]
          if (sum(is.na(group_by)) > 0 || identical(group_by, character(0)) ) { # Ensuring no column index is out of bounds
              stop('Column indices not valid')
          }
    } 
    
    else if (sum(group_by %in% names(dataFrame))==length(group_by)){   # if reached here then ensuring all input vector elements are legal column names
        invisible()
    }
  
    else {
        stop('Column name(s) not valid')
    }
  
  # now grouping the data, summarize by mean while omitting N/A values, and return
  dataFrame %>% 
      group_by_at(group_by) %>%
          summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
              return
}
```

###### Note: It is on purpose that the default column index was not used for grouping to narrow the scope of the function as opposed to widening it

## Exercise 3: Function Examples

This section delineates the working of the function through examples
using the ‘penguins’ data set from the ‘palmerpenguins’ library.

###### Example 1: Grouping on a single (valid) column using column name

``` r
# Quick glance at the penguins data set
head(penguins)
```

    ## # A tibble: 6 × 8
    ##   species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ## 1 Adelie  Torgersen           39.1          18.7               181        3750
    ## 2 Adelie  Torgersen           39.5          17.4               186        3800
    ## 3 Adelie  Torgersen           40.3          18                 195        3250
    ## 4 Adelie  Torgersen           NA            NA                  NA          NA
    ## 5 Adelie  Torgersen           36.7          19.3               193        3450
    ## 6 Adelie  Torgersen           39.3          20.6               190        3650
    ## # ℹ 2 more variables: sex <fct>, year <int>

``` r
# Function output when grouped on the column 'island'
Group_mean_summarization(penguins, "island")
```

    ## # A tibble: 3 × 6
    ##   island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g  year
    ##   <fct>              <dbl>         <dbl>             <dbl>       <dbl> <dbl>
    ## 1 Biscoe              45.3          15.9              210.       4716. 2008.
    ## 2 Dream               44.2          18.3              193.       3713. 2008.
    ## 3 Torgersen           39.0          18.4              191.       3706. 2008.

###### Example 2: Grouping on multiple (valid) columns using column names

``` r
# Function output when grouped on the columns 'species' and 'island'
Group_mean_summarization(penguins, c("species", "island"))
```

    ## # A tibble: 5 × 7
    ## # Groups:   species [3]
    ##   species   island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>     <fct>              <dbl>         <dbl>             <dbl>       <dbl>
    ## 1 Adelie    Biscoe              39.0          18.4              189.       3710.
    ## 2 Adelie    Dream               38.5          18.3              190.       3688.
    ## 3 Adelie    Torgersen           39.0          18.4              191.       3706.
    ## 4 Chinstrap Dream               48.8          18.4              196.       3733.
    ## 5 Gentoo    Biscoe              47.5          15.0              217.       5076.
    ## # ℹ 1 more variable: year <dbl>

###### Example 3: Grouping on multiple (valid) columns using column index

``` r
# Function output when grouped on the columns 1 and 2 that correspond to 'species' and 'island'
Group_mean_summarization(penguins, c(1,2))
```

    ## # A tibble: 5 × 7
    ## # Groups:   species [3]
    ##   species   island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##   <fct>     <fct>              <dbl>         <dbl>             <dbl>       <dbl>
    ## 1 Adelie    Biscoe              39.0          18.4              189.       3710.
    ## 2 Adelie    Dream               38.5          18.3              190.       3688.
    ## 3 Adelie    Torgersen           39.0          18.4              191.       3706.
    ## 4 Chinstrap Dream               48.8          18.4              196.       3733.
    ## 5 Gentoo    Biscoe              47.5          15.0              217.       5076.
    ## # ℹ 1 more variable: year <dbl>

###### Note: As expected the results from example 2 and 3 are equal

## Exercise 4: Function Testing

In this section we conduct formal checks on the function using five
non-redundant checks that are appropriate for the function design
requirements. In order to make testing more robust checks are conducted
for both the negative (test# 1 to 3: expect_error) and positive (test# 4
to 5: expect_equivalent) outcomes.

###### Test 1: Checking for valid argument type

``` r
test_that("Invalid data type error", {
    expect_error(Group_mean_summarization(list(1,2),c(1)))
    })
```

    ## Test passed 🎉

###### Test 2: Checking for valid argument range

``` r
test_that("Invalid group by index error", {
    expect_error(Group_mean_summarization(tibble(1,2),c(0)))
    })
```

    ## Test passed 🎉

###### Test 3: Checking for non-empty arguments

``` r
test_that("Empty group by index error", {
    expect_error(Group_mean_summarization(tibble(1,2),c()))
    })
```

    ## Test passed 🥳

###### Test 4: Checking for computational correctness

``` r
test_that("Expect the mean to equal 3 for all data vales = 3 grouped by first index", {
    expect_equivalent(Group_mean_summarization(tibble(c(1,1),3),c(1)),tibble(c(1),c(3)))
    })
```

    ## Test passed 🥇

###### Test 5: Checking for proper handling of N/A values

``` r
test_that("Test mean removes NA values in summarized column ", {
    expect_equivalent(Group_mean_summarization(tibble(c(1,1,2),c(3,NA,5)),c(1)),tibble(c(1,2),c(3,5)))
    })
```

    ## Test passed 😸
