# libraries
library(tibble)
library(dplyr)
library(tidyr)

generate_parameters <- function(retail=1, office=9, residential=3, scenario=NULL){
  # check
  assertthat::assert_that(retail+office+residential == 13)
  
  # constant model parameters
  params <- tibble( # using tibble at first to mutate more easily
    purchase_price = 4200000,
    purchase_sf = 104920,
    purchase_floors = 15,
    purchase_floors_basement = 2,
    efficiency = 0.85,
    new_residential_efficiency = 0.93,
    new_retail_efficiency = 0.88,
    lease_office_sf = 25,
    lease_retail_sf = 23,
    lease_2br = 12*2400,
    lease_1br = 12*1700,
    lease_studio = 12*1300,
    oer_commercial = 0.19,
    expense_escalator = 0.03,
    loan_interest = 0.06,
    loan_term = 10,
    loan_amortize = 25,
    loan_dcr = 1.25,
    loan_ltv = 0.75,
    cap_purchase = 0.0725,
    cap_exit = 0.0850,
    discount_rate = 0.09,
    millage_city = 8.06,
    millage_county = 4.73,
    millage_school = 10.25,
    floors_retail = as.integer(retail),
    floors_office = as.integer(office),
    floors_residential = as.integer(residential),
    sf_per_floor = purchase_sf / purchase_floors,
    oer_residential = # begins near 40%, asymptotic to 20% as n->inf
      (4 + 2 * (floors_residential)) /
      (10 + 10 * (floors_residential)),
    sf_retail = floors_retail * new_retail_efficiency * sf_per_floor,
    sf_residential = floors_residential * new_residential_efficiency * sf_per_floor,
    sf_office = floors_office * efficiency * sf_per_floor
  ) %>%
    as.list() # for simplicity

  # supply parameters
  return(params)
}

generate_data <- function(n=50000){
  dat <- tibble(
    instance=1:n,
    construction_term=sample(
      2:5, 
      n, 
      replace=TRUE,
      prob=c(.9,.09,.009,.001)
    ),
    leaseup_office=sample(
      1:4, 
      n, 
      replace=TRUE,
      prob=c(0.05,0.8,0.2,0.05)
    ),
    leaseup_residential=sample(
      2:3, 
      n, 
      replace=TRUE,
      prob=c(0.8,0.2)
    ),
    leaseup_retail=sample(
      1:4,
      n,
      replace=TRUE,
      prob=c(0.15,0.7,0.10,0.05)
    ),
    vacancy_retail = 0.13 + rbeta(n,0.5,8),
    vacancy_office = 0.13 + rbeta(n,1.5,8),
    vacancy_residential = rnorm(n,0.07,0.03)
  )
  return(dat)
}

# not sure when assessment will happen 
# but this might be helpful
calculate_tax <- function(value, params) {
  city <- value * params$millage_city / 1000
  county <- value * params$millage_county / 1000
  school <- value * params$millage_school / 1000
  tax <- city + county + school
  return(tax)
}

# this will help with lease up, assuming linear
calculate_vacancy <- function(year, construction, leaseup, stable) {
  # we finish *on* leaseup year
  step <- (1 - stable) / (leaseup)
  out <- case_when(
    year <= construction ~ 1.00, # start vacant
    year >= construction + leaseup ~ stable, # end stable
    TRUE ~ 1 - (step * (year - construction)) # step down equal increments
  )
  return(out)
}

# 
create_noi_table = function(instance, params) {
  
  income_table <- tibble(
    # year range
    year=1:10,
    
    # columns to drop
    .escalator=(params$expense_escalator + 1) ^ (year - 1),
    .construction_complete = ifelse(year<=instance[['construction_term']],TRUE,FALSE),
    
    # retail
    retail_pgi = params$sf_retail * params$lease_retail_sf * .escalator,
    retail_vacancy = calculate_vacancy(
      year,
      instance[['construction_term']],
      instance[['leaseup_retail']],
      instance[['vacancy_retail']]
    ),
    retail_egi = retail_pgi * (1 - retail_vacancy),
    
    # office
    office_pgi = params$sf_office * params$lease_office_sf * .escalator,
    office_vacancy = calculate_vacancy(
      year,
      instance[['construction_term']],
      instance[['leaseup_office']],
      instance[['vacancy_office']]
    ),
    office_egi = office_pgi * (1 - office_vacancy),
    
    # residential
    # THIS IS ALL WRONG LOL it needs vary by unit type :)
    residential_pgi = 100000,
      # params$sf_office * params$lease_office_sf * .escalator,
    residential_vacancy = calculate_vacancy(
      year,
      instance[['construction_term']],
      instance[['leaseup_residential']],
      instance[['vacancy_residential']]
    ),
    residential_egi = residential_pgi * (1 - residential_vacancy)
    
    ) %>%
    select(-starts_with('.') # remove internal columns
    
  )
  
  # supply table
  # change this when finalized with egi, noe, and noi
  return(income_table)
  
}

calc_all <- function(data, params) {
  # add noi tables
  data[['noi_tbl']] <- apply(
    data,
    1,
    create_noi_table,
    params=params
  )
  
  # etc
  
  # return data
  # change this to only be indicators of note if computationally expensive
  return(data)
}

# testing
test_params <- generate_parameters()
test_data <- generate_data(n=10)
out <- calc_all(test_data, test_params)
out