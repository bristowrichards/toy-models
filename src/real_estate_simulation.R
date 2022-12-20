# libraries
library(tibble)
library(dplyr)

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
      0:3, 
      n, 
      replace=TRUE,
      prob=c(0.05,0.8,0.2,0.05)
    ),
    leaseup_residential=sample(
      0:2, 
      n, 
      replace=TRUE,
      prob=c(0.6,0.3,0.01)
    ),
    leaseup_retail=sample(
      0:4,
      n,
      replace=TRUE,
      prob=c(0.13,0.7,0.14,0.01,0.01)
    ),
    vacancy_retail = 0.15 + rbeta(n,0.5,8),
    vacancy_office = 0.15 + rbeta(n,1.5,8),
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
calculate_vacancy <- function(year, start, fillout, stable) {
  out <- case_when(
    year < start ~ 1.00,
    year > start + fillout ~ stable,
    TRUE ~ NA_real_
  )
  return(out)
}

create_egi_table = function(instance, params) {
  
  income_table <- tibble(
    year=1:10,
    escalator=(params$expense_escalator + 1) ^ (year - 1),
    .construction_complete = ifelse(year<=instance[['construction_term']],TRUE,FALSE),
    retail_pgi = params$sf_retail * params$lease_retail_sf * escalator,
    retail_vacancy = 1
    ) %>%
    select(-starts_with('.') # remove internal columns
    
  )
  
  # supply table
  return(income_table)
  
}

asdf <- generate_parameters()
jkl <- generate_data(n=1)
asdf
jkl
apply(jkl,1,create_egi_table,params=asdf)
calculate_vacancy(0,1,1,.4)
