# Function to convert yield to kg/ha
convert_yield_to_kgha <- function(yield_amount, yield_unit) {
  # Conversion factors
  kg_to_lb <- 0.453592
  ha_to_acre <- 0.404686
  lb_to_kg <- 1 / kg_to_lb
  acre_to_ha <- 1 / ha_to_acre
  
  # Convert the input yield to kg/ha
  yield_kgha <- switch(tolower(yield_unit),
                       "kg/ha" = yield_amount,
                       "kg/acre" = yield_amount * acre_to_ha,
                       "lb/acre" = yield_amount * lb_to_kg * acre_to_ha,
                       "lb/ha" = yield_amount * lb_to_kg,
                       "g/ha" = yield_amount / 1000,
                       "g/acre" = (yield_amount / 1000) * acre_to_ha,
                       "ton/ha" = yield_amount * 1000,
                       "ton/acre" = yield_amount * 1000 * acre_to_ha,
                       "t/ha" = yield_amount * 1000,
                       "t/acre" = yield_amount * 1000 * acre_to_ha,
                       stop("Invalid yield unit provided.")
  )
  
  return(yield_kgha)
}