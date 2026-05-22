calculate_resource_constraints <- function(truth_data){
  require(dplyr)
  K_constraints <- truth_data %>% 
    group_by(target_end_date) %>%
    summarise(value = sum(value)) %>%
    dplyr::filter(target_end_date >= as.Date("2021-12-18")) %>%       # might want to make this changeable in _targets.R later
    dplyr::filter(target_end_date <= as.Date("2022-03-12")) %>% 
    mutate(max = value*0.99,
           min = value*0.81,
           mid = value*0.90)
}

  
make_resource_constraint_grid <- function(K_constraints){
klist <- list()                                                             # create an empty list

# Calculate n values between min and max
for (i in 1:nrow(K_constraints)) {
  vec <- numeric(5)                                                         # for this first test, we're only doing 5 columns (min, max, 3 in between)
  max <- K_constraints$max[i]
  min <- K_constraints$min[i]
  vec <- seq(from = min, to = max, by=(max-min)/4)                          # slice into the 5 columns
  klist[[i]] <- vec                                                         # put all vectors in the list
}

df <- do.call("rbind",klist)                                                # combine all vectors into a matrix
K_constraint_grid <- cbind(K_constraints, df) %>% select(-max, -min, -mid)  # bind Ks to date list (I checked - this stays in the correct order so max, min, mid correct)
return(K_constraint_grid)
}



# # Constraints visualizer - change numbers to get max and midpoint
# ggplot(data=K_constraint_grid, aes(x=target_end_date, y=value)) + 
#   geom_point(color="black") + 
#   geom_line(color="black") + 
#   labs(title = "Allocation Score: Weekly 90% Resource Constraints", 
#        subtitle = "Omicron Wave in California",
#        x = "MMWR Week End Date", y ="Weekly Incident Cases") + 
#   theme_bw() + geom_line(aes(y=`1`), color="grey80") + geom_line(aes(y=`3`), color="grey60") + geom_line(aes(y=`5`), color="grey80")
