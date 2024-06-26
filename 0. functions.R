## FUNCTIONS ## 


summarise_vars <- function(dat, by = NULL){
  by <- sym(by)
  main0 %>%
    group_by(!!by) %>% 
    summarise(n = n(),
              n_small = 100*sum(size == "S", na.rm = T)/n, 
              n_pws_gw = 100*sum(pws_type == "GW", na.rm = T)/n,
              n_pws_sw = 100*sum(pws_type == "SW", na.rm = T)/n,
              tot_pop_served = sum(pop_served, na.rm = T), 
              across(c(starts_with("perc_"), mdi_rate), 
                     list(median = median, 
                          mean = mean)))
}


use_t.test <- function(dat, by = NULL){
  by <- sym(by)
  temp <- dat %>%
    select(!!by, starts_with("perc_"), mdi_rate) %>%
    pivot_longer(cols = c(starts_with("perc_"), mdi_rate)) %>%
    mutate(outcome = colnames(.)[1]) %>%
    group_by(name, outcome) %>%
    nest() %>%
    mutate(t_test = map(data, ~ t.test(value ~ !!by, 
                                       data = .x, 
                                       var.eq = F, paired = F)),
           res = map(t_test, tidy)) %>%
    unnest(res) %>%
    ungroup() %>%
    select(-data, -t_test)
  return(temp)
}
