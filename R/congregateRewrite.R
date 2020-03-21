congregateDataDates <- function(df) {
  # Check that this is all the same data
  if(length(unique(df$disease)) != 1) {
    stop("'disease' column has multiple entries, function can only be used on one disease")
  }
  pop_col_name <- names(df)[grepl("pop_", names(df))]
  names(df)[grepl("pop_", names(df))] <- "pop"
  
  # Identify first and last dates
  first_date = as.Date(min(df$date))
  last_date = as.Date(max(df$date))
  # Sequence of weekly dates
  dates_used = seq(first_date, last_date, by = "weeks")
  
  getCongregateDate <- function(d) {
    return (as.Date(dates_used[which.min(abs(dates_used - as.Date(d)))]))
  }
  df$congregate_date <- Reduce(c, purrr::map(df$date, getCongregateDate))
  
  # Determine all unique location-type combinations
  if ("province" %in% names(df)) {
    unique_locations <- unique(df[, c("province", "region", "value_type")])
  } else {
    unique_locations = unique(df[, c("region", "value_type")])
  }
  
  getCongregatedValue <- function(df, date, start_date, type, reg, prov = NA) {
    subset <- df %>%
      dplyr::filter(date >= start_date & date <= date) %>%
      dplyr::filter(value_type == type) %>%
      dplyr::filter(region == reg)
    if (!is.na(prov)) {
      subset <- subset %>%
        dplyr::filter(province == prov)
    }
    if (nrow(subset) < 1) {
      return (0)
    }
    max_value <- max(subset$value)
    result <- c("congregate_date" = date, "value_type" = type, "region" = reg, "value" = max_value)
    if (!is.na(prov)) {
      result <- c(result, "province" = prov)
    }
    return (result)
  }
  
  purrr::pmap_dfr(list(dates_used, 
                       unique_locations$value_type,
                       unique_locations$region,
                       unique_locations$province), getCongregatedValue,
                  start_date = first_date, df = df)
  
  getCongregatedValue()
  
  
  # Create data frame which has a row for each unique province-region-dates_used combination
  new_df = unique_locations %>%
    dplyr::mutate(disease = df$disease[1]) %>%
    dplyr::slice(rep(1:(dplyr::n()), each = length(dates_used))) %>%
    dplyr::mutate(date = as.Date(rep(dates_used, times = nrow(unique_locations))))
  
  
  
  
  
  
  
  #------------------------------------------------------------------
  small = function(r, df, new_df) {
    if(length(df$province) > 0){
      prov = new_df[r,"province"][[1]]
    }
    country = new_df[r,"region"][[1]]
    value_type = new_df[r,"value_type"][[1]]
    dt = as.Date(new_df[r,"date"][[1]])
    narrowed = df %>%
      dplyr::filter(region == country) %>%
      dplyr::filter(value_type == value_type) %>%
      dplyr::filter(date <= dt) %>%
      dplyr::filter(date >= (as.Date(dt) - 6))
    
    if(length(df$province) > 0) {
      narrowed = narrowed %>%
        dplyr::filter(province == prov)
    }
    
    value = 0
    if(nrow(narrowed) > 0) {
      value = max(narrowed$value)
    }
    return(value)
  }
  
  value_vec = sapply(1:nrow(new_df), function(x) small(x, df, new_df))
  #------------------------------------------------------------------
  
  
  # for-loop which idenitifies the max value from prior week to fill into new data frame
  value_vec = c()
  for(i in 1:nrow(new_df)) {
    if(length(df$province) > 0){
      prov = new_df[i,"province"][[1]]
    }
    country = new_df[i,"region"][[1]]
    value_type = new_df[i,"value_type"][[1]]
    dt = as.Date(new_df[i,"date"][[1]])
    narrowed = df %>%
      dplyr::filter(region == country) %>%
      dplyr::filter(value_type == value_type) %>%
      dplyr::filter(date <= dt) %>%
      dplyr::filter(date >= (as.Date(dt) - 6))
    
    if(length(df$province) > 0) {
      narrowed = narrowed %>%
        dplyr::filter(province == prov)
    }
    
    value = 0
    if(nrow(narrowed) > 0) {
      value = max(narrowed$value)
    } else {
      # If no data from prior week, fill in data value from week before
      if(i > 1 && 
         new_df$province[i - 1] == prov && 
         new_df$region[i - 1] == country &&
         new_df$value_type[i - 1] == value_type) {
        value = value_vec[i - 1]
      }
    }
    value_vec = c(value_vec, value)
  }
  
  value_vec
  # Add value column and reformat data frame to match previous format
  new_df = new_df %>%
    dplyr::mutate(value = value_vec) %>%
    dplyr::select(disease, province, region, date, value, value_type, pop_2016, lat, long)
  
  return(new_df)
}