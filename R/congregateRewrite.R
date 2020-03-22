
#' #' Congregation of Data Dates
#' #' 
#' #' This function aligns and congregates data value counts on a weekly basis (taking the maximum if necessary)
#' #' so that the dates in the data are aligned for all data value types at all locations.
#' #' 
#' #' WARNING: This function manipulates the data in a way that makes the dates of observations 
#' #' less accurate, but in a way that is beneficial for plotting and understanding the cumulative
#' #' growth of the data.
#' #' 
#' #' @param df A data frame for which the data values will be congregated to weekly dates.
#' #' 
#' #' @return Output is a data frame of the same format as the passed-in df, but with congregated dates.
#' #' 
#' #' @importFrom purrr map pmap_dfr
#' #' @importFrom tibble tibble 
#' #' @importFrom dplyr left_join select rename distinct slice mutate n
#' #'  
#' #' @examples
#' #' zika_raw <- importZikaData()
#' #' congregateDataDates(zika_raw)
#' #' 
#' #' @export
#' #'
#' 
#' congregateDataDates <- function(df) {
#'   # Check that this is all the same disease 
#'   if (length(unique(df$disease)) != 1) {
#'     stop("'disease' column has multiple entries, function can only be used on one disease")
#'   }
#'   pop_col_name <- names(df)[grepl("pop_", names(df))]
#'   names(df)[grepl("pop_", names(df))] <- "pop"
#'   
#'   # Identify first and last dates
#'   first_date = min(as.Date(df$date))
#'   last_date = max(as.Date(df$date)) + 7
#'   # Sequence of weekly dates
#'   dates_used = seq(first_date, last_date, by = "weeks")
#'   
#'   if ("province" %in% names(df)) {
#'     map_df <- df[!duplicated(df[, c("province", "region", "value_type")]), ]
#'   } else {
#'     map_df <- df[!duplicated(df[, c("region", "value_type")]), ]
#'   }
#'   
#'   map_df_upd <- map_df %>%
#'     dplyr::slice(rep(1:(dplyr::n()), each = length(dates_used))) %>%
#'     dplyr::mutate(congregate_date = as.Date(dates_used))
#'   
#'   getCongregateValue <- function(date_congregate, type, reg, prov) {
#'     date_congregate <- as.Date(date_congregate)
#'     subset <- df[(df$date >= first_date & df$date <= date_congregate) & df$value_type == type & df$region == reg, ]
#'     if (!is.na(prov)) {
#'       subset <- subset[subset$province == prov, ]
#'     }
#'     max_value <- suppressWarnings(max(max(subset$value), 0))
#'     result <- tibble::tibble("congregate_date" = as.character(date_congregate), "value_type" = type, "region" = reg, "value" = max_value)
#'     if (!is.na(prov)) {
#'       result[["province"]] <- prov
#'     }
#'     return (result)
#'   }
#'   
#'   if ("province" %in% names(df)) {
#'     congregated <- purrr::pmap_dfr(list(map_df_upd$congregate_date,
#'                                         map_df_upd$value_type,
#'                                         map_df_upd$region,
#'                                         map_df_upd$province), getCongregateValue)
#'   } else {
#'     congregated <- purrr::pmap_dfr(list(map_df_upd$congregate_date,
#'                                         map_df_upd$value_type,
#'                                         map_df_upd$region,
#'                                         rep(NA, nrow(map_df_upd))), getCongregateValue)
#'   }
#'   congregated$congregate_date <- as.Date(congregated$congregate_date)
#'   
#'   if ("province" %in% names(df)) {
#'     joined <- congregated %>%
#'       dplyr::left_join(df, by = c("value_type" = "value_type",
#'                                   "region" = "region",
#'                                   "province" = "province"))
#'   } else {
#'     joined <- congregated %>%
#'       dplyr::left_join(df, by = c("value_type" = "value_type",
#'                                   "region" = "region"))
#'   }
#'   final <- joined %>%
#'     dplyr::select(-value.y, -date) %>%
#'     dplyr::rename("value" = "value.x", "date" = "congregate_date") %>%
#'     dplyr::select(disease, province, region, date, value, value_type, pop, lat, long) %>%
#'     dplyr::rename(!!pop_col_name := "pop") %>%
#'     dplyr::distinct()
#'   return (final)
#' }
#' 
