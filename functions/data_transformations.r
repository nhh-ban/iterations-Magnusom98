#Transform metadata task

#Start of code
transform_metadata_to_df <- function(stations_metadata) {
  
  # Extract the first element of the list and convert each list element to a tibble
  df <- stations_metadata[[1]] %>%
    map(as_tibble) %>%
    # Bind all the tibbles together into one dataframe
    bind_rows() %>%
    # Extract the first value from the 'latestData' list (if present) or set as empty string
    mutate(latestData = map_chr(latestData, 1, .default = ""))  %>%
    # Convert the 'latestData' to datetime format with the specified timezone
    mutate(latestData = as_datetime(latestData, tz = "UTC"))  %>%
    # Unlist the 'location' column to extract lat and lon values
    mutate(location = map(location, unlist)) %>%  
    # Create new columns for latitude and longitude
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    # Remove the original 'location' column
    select(-location)
  
  # Return the transformed dataframe
  return(df)
}

#to_iso8601 Function

to_iso8601 <- function(datetime, offset_days) {
  # Convert to POSIXct in case the input is in character format
  datetime <- anytime::anytime(datetime)
  
  # Add the offset in days
  new_datetime <- datetime + lubridate::days(offset_days)
  
  # Convert to ISO8601 format with two-digit seconds and append 'Z'
  iso_str <- format(new_datetime, format="%Y-%m-%dT%H:%M:%SZ")
  return(iso_str)
}

#Json to API

