library(glue)

vol_qry <- function(id, from, to) {
  # Use glue to interpolate values into the string
  query <- glue::glue('{{
    trafficData(trafficRegistrationPointId: "{id}") {{
      byTime (from: "{from}", to: "{to}") {{
        edges {{
          node {{
            from
            to
            total {{
              volumeNumbers {{
                volume
              }}
            }}
          }}
        }}
      }}
    }}
  }}')
  return(query)
}