library(httr)
library(jsonlite)
library(dplyr)
library(bigrquery)

# usethis::edit_r_environ(scope = "project")

### Call the GitHub api based on the passed url
# Return a dataframe
call_api <- function(url) {
  get_details <- GET(
    {{ url }},
    add_headers("Accept", "application/vnd.github+json"),
    add_headers("Authorization", paste("Bearer", Sys.getenv("API_TOKEN"))),
    add_headers("X-GitHub-Api-Version", "2022-11-28")
  )
  
  if (status_code(get_details) == 200) {
    return(
      as.data.frame(
        fromJSON(
          content(get_details, "text", encoding = "UTF-8"), 
          flatten = TRUE
        )
      )[c("commit.message", "commit.author.date")]
    )
  } else {
    return(data.frame())
  }
}

# url <- "https://api.github.com/repos/boa50/r-cheat-sheet/commits?since=2023-02-19T13:10:23Z"

url <- "https://api.github.com/repos/boa50/r-cheat-sheet/commits?author=boa50"

df <- call_api(url)

### Insertign data into the BigQuery table
if (nrow(df) > 0) {
  names(df) <- c("message", "executed_date")
  df$executed_date <- as.POSIXct(stringr::str_replace_all(df$executed_date, "Z|T", " "))
  
  con <- dbConnect(
    bigquery(),
    project = "github-dashboard-378513",
    dataset = "interactions",
    billing = "github-dashboard-378513"
  )
  
  dbWriteTable(con, "commits", df, append = TRUE)
}