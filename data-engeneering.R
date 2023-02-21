library(httr)
library(jsonlite)
library(dplyr)
library(bigrquery)

# usethis::edit_r_environ(scope = "project")

con <- dbConnect(
  bigquery(),
  project = "github-dashboard-378513",
  dataset = "interactions",
  billing = "github-dashboard-378513"
)

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
    df <- as.data.frame(
      fromJSON(
        content(get_details, "text", encoding = "UTF-8"), 
        flatten = TRUE
      )
    )
    
    if (nrow(df) > 0) {
      return(df[c("commit.message", "commit.author.date")])
    } 
  } 
  
  return(data.frame())
}

### Get the next date to start loading commits from the repository
get_next_date <- function() {
  tb_commits <- tbl(con, "commits")
  
  last_date <- tb_commits %>%
    summarise(last_date = max(executed_date, na.rm = TRUE)) %>%
    collect()
  
  next_date <- format(last_date[[1]] + 1, "%Y-%m-%dT%H:%M:%SZ")
  
  return(next_date)
}

### Insertign data into the BigQuery table
insert_values <- function(df, con) {
  if (nrow(df) > 0) {
    names(df) <- c("message", "executed_date")
    df$executed_date <- as.POSIXct(stringr::str_replace_all(df$executed_date, "Z|T", " "))
    
    dbWriteTable(con, "commits", df, append = TRUE)
  } else {
    message("No rows to be inserted")
  }
}

# url <- "https://api.github.com/repos/boa50/r-cheat-sheet/commits?since=2023-02-19T13:10:23Z"

url <- "https://api.github.com/repos/boa50/r-cheat-sheet/commits?author=boa50"

df <- call_api(url)

# insert_values(df, con)


next_date <- get_next_date()

url <- paste(
  "https://api.github.com/repos/boa50/r-cheat-sheet/commits?author=boa50&since=", 
  data_str, 
  sep = ""
)

df <- call_api(url)

