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
  response <- GET(
    {{ url }},
    add_headers("Accept", "application/vnd.github+json"),
    add_headers("Authorization", paste("Bearer", Sys.getenv("API_TOKEN"))),
    add_headers("X-GitHub-Api-Version", "2022-11-28")
  )
  
  if (status_code(response) == 200) {
    df <- as.data.frame(
      fromJSON(
        content(response, "text", encoding = "UTF-8"),
        flatten = TRUE
      )
    )

    return(df)
  }

  return(data.frame())
}

### Get my GitHub repositories
get_repositories <- function() {
  df_repositories <- data.frame()
  
  for (page in c(1:50)) {
    url <- paste("https://api.github.com/users/boa50/repos?page=", page, sep = "")
    df <- call_api(url)
    
    if (nrow(df) > 0) {
      df_repositories <- rbind(df_repositories, df["name"])
    }
    
    if (nrow(df) < 30) {
      break
    }
  }
  
  return(df_repositories)
}

### Get the my commits at a single repository
get_commits <- function(repository, next_date = "1970-01-01T00:00:00Z") {
  url <- paste(
    "https://api.github.com/repos/boa50/",
    repository,
    "/commits?author=boa50&since=", 
    next_date, 
    sep = ""
  )
  
  df <- call_api(url)
  
  if (nrow(df) > 0) {
    return(df[c("commit.message", "commit.author.date")])
  }
  
  return(df)
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
    names(df) <- c("message", "executed_date", "repository")
    df$executed_date <- as.POSIXct(stringr::str_replace_all(df$executed_date, "Z|T", " "))
    
    dbWriteTable(con, "commits", df, append = TRUE)
  } else {
    message("No rows to be inserted")
  }
}


############################# Program Execution ################################
df_repositories <- get_repositories()

# next_date <- get_next_date()
# next_date <- "1970-01-01T00:00:00Z"

df_commits <- data.frame()

for (repo in df_repositories$name) {
  message(repo)
  
  df_tmp <- get_commits(repo, next_date)
  df_tmp$repository <- repo
  df_commits <- rbind(df_commits, df_tmp)
  
  if (repo == "analise-sentimentos") {
    break
  }
}

rm(df_tmp)

# insert_values(df_commits, con)
