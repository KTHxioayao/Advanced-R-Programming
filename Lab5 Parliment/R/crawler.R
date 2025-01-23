library(rvest)
library(stringr)
library(tibble)
library(dplyr)
library(readr)
library(usethis)

parliments_source <-  "https://www.riksdagen.se/en/members-and-parties/members/"


#' Get Party Names and Abbreviations
#'
#' Returns a list of party names and their corresponding abbreviations.
#'
#' @return A named list where keys are full party names and values are their abbreviations.
#'
#' @examples
#' # Example usage:
#' get_parties()
get_parties <- function () {
  return (list('The Social Democratic Party'='socdem',
               'The Sweden Democrats'='swedem',
               'The Moderate Party'='mod',
               'The Left Party'='lft',
               'The Centre Party'='cen',
               'The Christian Democrats'='chrdem',
               'The Green Party'='grn',
               'The Liberal Party'='lib'))
}

#'
#' Get Member Detail URLs and Names by Party
#'
#' Extracts member detail URLs and names from an HTML document for a given party.
#'
#' @param html An HTML document object obtained using `read_html`.
#' @param parti A string representing the political party.
#' @return A list containing two elements:
#'         - `member_detail_urls`: A vector of member detail URLs.
#'         - `member_names`: A vector of member names.
#'
#' @examples
#' # Example usage:
#' library(rvest)
#' # Assume `html` is an HTML document object
#' html <- read_html("https://www.riksdagen.se/en/members-and-parties/members/?parti=socdem")
#' get_name_return_detail_url_by_party(html, "socdem")
get_name_return_detail_url_by_party <- function (html,parti) {
  if (!parti %in% get_parties()){
    stop("party name is invalid")
  }
  member_detail_urls <- list()
  member_detail_nodes <-html %>%
    html_nodes("#__next")%>%
    html_nodes("div.sc-e76b6126-0.iAtpHa")%>%
    html_nodes("div:nth-child(1)")%>%
    html_nodes("div.sc-fc94b50d-1.icalgq") %>%
    html_nodes('a')
  member_detail_urls <- member_detail_nodes %>%  html_attr(name = "href")
  return (list(member_detail_urls))
}


#' Catch Member Information by Party
#'
#' This function collects member information by iterating through a list of political parties.
#'
#' @param parliments_source A string representing the source URL for parliament members' information.
#' @return A tibble containing the collected member information.
catch_member_info_by_party <- function(parliments_source){
  member_info_df <- tibble(
    Name = character(),
    Party = character(),
    Constituency = character(),
    Birthyear = character(),
    Address = character(),
    Email = character(),
    Telephone = character(),
    Title = character(),
    Age = numeric()
  )
  parties <- unlist(get_parties())
  for (parti in parties) {
    cat(sprintf("start to catch name of member of %s \n" , parti))
    party_url <- paste(parliments_source,'?parti=' , parti, sep="")
    html <- read_html(party_url)
    member_detail_urls <- get_name_return_detail_url_by_party(html,parti)
    party_member_info_df <- get_member_detail_info(member_detail_urls[[1]],member_info_df)
    member_info_df <- bind_rows(member_info_df,party_member_info_df)
    member_info_df <- member_info_df %>% distinct(Name, .keep_all = TRUE)
    cat(sprintf("have record number of member:%d \n", nrow(member_info_df)))
  }
  register_data(member_info_df,parti)
}





#'
#' Extract detailed information of members
#'
#' This function extracts detailed information of each member from their respective detail URLs and appends it to a dataframe.
#'
#' @param detail_url A vector of member detail URLs
#' @param party_member_info_df The dataframe to store the extracted information
#'
#' @return An updated dataframe with member details
#'
#' @export
get_member_detail_info <- function(detail_url, party_member_info_df) {
  for (url in detail_url) {
    single_member_info <- list()
    detail_html <- read_html(url)
    info_nodes <- detail_html %>%
      html_nodes("#content") %>%
      html_nodes("div.sc-e76b6126-1.koghww")
    name <- info_nodes %>% html_nodes("h1") %>% html_text(trim = TRUE)
    single_member_info$Name <- c(name)
    info_names <- info_nodes %>% html_nodes("dt") %>% html_text(trim = TRUE)
    infos <- info_nodes %>% html_nodes("dd") %>% html_text(trim = TRUE)

    for (i in 1:length(info_names)) {
      att_name <- gsub("[^a-zA-Z]", "", info_names[[i]])
      info_value <- infos[[i]]

      if (!att_name %in% colnames(party_member_info_df)) {
        party_member_info_df[[att_name]] <- rep(NA, nrow(party_member_info_df))
      }

      single_member_info[[att_name]] <- c(info_value)

      if (att_name == 'Birthyear' && !is.null(info_value)) {
        age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(info_value)
        single_member_info$Age <- c(age)
      }
    }

    single_member_df <- data.frame(single_member_info)
    party_member_info_df <- bind_rows(party_member_info_df, single_member_df)
  }

  return(party_member_info_df)
}

#'
#' Register member basic information
#'
#' This function writes the member basic information to a CSV file and registers it in the project.
#'
#' @param member_basic_info The dataframe containing member basic information
#' @param party The political party of the members
#'
#' @export
register_data <- function(member_basic_info, party) {
  file_name <- "member_basic_info.csv"
  file_path <- paste("data-raw/", file_name, sep="")
  write_csv(member_basic_info, file_path)
  usethis::use_data(member_basic_info, overwrite = TRUE)
}
