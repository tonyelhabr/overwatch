
library("tidyverse")
# library("rvest")

# nbastatR ----
url_bball <-"https://www.basketball-reference.com/leagues/NBA_2018.html"
read_page <-
  function(url) {
    page <-
      url %>%
      readr::read_lines() %>%
      str_replace_all("<!--|-->", "") %>%
      str_trim() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_c(collapse = "") %>%
      xml2::read_html()

    page
  }
page_bball <-
  url_bball %>%
  read_page()
page_bball

xml_tables_bball <-
  page_bball %>%
  rvest::html_nodes(xpath = "//*[contains(@class, 'sortable')]")
# //*[contains(concat( " ", @class, " " ), concat( " ", "center", " " ))]
page_bball %>%
  rvest::html_nodes(xpath = "//*[contains(@class, 'right')]")

# ow ----
url_ow <- "https://overwatchleague.com/en-us/stats"

# NOTE: This is for experimental purposes only.
blob_ow <-
  url_ow %>%
  readr::read_lines() %>%
  str_replace_all("<!--|-->", "") %>%
  str_trim() %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  str_c(collapse = "")
blob_ow %>%
  readr::write_lines(path = file.path("data", "temp.txt"))
page_ow <-
  url_ow %>%
  read_page()

page_ow %>%
  rvest::html_nodes(xpath = "//*[contains(@class)]")

path_dl_ow <- file.path("data-raw", "stats.html")
download.file(url, destfile = path_dl, quiet = TRUE)

ns_ow <-
  path_dl_ow %>%
  xml2::read_html() %>%
  rvest::html_nodes("g")
ns_ow

# //*[contains(concat( " ", @class, " " ), concat( " ", "Table-header--nowrap", " " ))] # table header?
# //*[contains(concat( " ", @class, " " ), concat( " ", "griddle-table-body", " " ))] # entire table?
# //*[@id="stats"]/div/div/div[2]/div[3]/table/tbody/tr[1]/td[5] # table cell?
url_ow %>%
  xml2::read_html() %>%
  # rvest::html_nodes(xpath = "//*[contains(@class, 'Table-header--nowrap')]")
  rvest::html_nodes(xpath = '//*[@id="stats"]') %>%
  rvest::html_nodes("div")

path_dl_ow %>%
  xml2::read_html() %>%
  rvest::html_nodes("script")
path_dl_ow %>%
  xml2::read_html() %>%
  rvest::html_nodes("tr")

