library(httr)
library(XML)
library(stringr)
library(rvest)
library(selenium)
library(wordcloud2)
library(gtsummary)
library(gt)
library(tidyverse)
library(tidytext)
library(topicmodels)
## Use a headless browser for login
url <- "https://link.springer.com/journal/42001"

screenshot <- function(what) {
  base64 <- what$screenshot()
  raw <- base64enc::base64decode(base64)
  writeBin(raw, "screenshot.png")
}

session <- SeleniumSession$new(browser = "firefox")
session$status()
session$navigate(url)
session$current_url()
screenshot(session)

# Accept all cookies (actually it doesn't matter)
session$find_element("xpath", "//div//button[@data-cc-action = 'accept']")$click()
Sys.sleep(2)
# Login with email and password
session$find_element("xpath", "//span[@class = 'eds-c-header__widget-fragment-title']")$click()
Sys.sleep(2)
email_input <- session$find_element("xpath", "//input[@type = 'email']")
Sys.sleep(2)
email_input$send_keys("Your_Account", keys$enter)
Sys.sleep(2)
password_input <- session$find_element("xpath", "//input[@type = 'password']")
Sys.sleep(2)
password_input$send_keys("Your_Password", keys$enter)
Sys.sleep(5)
session$find_element("xpath", "//a[@aria-controls = 'account-nav-menu']")$click()
screenshot(session)
Sys.sleep(2)
# Navigate to the articles section using the top navigation bar
session$find_element("xpath", "//div//li//a[@aria-controls = 'articles']")$click()
Sys.sleep(2)
session$find_element("xpath", "//div//a[@data-track = 'nav_articles']")$click()

# Alternatively, navigate by clicking the "View all articles" button at the bottom
# Here, I click the button in the bottom.
# session$find_element("xpath", "//div//a[@data-track = 'click_all_articles']")$click()

# Now on the article list page!

# Then, get the HTML source of the current page and save it and close session
src <- session$get_page_source()
writeLines(src, "src.html")
session$close()
rm(session)
gc()

# Now, ready to go through pages and GET (literally) articles
#################################
src <- readLines("src.html")
dom <- htmlParse(src)
xpathSApply(dom, "//ul[@class = 'eds-c-pagination']//li/a/@href")
# We can find that the maximum pagination is 8.
#################################
article_page_urls <- paste0("https://link.springer.com/journal/42001/articles?page=", 1:8)

ua <- user_agent("DSSS student project/1.0 (MacOS; only for course assignment; contact: Your_Email_Address)")
article_page_responses <- lapply(
  article_page_urls, 
  function(url) {
    Sys.sleep(2)
    GET(url, ua)
    })
############
# Save and read with rds for eight pages
saveRDS(article_page_responses, "article_page_responses.rds")
article_page_responses <- readRDS("article_page_responses.rds")

each_page_contents <- 
  lapply(article_page_responses, 
         function(x) content(x, as = "text", encoing = "UTF-8"))

each_page_doms <- lapply(each_page_contents, htmlParse)

# Flatten the article urls for further lapply
article_urls <- lapply(each_page_doms, function(x){
  xpathSApply(x, "//div//h3//a//@href")
}) |> unlist()

saveRDS(article_urls, "article_urls.rds")

article_page_dois <- lapply(each_page_doms, function(x) xpathSApply(x, "//div//h3//a//@data-track-label")) |> unlist()

# GET responses for all article urls
# article_responses <- 
#   lapply(article_urls, 
#          function(x){
#            Sys.sleep(2)
#            GET(x, ua)
#          })

# GET responses for two articles as an example
article_responses <- 
  lapply(article_urls[1:2], 
         function(x){
           Sys.sleep(2)
           GET(x, ua)
         })

# Save and read with rds
saveRDS(article_responses, "article_responses.rds")
article_responses <- readRDS("article_responses.rds")

# I removed article_contents and replaced it with a pipe operator
# to avoid using the intermediate variable (article_contents),
# which often tempted me to inspect the result of article_contents 
# and occasionally caused the R session to crash.

article_doms <- 
  lapply(article_responses, 
         function(x) content(x, as = "text", encoing = "UTF-8")) |> 
  lapply(htmlParse)

article_titles <- sapply(article_doms, function(x) xpathSApply(x, "//section//h1", xmlValue))
article_authors <- sapply(article_doms, function(x) xpathSApply(x, "//div//ul[@data-test = 'authors-list']", xmlValue))
article_authors <- lapply(article_authors, function (x){
  str_extract_all(x, "[A-Z][A-Za-z\\-\\.]+(\\s[A-Z][A-Za-z\\-\\.]+)*")[[1]] |> 
    setdiff("ORCID")
})
article_authors <- sapply(article_authors, paste, collapse = ", ")

article_relative_pdf_urls <- lapply(article_doms, function(x) xpathSApply(x, "//section//div[@class = 'c-pdf-container']//a//@href"))
article_pdf_urls <- paste0("https://link.springer.com", article_relative_pdf_urls)
article_titles <- sapply(article_doms, function(x) xpathSApply(x, "//section//h1", xmlValue))
article_abstracts <- sapply(article_doms, function(x) {
  abstr <- xpathSApply(x, "//section[@data-title = 'Abstract']//p", xmlValue)
  if (length(abstr) == 0) {NA} else {abstr}
})
article_year <- sapply(article_doms, function(x) xpathSApply(x, "//div[@class = 'app-article-masthead__info']//time//@datetime")) |> 
  str_extract("^\\d{4}")

article_dois <- sapply(article_doms, function(x) 
{
  doi <- xpathSApply(x, "//li[@class = 'c-bibliographic-information__list-item c-bibliographic-information__list-item--full-width']", xmlValue)
  gsub("DOI: https://doi.org/","", doi)
})

df_article <- data.frame(
  year = article_year,
  title = article_titles,
  author = article_authors,
  pdf_url = article_pdf_urls,
  doi = article_dois,
  abstract = article_abstracts
)

# If I'm in home, NULL means the corresponding paper is non-open access one.
# If I'm in campus, no NULL herein :D
df_article <- df_article[!str_detect(df_article$pdf_url, "NULL"), ]

# In the end, only one paper was included because the other is not open access.
# So only one paper was downloaded.

write.csv(df_article, "pdf.csv", row.names = FALSE)
df_article <- read.csv("pdf.csv")


mapply(function(title, url) {
  # avoiding illegal characters in title
  safe_title <- gsub("[^a-zA-Z0-9]+", "_", title)
  
  Sys.sleep(2)
  # download pdf
  download.file(url, paste0("Yukun_Jiao_Download_", safe_title, ".pdf"), mode = "wb")
  
  cat("Downloading", title, "from", url, "\n")
}, df_article$title, df_article$pdf_url)


# I can also GET dois from article_relative_urls by cutting off something non-doi.
# article_dois <- lapply(article_relative_urls, function(x){gsub(".*/pdf/|\\.pdf", "", x)})

api_urls <- lapply(article_page_dois, function(doi)
{
  paste0("https://api.springernature.com/metadata/json?q=doi:", doi, "&api_key=Your_API_Key")
})

# limits
api_responses <- lapply(api_urls, function(x){
  Sys.sleep(0.7)
  GET(x)
})

saveRDS(api_responses, "api_responses.rds")
api_responses <- readRDS("api_responses.rds")
article_urls <- readRDS("article_urls.rds")
# Use regex to retrieve abstract, year, title and author
api_contents <- lapply(api_responses, function(x) content(x, as = "text", encoding = "UTF-8"))

api_abstracts <- sapply(api_contents, function(x) str_match(x, '"abstract":"(.*?)"')[,2])

api_years <- sapply(api_contents, function(x) str_match(x, '"publicationDate":"(\\d{4})')[,2])

api_titles <- sapply(api_contents, function(x) str_match(x, '"title":"(.*?)"')[, 2])

api_authors <- sapply(api_contents, function(x) str_match_all(x, '"creator":"(.*?)"')[[1]][, 2])
api_authors <- sapply(api_authors, paste, collapse = "; ")

api_volumes <- sapply(api_contents, function(x) str_match(x, '"volume"\\s*:\\s*"(\\d+)"')[, 2])

df_api <- data.frame(
  volume = api_volumes,
  year = api_years,
  title = api_titles,
  author = api_authors,
  url = article_urls,
  abstract = api_abstracts
)
df_api <- df_api[order(df_api$volume, df_api$title),]

write.csv(df_api, "df_api.csv", row.names = FALSE)
df_api <- read.csv("df_api.csv")

df_api |> gt()

#####################################
# mini analysis with topic models
df_api <- read.csv("df_api.csv")
CSS_abstracts <- df_api[, "abstract"]
library("tm")
corpus <- Corpus(VectorSource(unlist(CSS_abstracts)))

# ASCII
Sys.setlocale("LC_COLLATE", "C")

CSS_dtm <- DocumentTermMatrix(corpus, control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))

dim(CSS_dtm)
library("slam")
summary(col_sums(CSS_dtm))

term_tfidf <- tapply(CSS_dtm$v/row_sums(CSS_dtm)[CSS_dtm$i], CSS_dtm$j, mean) * log2(nDocs(CSS_dtm)/col_sums(CSS_dtm > 0))


summary(term_tfidf)

CSS_dtm <- CSS_dtm[,term_tfidf >= 0.1]
CSS_dtm <- CSS_dtm[row_sums(CSS_dtm) > 0,]
summary(col_sums(CSS_dtm))
dim(CSS_dtm)

library("topicmodels")
k <- 20
SEED <- 2077
css_TM <- list(VEM = LDA(CSS_dtm, k = k, control = list(seed = SEED)),
               VEM_fixed = LDA(CSS_dtm, k = k,
                               control = list(estimate.alpha = FALSE, seed = SEED)),
               Gibbs = LDA(CSS_dtm, k = k, method = "Gibbs",
                           control = list(seed = SEED, burnin = 1000,
                                          thin = 100, iter = 1000)),
               CTM = CTM(CSS_dtm, k = k,
                         control = list(seed = SEED,
                                        var = list(tol = 10^-4), em = list(tol = 10^-3))))
sapply(css_TM[1:2], slot, "alpha")

sapply(css_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))

Topic <- topics(css_TM[["VEM"]], 1)
Terms <- terms(css_TM[["VEM"]], 5)

Terms[,1:5]

(topics_v1 <- topics(css_TM[["VEM"]])[grep("1", df_api[, "volume"])])
most_frequent_v1 <- which.max(tabulate(topics_v1))
terms(css_TM[["VEM"]], 10)[, most_frequent_v1]

(topics_v2 <- topics(css_TM[["VEM"]])[grep("2", df_api[, "volume"])])
most_frequent_v2 <- which.max(tabulate(topics_v2))
terms(css_TM[["VEM"]], 10)[, most_frequent_v2]

# The 5 Most Frequent Terms in First 5 Topics
df_terms <- as.data.frame(Terms[, 1:5])
colnames(df_terms) <- paste0("Topic ", 1:5)
gt(df_terms)

# The Ten Most Frequent Terms for Each Volume
volumes <- 1:8

terms_by_volume <- lapply(volumes, function(v) {
  topics_v <- topics(css_TM[["VEM"]])[grep(as.character(v), df_api[, "volume"])]
  most_frequent <- which.max(tabulate(topics_v))
  terms(css_TM[["VEM"]], 10)[, most_frequent]
})

df_terms_wide <- as.data.frame(terms_by_volume)
colnames(df_terms_wide) <- paste0("Volume ", volumes)
df_part1 <- df_terms_wide[, 1:4]  # Part 1: volumes 1 to 4
df_part2 <- df_terms_wide[, 5:8]  # Part 2: volumes 5 to 8

df_part1 |> gt()
df_part2 |> gt()