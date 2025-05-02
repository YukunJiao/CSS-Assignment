library(httr)
library(XML)
library(stringr)
library(rvest)
library(selenium)

# Ethics about the crawling?
# what should I talk about this?

## Another simple website
## with a headless browser for downloading pdf
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
# click once to close the cookie window
session$find_element("xpath", "//div//button[@data-cc-action = 'accept']")$click()
# I can either click twice to enter into article pages with buttons in the top
session$find_element("xpath", "//div//li//a[@aria-controls = 'articles']")$click()
session$find_element("xpath", "//div//a[@data-track = 'nav_articles']")$click()

# or I can CLICK or GET the "View all articles" url in the bottom
# Here, I click the button in the bottom.
# session$find_element("xpath", "//div//a[@data-track = 'click_all_articles']")$click()

# NOW, I'm in the article page!

# Then, get the article-page html, and store-read it
src <- session$get_page_source()
writeLines(src, "src.html")
session$close()
rm(session)
gc()

# Now, ready to go through pages and GET (literally) every article
#################################
src <- readLines("src.html")
dom <- htmlParse(src)
#################################

article_page_urls <- paste0("https://link.springer.com/journal/42001/articles?page=", 1:8)

article_page_responses <- lapply(
  article_page_urls, 
  function(url) {
    Sys.sleep(2)
    GET(url)
    })

############

# saving and readng with rds
saveRDS(article_page_responses, "article_page_responses.rds")
article_page_responses <- readRDS("article_page_responses.rds")

each_page_contents <- 
  lapply(article_page_responses, 
         function(x) content(x, as = "text", encoing = "UTF-8"))

each_page_doms <- lapply(each_page_contents, htmlParse)
articles_urls <- lapply(each_page_doms, function(x){
  xpathSApply(x, "//div//h3//a//@href")
})

# flatten the urls for lapply
articles_urls_flatten <- articles_urls |> unlist()

article_responses <- 
  lapply(articles_urls_flatten, 
         function(x){
           Sys.sleep(1)
           GET(x)
         })

# saving and readng with rds
saveRDS(article_responses, "article_responses.rds")
article_responses <- readRDS("article_responses.rds")

# I removed article_contents and replaced it with a pipe operator
# to reduce the use of the intermediate variable (article_contents),
# which often tempted me to inspect the result of article_contents 
# and caused the R session to crash.

article_doms <- 
  lapply(article_responses, 
         function(x) content(x, as = "text", encoing = "UTF-8")) |> 
  lapply(htmlParse)

article_titles <- sapply(article_doms, function(x) xpathSApply(x, "//section//h1", xmlValue))

article_relative_urls <- lapply(article_doms, function(x) xpathSApply(x, "//section//div[@class = 'c-pdf-container']//a//@href"))
article_urls <- paste0("https://link.springer.com", article_relative_urls)

df <- data.frame(
  title = article_titles,
  url = article_urls
)

# If I'm in home, NULL means the corresponding paper is non-open access one.
# If I'm in campus, no NULL herein :D
df <- df[!str_detect(df$url, "NULL"), ]

write.csv(df, "pdf.csv", row.names = FALSE)
df <- read.csv("pdf.csv")

# sub the df into df_sub to download two articles
df_sub <- df[1:2, ]

mapply(function(title, url) {
  # avoiding illegal characters in title
  safe_title <- gsub("[^a-zA-Z0-9]+", "_", title)
  
  Sys.sleep(3)
  # download pdf
  download.file(url, paste0("css_paper_two//", safe_title, ".pdf"), mode = "wb")
  
  cat("Downloading", title, "from", url, "\n")
}, df_sub$title, df_sub$url)

# These codes would download 375 articles,
# which might get me banned
.
# mapply(function(title, url) {
#   # avoiding illegal characters in title
#   safe_title <- gsub("[^a-zA-Z0-9]+", "_", title)
#   
#   Sys.sleep(2)
#   # download pdf
#   download.file(url, paste0("css_paper//", safe_title, ".pdf"), mode = "wb")
#   
#   cat("Downloading", title, "from", url, "\n")
# }, df$title, df$url)

# In campus, I have access to all articles in this journal
# But when I'm home, I only have access to 183 articles (I forgot the exact number)
# So I should use selenium to log in lisam to get the access, right?
# But I cannot simulate the click and sendtokey for now.
# Whatever, I will add a log-in with session in the middle of the code.


# mini analysis on which institution the papers' author is affiliated with.
article_page_responses
dom <- article_doms[[30]]
dom
xpathSApply(dom, "//div[@class = 'c-article-header']", xmlValue)
xpathSApply(dom, "//div[@class = 'c-article-header']")
test <- xpathSApply(dom, "//div//ol[@class = 'c-article-references']", xmlValue)

# 