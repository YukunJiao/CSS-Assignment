library(httr)
library(XML)
library(stringr)
library(rvest)
library(selenium)

## Another simple website with downloading pdf and headerless browswer
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
# NOW, I'm in the article page!
# or I can CLICK or GET the "View all articles" url in the bottom
# session$find_element("xpath", "//div//a[@data-track = 'nav_articles']")$click()
# GET

# Then, get the html, and store-read it
src <- session$get_page_source()
writeLines(src, "src.html")
session$close()
rm(session)
gc()
# Now, I'm ready to go through every pages and GET every articles
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

# I deleted article_contents, which 经常诱惑我去执行查看结果 然后进程卡死
article_doms <- 
  lapply(article_responses, 
         function(x) content(x, as = "text", encoing = "UTF-8")) |> 
  lapply(htmlParse)

article_relative_urls <- lapply(article_doms, function(x) xpathSApply(x, "//section//div[@class = 'c-pdf-container']//a//@href"))
article_urls <- paste0("https://link.springer.com", article_relative_urls)
article_titles <- sapply(article_doms, function(x) xpathSApply(x, "//section//h1", xmlValue))

df <- data.frame(
  title = article_titles,
  url = article_urls
)
df <- df[!str_detect(df$url, "NULL"), ]

mapply(function(title, url) {
  # avoiding illegal characters in title
  safe_title <- gsub("[^a-zA-Z0-9]", "_", title)
  
  Sys.sleep(2)
  # download pdf
  download.file(url, paste0("css_paper//", safe_title, ".pdf"), mode = "wb")
  
  cat("Downloading", title, "from", url, "\n")
}, df$title, df$url)

##这里要么访问每一个连接 然后去点击 要么直接get这些连接 然后再get 再download

# 为了获取剩下的无法访问的 可以使用selenium吧
# 但还不清楚如何在session中登录lisam
# 模拟不出来点击行为和输入