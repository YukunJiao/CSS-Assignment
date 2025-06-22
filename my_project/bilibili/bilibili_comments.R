library(httr)
library(XML)
library(stringr)
library(selenium)
library(jsonlite)
library(quanteda)
library(tidyverse)
##
cookies <- 'SESSDATA=5c50b188%2C1764597402%2C2a104%2A62CjBElobWRYKZErDi6c9JrYMtvxRUHVsvYiv0rAFbOcF7JxolNWAkY_PuCmWIR4vEWz4SVlZwNS1wOTVQYWRGblJhTnBtcGg3NThOV0FJR2ZhZnd5ZWdnRE5qNVljcmVFSk40eDdXaXdGSmdZMGlSQVVEdHozemdTdVhlNVlQSExiazV5WEZGbHR3IIEC;domain=.bilibili.com;path=/'
url <- "https://www.bilibili.com/video/BV1RLNJzrE4X"
bv <- sub("https://www.bilibili.com/video/", "", url)
# second method: GET with cookies and Comments with cookies and API
headers <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
  `Referer` = paste0(url),
  `Cookie` = cookies
)

response <- GET(url, headers)
page_content <- content(response, as = "text", encoding = "UTF-8")
dir.create(bv, showWarnings = FALSE)
writeLines(page_content, paste0(bv, "/bilibili_bv.html"))

# 上面的aid (b站视频av号) 也可以通过下面的正则匹配的到

url_api <- "https://api.bilibili.com/x/v2/reply/main"
oid <- str_match(page_content, '"aid":\\s*(\\d+)')[,2] |> as.numeric()
response_comments <- GET(url_api, headers, 
                         query = list(
                           type = 1,
                           oid = oid,
                           mode = 3)
)
page_comments <- content(response_comments, as = "text", encoding = "UTF-8")
# writeLines(page_comments, paste0(bv, "/bilibili_next_offset.html"))
json_data <- fromJSON(page_comments)
# json_data$data$replies$content$message
next_offset <- json_data$data$cursor$pagination_reply$next_offset
next_page <- 1
while (TRUE) {
  url_comments <- paste0("https://api.bilibili.com/x/v2/reply/main?next=", next_page,
                         "&type=1&oid=", oid, "&mode=3&next_offset=", next_offset)
  Sys.sleep(2)
  response_comments <- GET(url_comments, headers)
  
  page_contents <- content(response_comments, as = "text", encoding = "UTF-8")
  
  writeLines(page_contents, paste0(bv, "/bilibili_comments_", next_page, ".html"))
  
  next_offset <- str_match(page_contents, '"next_offset":"(.*?)"')[,2]
  
  if (is.na(next_offset) || next_offset == "") {
    break
  }
  next_page <- next_page + 1
}


##########
##########

files <- list.files(path = bv, pattern = "^bilibili_comments_\\d+\\.html$", full.names = TRUE)

read_comments <- function(file_path) {
  comments <- readLines(file_path, warn = FALSE) |> fromJSON(flatten = TRUE)
  return(comments)
}

list_of_comments <- lapply(files, read_comments)

get_uid  <- function(x) x$data$replies$member.mid
get_user  <- function(x) x$data$replies$member.uname
get_message <- function(x) x$data$replies$content.message
get_likes <- function(x) x$data$replies$like
get_time <-  function(x) x$data$replies$ctime |> as.POSIXct(origin = "1970-01-01", "Asia/Shanghai")

df_parent <- lapply(list_of_comments, function(x) {
  replies <- x$data$replies
  if (is.null(replies) || length(replies) == 0) {
    return(NULL)
  }
  data.frame(
    uid = replies$member.mid,
    user = replies$member.uname,
    location = replies$reply_control.location,
    message = replies$content.message,
    likes = replies$like,
    n_of_message = replies$reply_control.sub_reply_entry_text,
    time = as.POSIXct(replies$ctime, origin = "1970-01-01", tz = "Asia/Shanghai"),
    stringsAsFactors = FALSE
  )
}) |> bind_rows()

{
  df_parent$n_of_message <- as.integer(stringr::str_extract(df$n_of_message, "\\d+"))
  df_parent$n_of_message[is.na(df$n_of_message)] <- 0
  df_parent$location <- sub("^IP属地：", "", df$location)
  }

write.csv(df_parent, paste0(bv, "/df_parent.csv"), row.names = FALSE)

# 把所有分页评论合并成一个 data.frame
# 这里假设 replies 是 list，每条是一个评论，包含多字段

root_comment_id <- function(x) {x$data$replies$rpid}
root_comment_ids <- lapply(list_of_comments, root_comment_id) |> unlist()
root_comment_ids <- root_comment_ids[1:2]
get_sub_replies <- function(root_rpid) {
  url_sub_comments <- paste0(
    "https://api.bilibili.com/x/v2/reply/reply?oid=", oid,
    "&type=1&root=", root_rpid,
    "&ps=20&pn=1"
  )
  response <- GET(url_sub_comments, headers)
  Sys.sleep(2)
  
  if (status_code(response) != 200) return(NULL)
  sub_comments_data <- response |> content(as = "text", encoding = "UTF-8") |> fromJSON()
  replies <- sub_comments_data$data$replies
  
  data.frame(
    root = replies$root,
    dialog = replies$dialog,
    uid = replies$member$mid,
    user = replies$member$uname,
    message = replies$content$message,
    location = replies$reply_control$location,
    time = replies$ctime |> as.POSIXct(origin = "1970-01-01", "Asia/Shanghai")
  )
}


list_of_sub_dfs <- lapply(root_comment_ids, get_sub_replies)
df_sub <- list_of_sub_dfs |> bind_rows()

write.csv(df_sub, paste0(bv, "/df_sub.csv"), row.names = FALSE)

# rid是评论区中的id
# mid是发送者uid
# root是根评论id 要留下作为匹配的identifier
# dialog是一个人对楼主的一级评论留下的二级评论 和其他人对该二级评论的评论
# ctime like member$mid member$uname
# member$level_info$current_level
# content$message
# reply_control$location

# 这一段是练习查看用的
# 
# sub_replies <- sub_comments_data$data$replies
# df_sub <- data.frame(
#   rid = sub_replies$root,
#   dialog = sub_replies$dialog,
#   uid = sub_replies$mid,
#   message = sub_replies$content$message,
#   location = sub_replies$reply_control$location,
#   time = sub_replies$ctime |> as.POSIXct(origin = "1970-01-01", "Asia/Shanghai")
# )

#####
#####


# comments <- readLines(paste0(bv, "/bilibili_comments_1.html"), warn = FALSE) |> fromJSON()
# test <- comments$data$replies$content$max_line
# 
# 
# comments$data$replies$replies
# vvv <- test$reply_control
# 
# a <- data.frame(
#   uid = comments$data$replies$member$mid,
#   user = comments$data$replies$member$uname,
#   message = comments$data$replies$content$message,
#   likes = comments$data$replies$like,
#   locations <- comments$data$replies$reply_control$location,
#   time = comments$data$replies$ctime |> as.POSIXct(origin = "1970-01-01", "Asia/Shanghai"))


