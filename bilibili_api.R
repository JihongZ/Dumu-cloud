library(shiny)
library(shinyjs)
library(cidian)
library(jiebaR)
library(wordcloud2)
library(xml2)
library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(wordcloud)

# 主页API
api <- "www.bilibili.com/index/ding.json"
api2 <- "http://api.bilibili.cn/search"
response <- httr::GET(url = api2, )
bilibili.list <- jsonlite::parse_json(response)
bilibili.list$code
bilibili.list$result


# bilibili.list$technology$`0`$tname
bilibili.list$technology$`1`$tname
bilibili.list$technology$`1`$pic
bilibili.list$technology$`1`$title
bilibili.list$technology$`1`$pubdate
bilibili.list$technology$`1`$ctime
bilibili.list$technology$`1`$cid
bilibili.list$technology$`2`$title
bilibili.list$technology$`1`$owner$name

bilibili.list$technology$`2`$tname
bilibili.list$technology$`3`$tname
bilibili.list$technology$`4`$pic

html_nodes(content(response), xpath = )

unlist(lapply(bilibili.list$technology, function(x) {x$title}))
unlist(lapply(bilibili.list$technology, function(x) {x$owner$name}))
unlist(lapply(bilibili.list$technology, function(x) {x$cid}))
