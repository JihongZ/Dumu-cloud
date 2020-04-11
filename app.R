#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# devtools::install_github("qinwf/cidian") #用来从github上安装R包的函数，“qinwf”应该是cidian的作者

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

options(shiny.usecairo = FALSE)
# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux') {
    dir.create(font_home())
    file.copy('wqy-zenhei.ttc', font_home())
    system2('fc-cache', paste('-f', font_home()))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("B站弹幕词云alpha版"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("url_text","请输入视频网址(可抓取做多个视频):", 
                      # value = "https://www.bilibili.com/video/BV1xK4y1r7Gy"),
                      value = "https://www.bilibili.com/video/BV1gE411u7dF\nhttps://www.bilibili.com/video/BV1xK4y1r7Gy\nhttps://www.bilibili.com/video/BV1JW411c7WV?from=search&seid=6036054819977187147"),
            
            
            actionButton("sure", "画出词云"),
            actionButton("add", "加一个视频"),
            actionButton("randomize", "随机一下"),
            
            textInput("keyword_text","请输入关键词(work in progress):", 
                          # value = "https://www.bilibili.com/video/BV1xK4y1r7Gy"),
                          value = "小樱"),
        ),
        
        

        # Show a plot of the generated word cloud
        mainPanel(
      
            htmlOutput("section_name", container = div),
            h3(textOutput("counts", container = div)),
            htmlOutput("caption", container = div),
            htmlOutput(outputId = "author"),
            wordcloud2Output('wordcloud'),
            h5("Author: 张吉鸿", align = "right")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  
      observe({
          
            url = strsplit(input$url_text, "\n")[[1]][strsplit(input$url_text, "\n")[[1]] != ""]
            html.content <- lapply(url, read_html)
            
           
        })

      observeEvent(input$sure, {
            
            ## url_search = https://search.bilibili.com/all?keyword=%E6%B7%A1%E9%BB%84&from_source=nav_search_new
            # url_search <- paste0("https://search.bilibili.com/all?keyword=", input$keyword_text, "&from_source=nav_search_new")
            # search_html <- content(httr::GET("https://search.bilibili.com/all?keyword=%E6%B7%A1%E9%BB%84&from_source=nav_search_new", authenticate("z.john92@gmail.com", "180801@Zjh")))
            # search_nodes <- 
            #   html_nodes(search_html, css = "ul.video-list.clearfix a.title")
            # url = html_attr(search_nodes, "href")
            # url = sub(x = url, "//", "")
            
            
            
            url = strsplit(input$url_text, "\n")[[1]][strsplit(input$url_text, "\n")[[1]] != ""]
            
            html.content <- lapply(url, read_html)
            
            output$caption <- renderUI({ 
              title_name <- lapply(html.content, function(x) {
                as.character(xml_attr(xml_find_all(x, xpath = "//h1"), "title"))}) 
              # return(paste0(unlist(title_name), collapse = "\t" ))
              HTML(paste0(unlist(title_name), sep = '<br/>'))
            })
            
            output$counts <- renderText({ 
              as.character(paste0("总共抓取视频数：",length(url)))
            })
            
            output$author <- renderUI({ 
              up_name <- lapply(html.content, function(x) {
                xml_text(xml_find_all(x, xpath = "//a[@report-id='name']"))}) 
              HTML(paste0("up主：", paste0(unlist(up_name), sep = '<br/>')))
            })
            
            # 抓取弹幕
            # url = input$url
            # url = "http://api.bilibili.cn/view"
            # url <- "http://www.bilibili.tv/widget/getPageList?aid={BV1Ca4y1t7YG}"
            # 
            # res_html <- GET(url)
            #     read_html(url) %>% 
            #     html_nodes("head") %>% 
            #     html_text()
            # 
            # html_nodes(pg_html,xpath = "//head/script/cid")
            
            ## find the cid for given url
            
            # url = "https://www.bilibili.com/video/BV1jZ4y1j7Sq"
            # url2 = "https://www.bilibili.com/video/BV1bK4y1C7hp"
            
            # xml_find_all(html.content, xpath = "//h1[@class='video-title']")
            # xml_text(xml_find_all(html.content, xpath = "//a[@report-id='name']"))
            # 
            ## title
            
            
            
            # "cid":172503161,"dimension"
            # cid = "172503161"
            extrac_damu_vec <- function(html_cont) {
              # extract cid for the video
              html_txt <- xml_text(xml_find_all(html_cont, xpath = "//head"))
              cid_chunck = str_extract(html_txt, "\"cid\":(\\d+),\"dimension\"")
              cid = str_extract(cid_chunck, "\\d+")
              
              ## extract all damu from the video based on the cid
              url.xml = paste0("http://comment.bilibili.com", "/" ,cid, ".xml")
              html.response <- httr::GET(url.xml)
              html.xml = content(html.response, encoding = "UTF-8")
              dm <- rvest::html_nodes(html.xml, xpath = "/i/d/text()")              
              dm_vector <- xml_text(dm) %>% unlist()
              
              # remove the white space
              dm_vector <- sapply(dm_vector, function(x) {gsub(pattern = " ", replacement = "", x)})
              return(dm_vector)
            }
            
            dumu_vec <- unlist(lapply(html.content, extrac_damu_vec))
            
            # 进行分词
            mixseg <- worker()
            wordsC = segment(dumu_vec, mixseg)
            filter = c("了", "吧","啊", "哈哈哈","啊啊啊","我","是","你","就","的","这")
            wordsC <- filter_segment(wordsC, filter)
            # decode_scel(scel = "./网络流行新词.scel",
            #             output = "网络流行新词.scel_2020-04-05_19_37_54.dict")
            # show_dictpath()
            # scan(file = paste0(show_dictpath(), "/user.dict.utf8"),
            #      what=character(),nlines=30,sep='\n',
            #      encoding='utf-8',fileEncoding='utf-8')
            # wordsC <- base::setdiff(x = wordsC, y =  stop_words)
            wordsC <- as.data.frame(table(wordsC)[order(-as.numeric(table(wordsC)))])
            # 进行词云绘制
            
            output$wordcloud <- renderWordcloud2({
                # wordcloud(words = word, freq = Freq,
                #           col = brewer.pal(8, "Set1"), random.color = T, 
                #           random.order = T,  scale = c(3, 1), family="STKaiti")                  
                wordcloud2(wordsC, 
                           color = "random-dark", 
                           backgroundColor = "white",
                           fontFamily = "STKaiti",
                           fontWeight = "bold",
                           size = 2,
                           # shape = c("circle", "cardioid", "diamond", "triangle-forward",
                           #"triangle", "pentagon", "star")[sample(x = 1:5, size = 1)]
                           shape = "pentagon"
                           ) # 先排序再做词云
            })
      })
        
      observeEvent(input$randomize, {
        # 主页API
        api <- "www.bilibili.com/index/ding.json"
        response <- httr::GET(url = api)
        bilibili.list <- jsonlite::parse_json(response)
        
        all_sections <- c(names(bilibili.list))
        all_sections <- all_sections[!all_sections == ""]
        one_section <- all_sections[sample(1:length(all_sections),1)]
        content_section <- bilibili.list[[one_section]]
        
        output$section_name <- renderUI({ 
          # return(paste0(unlist(title_name), collapse = "\t" ))
          HTML(paste0("你选到了",one_section, "区"))
        })
        
        output$caption <- renderUI({ 
          # return(paste0(unlist(title_name), collapse = "\t" ))
          HTML(paste0(unlist(lapply(content_section, function(x) {x$title})), sep = '<br/>'))
        })
        
        
        output$author <- renderUI({ 
          HTML(paste0("up主：", paste0(unlist(lapply(content_section, function(x) {x$owner$name})),
                                     sep = '<br/>')))
        })
        
        extrac_damu_from_cid <- function(cid) {
          
          ## extract all damu from the video based on the cid
          url.xml = paste0("http://comment.bilibili.com", "/" ,cid, ".xml")
          html.response <- httr::GET(url.xml)
          html.xml = content(html.response, encoding = "UTF-8")
          dm <- rvest::html_nodes(html.xml, xpath = "/i/d/text()")              
          dm_vector <- xml_text(dm) %>% unlist()
          
          # remove the white space
          dm_vector <- sapply(dm_vector, function(x) {gsub(pattern = " ", replacement = "", x)})
          return(dm_vector)
        }
        
        
        
        dumu_vec <- unlist(lapply(lapply(content_section, function(x) {x$cid}), extrac_damu_from_cid))
        
        # 进行分词
        mixseg <- worker()
        wordsC = segment(dumu_vec, mixseg)
        filter = c("了", "吧","啊", "哈哈哈","啊啊啊","我","是","你","就","的","这")
        wordsC <- filter_segment(wordsC, filter)
        # decode_scel(scel = "./网络流行新词.scel",
        #             output = "网络流行新词.scel_2020-04-05_19_37_54.dict")
        # show_dictpath()
        # scan(file = paste0(show_dictpath(), "/user.dict.utf8"),
        #      what=character(),nlines=30,sep='\n',
        #      encoding='utf-8',fileEncoding='utf-8')
        # wordsC <- base::setdiff(x = wordsC, y =  stop_words)
        wordsC <- as.data.frame(table(wordsC)[order(-as.numeric(table(wordsC)))])
        # 进行词云绘制
        
        output$wordcloud <- renderWordcloud2({
          # wordcloud(words = word, freq = Freq,
          #           col = brewer.pal(8, "Set1"), random.color = T, 
          #           random.order = T,  scale = c(3, 1), family="STKaiti")                  
          wordcloud2(wordsC, 
                     color = "random-dark", 
                     backgroundColor = "white",
                     fontFamily = "STKaiti",
                     fontWeight = "bold",
                     size = 2,
                     # shape = c("circle", "cardioid", "diamond", "triangle-forward",
                     #"triangle", "pentagon", "star")[sample(x = 1:5, size = 1)]
                     shape = "pentagon"
          ) # 先排序再做词云
        })
      })
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
