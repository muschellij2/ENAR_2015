library(shiny)
library(shinyapps)
library(stringr)
library(tm)
library(plyr)
### make shiny max request size 30MB
options(stringsAsFactors=FALSE)

#######################################
# ENAR Scrape
#######################################
load(file="ENAR_2014.Rda")
# end = read.csv(file="ENAR_2014.csv")

shinyServer(function(input, output, session) {
  # output$filetable <- renderTable({
  extract.data = reactive({
    days = unique(end$day)  
    sess = unique(end$sessname)
    days = c("All", days)
    sess = c("All", sess)
#     message("extract")
    return(list(day=days, df=end, sess=sess))
  })


	# Partial example
  output$day <- renderUI({
    ed = extract.data()
    day = ed$day
#     message("drop_inst")
#     print(head(day))
    selectInput("drop_day", "Day:",
                choices= day, selected=day[1])
  })  
	
  # Partial example
  output$sess <- renderUI({
    ed = extract.data()
    sess = ed$sess
#     message("drop_sess")
    print(head(sess))
    selectInput("drop_sess", "Session:",
                choices= sess, selected=sess[1])
  })  

  get.dat = reactive({
    sess = input$drop_sess
    day = input$drop_day
#     message("inhere")
#     message(names(input))
#     message("crap")
    if(is.null(day) | is.null(sess)){
      return()    
    }
#     message("outhere")
    dat = end
    message(sess)
    if (sess != "All") {
      print(head(dat$sessname))
      dat = dat[ dat$sessname %in% sess, ]
    } else {
      message("in sess all")
      sess = paste0(sess, " Sessions")
      dat = dat
    }
    if (day != "All") {
      dat = dat[ dat$day %in% day, ]
    } else {
      day = paste0(day, " Days")
    }
    #     print(dat)
    if (input$search != ""){
      search = str_trim(tolower(input$search))
      print(search)
      g = grepl(search, dat$whole) | grepl(search, dat$sessname)
      print(sum(g))
      dat = dat[g, ]
    }
    #     paste0("Frequency of for ", sess, " for ", day))    
#     Encoding(dat$sessname) = "latin1"
#     dat$sessname = iconv(dat$sessname, "latin1", "UTF-8")
    message("printer")

    dat = dat[, c("day", "sessname", "whole", "where", "time")]
    colnames(dat) = c("Day", "Session Name", "Title", "Location", "Time")
#     dat = dat[, c("day", "time")]
#     message(dim(dat)[2])
    if(dim(dat)[1] != 0){
      return(dat)
    } else {
      return(data.frame("no data"))
    }
  })

  output$outtab = renderTable({
    x = get.dat()
    return(x)
  }, include.rownames=FALSE)


output$dlcsv <- downloadHandler(
  filename = function() {
    searcher = ""
    if (input$search != "") searcher = paste0("_", input$search)
    paste0('table', searcher, '.csv')
  },
  content = function(file) {
    dat = get.dat()
    write.csv(dat, file, row.names=FALSE)
  }
)


})