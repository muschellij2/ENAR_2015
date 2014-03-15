library(shiny)
library(shinyapps)
library(stringr)
library(tm)
library(plyr)
library(RCurl)
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
    time = as.character(sort(unique(end$est.time)))
    days = c("All", days)
    sess = c("All", sess)
    time = c("All", time)
    #     message("extract")
    return(list(day=days, df=end, sess=sess, time=time))
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
	
  output$time <- renderUI({
    ed = extract.data()
    day = ed$time
    #     message("drop_inst")
    #     print(head(day))
    selectInput("drop_time", "Time:",
                choices= time, selected=time[1])
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
    est.time = input$drop_time
    #     message("inhere")
#     message(names(input))
#     message("crap")
    if(is.null(day) | is.null(sess)){
      return()    
    }
#     message("outhere")
    dat = end

# mystr = c('<a href="http://example.com/link-to-your-event" class="addthisevent">Add to Calendar<span class="_start">%s %s</span>  <span class="_end">%s %s</span>  <span class="_zonecode">15</span>    <span class="_summary">%s </span> <span class="_description">%s</span>  <span class="_location">%s</span>  <span class="_all_day_event">false</span>  <span class="_date_format">MM/DD/YYYY</span></a>')
mystr = c('<a href="%s" target="Google Calendar" rel="nofollow">Add to my Google calendar</a>')
myurl = paste0('http://www.google.com/calendar/event?action=TEMPLATE&',
'text=%s&', 
"dates=%sT%sZ/%sT%sZ&",
"details=%s&",
"location=%s&",
"trp=false&",
"sprop=&sprop=name:")


dat$ev = sprintf(mystr, dat$dday, dat$ttime, dat$dday, 
                 dat$next.time, dat$sessname, dat$whole, dat$where)
dat$date = gsub("(.*)/(.*)/(.*)", "\\3\\1\\2", dat$dday)
dat$ev2 = sprintf(myurl, dat$whole, 
                  dat$date, gsub(":", "", dat$ttime), 
                  dat$date, gsub(":", "", dat$next.time),
                  dat$whole, dat$where)
dat$ev = sprintf(mystr, dat$ev2)
dat$ev2 = NULL


  
# dat$ev = '<a href="google.com"> goog</a>'
# dat$ev = curlEscape(dat$url)

    message(sess)
    if (est.time != "All") {
      message(est.time)
      print(head(dat$est.time))
      dat = dat[ dat$est.time %in% est.time, ]
    } else {
      message("in sess all")
      est.time = paste0(est.time, " Times")
    }
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
#     N = nrow(dat)
#     add.to.row = vector(mode="list", length=2)
#     names(add.to.row) = c("pos", "command")
#     add.to.row$pos = list()
#     for (irow in seq(N)) add.to.row$pos[[irow]] = irow
#     add.to.row$command = dat$ev
#     add.to.row <<- add.to.row

    dat = dat[, c("day", "sessname", "whole", "where", "time", "ev")]
    colnames(dat) = c("Day", "Session Name", "Title", "Location", 
                      "Time", "Add")
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
#     x = xtable(x, type="html", include.rownames=FALSE, 
#                sanitize.text.function = force)
    x
  }, include.rownames=FALSE, sanitize.text.function=`(`)


output$dlcsv <- downloadHandler(
  filename = function() {
    searcher = ""
    if (input$search != "") searcher = paste0("_", input$search)
    paste0('table', searcher, '.csv')
  },
  content = function(file) {
    dat = get.dat()
    cn = colnames(dat)
    cn = cn[ !(cn%in% "Add")]
    dat = dat[,cn]
    write.csv(dat, file, row.names=FALSE)
  }
)


})