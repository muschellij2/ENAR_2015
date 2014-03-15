library(shinyapps)
load(file="ENAR_2014.Rda")
days = unique(end$day)  
sess = unique(end$sessname)
time = as.character(sort(unique(end$est.time)))
day = c("All", days)
sess = c("All", sess)
time = c("All", time)

shinyUI(pageWithSidebar(
  headerPanel("ENAR 2014 searcher"),
  sidebarPanel(
#     uiOutput("day"),
#     uiOutput("sess"),
    HTML('<!-- AddThisEvent -->
<script type="text/javascript" src="http://js.addthisevent.com/atemay.js"></script>'),
    selectInput("drop_day", "Day:",
                choices= day, selected=day[1]),
    selectInput("drop_time", "Time:",
                choices= time, selected=time[1]),   
    selectInput("drop_sess", "Session:",
                choices= sess, selected=sess[1]),  
    textInput("search", "Search Criteria (uses grep):", ""),
    downloadButton("dlcsv", "Download CSV of Table"),
    HTML('<br>If you like it donate a beer at ENAR or PayPal:
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=muschellij2%40gmail%2ecom&lc=US&item_name=ENAR%20Scraper&item_number=enar_scraper2014&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donate_LG%2egif%3aNonHosted"><img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!">
<img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1"></a>
')
  ),
  mainPanel(
    tableOutput("outtab")
  )
))