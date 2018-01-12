rm(list=ls(all=T))
getwd()
setwd("C:\\Users\\aksha\\Desktop")
data<-read.csv("CAVS_DATA.csv")
cavs_points<-data[,c("PLAYER","PTS","OPPONENT")]
cavs_Ast<-data[,c("PLAYER","AST","OPPONENT")]
cavs_min<-data[,c("PLAYER","MIN","OPPONENT")]
cavs_Stl<-data[,c("PLAYER","STL","OPPONENT")]
cavs_rebounds<-data[,c("PLAYER","REB","OPPONENT")]
cavs_blocks<-data[,c("PLAYER","BLKS","OPPONENT")]
cavs_3pm<-data[,c("PLAYER","X3PM","OPPONENT")]
cavs_FT<-data[,c("PLAYER","FT.","OPPONENT")]
cavs2<-cbind(cavs_points,cavs_Ast$AST,cavs_min$MIN,cavs_Stl$STL,cavs_rebounds$REB,cavs_blocks$BLKS,cavs_3pm$X3PM,cavs_FT$FT.) 
names(cavs2)<-c('Player','Points',"OPPONENT",'Assists',"Minutes","Steals","Rebounds","Blocks","3points","Freethrows")
names(cavs2)
####################################################################################
#Averages
#eachplayerpoints<-cavs2%>%group_by(cavs2$Player)%>%summarise(median=median(cavs2$Points))
points<-aggregate(cavs2$Points, by=list(Category=cavs2$Player), FUN=sum)
Assists<-aggregate(cavs2$Assists, by=list(Category=cavs2$Player), FUN=sum)
Minutes<-aggregate(cavs2$Minutes, by=list(Category=cavs2$Player), FUN=sum)
Steals<-aggregate(cavs2$Steals, by=list(Category=cavs2$Player), FUN=sum)
Rebounds<-aggregate(cavs2$Rebounds, by=list(Category=cavs2$Player), FUN=sum)
Blocks<-aggregate(cavs2$Blocks, by=list(Category=cavs2$Player), FUN=sum)
Threep<-aggregate(cavs2$`3points`, by=list(Category=cavs2$Player), FUN=sum)
ft<-aggregate(cavs2$Freethrows, by=list(Category=cavs2$Player), FUN=sum)

cavs3<-cbind(points,Assists$x,Minutes$x,Steals$x,Rebounds$x,Blocks$x,Threep$x,ft$x)

names(cavs3)<-c('Player','Points',"Assists","Minutes","Steals","Rebounds","Blocks","3points","Freethrows")
names(cavs3)

####################################################################################
#plot_ly(cavs2, y = cavs2$Points, color = cavs2$Player, type = "box")

#newdata<-split(data,data$PLAYER)
#newdata<-as.data.frame(newdata)
#names(newdata)
#AZ<-newdata[,1:27]
#m1<-t(newdata)
#d2 <- data.frame(r1=row.names(m1), m1)

library(shiny)
library(plotly)
library(shinydashboard)
library(heatmaply)
library(rsconnect)
library(ggplot2)

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "CAVS_NBA2K18 STATS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cavsboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Heatmapboard", tabName = "dashboard2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
          fluidRow(
            box(width = 8,
            plotlyOutput("plot1")),
            box(width = 4,
              plotlyOutput("plot2"))
        ),
        fluidRow(
          column(width = 5,
        box(
          selectInput("cavs","choose",c('Points','Assists',"Minutes","Steals","Rebounds","Blocks","3points","Freethrows"))
          )),
        valueBox( 35,width=2 ,"Total games played",icon = icon("futbol-o"),color ="light-blue" ),
        valueBox( 24,width=2 ,"wins",icon = icon("sort"),color="green"),
        valueBox( 11,width =2, "Loss",icon = icon("sort"),color="red")
               
                       )
          ),
      tabItem(tabName = "dashboard2",
              h2(
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput("plot3")
                )
              )
              )
              )
            
          )
               )
)
  

server <- function(input, output) {
  output$plot1<-renderPlotly({
    p <- ggplot(cavs2, aes(cavs2$Player, cavs2[,input$cavs], fill = cavs2$Player)) +
      geom_boxplot()+
      ggtitle("cavaliers performance")+labs(x="cavaliers",y=input$cavs)+scale_x_discrete(drop=FALSE)+
      theme(axis.title.x = element_text(face="bold", colour="#990000"),
           axis.text.x  = element_text(angle=90))
        p <- ggplotly(p)
    
  }
      )
  output$plot2<-renderPlotly({
    p<-plot_ly(cavs3,labels=cavs3$Player,values=cavs3[,input$cavs],type = 'pie')%>%
      layout(title = 'Cavaliers pie chart ',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  }
  
  )
  output$plot3<-renderPlotly({
    heatmaply(cavs2,k_col = 2,k_row = 3)
    
  }
    
  )
 
  
}
  
  
  
shinyApp(ui,server)
#library(devtools)


rsconnect::setAccountInfo(name='beaversland', token='2964E892E338A57B940C5D5C5578006E', secret='VXqG9In0iooNjyRNJMG2m0NMJ8hzKSA9g0OJYY1o')
rsconnect::setAccountInfo(name='beaversland', token='2964E892E338A57B940C5D5C5578006E', secret='VXqG9In0iooNjyRNJMG2m0NMJ8hzKSA9g0OJYY1o')
deployApp()
warnings()
###########################################################################################################
gethelp.df =tryCatch(htmlTreeParse(gethelp.url, useInternalNodes = T), error = function(cond) next)
devtools::install_github('hadley/devtools')

fmt <- "Package '%s' is missing the GithubSHA1 field and needs to be reinstalled."
pkgDirs <- list.files(.libPaths(), full.names = TRUE)
descPaths <- file.path(pkgDirs, "DESCRIPTION")
pkgInfo <- lapply(descPaths, function(path) {
  tryCatch({
    pkg <- basename(dirname(path))
    contents <- readChar(path, file.info(path)$size, TRUE)
    Encoding(contents) <- "UTF-8"
    if (grepl("GithubRepo", contents) && !grepl("GithubSHA1", contents)) {
      cat(sprintf(fmt, pkg), sep = "\n")
      return(pkg)
    }
    NULL
  }, error = function(e) NULL)
})

needsReinstall <- as.character(Filter(Negate(is.null), pkgInfo))
if (length(needsReinstall) == 0) {
  cat("All packages appear up to date -- no action necessary.\n")
} else {
  cat("One or more packages need to be updated.\n")
}
devtools::install_github("rstudio/shinyapps")
