#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Load the libraries in R 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rmarkdown)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    # You can access the value of the widget with input$file, e.g.
    
    
    output$contents <- renderTable({
        
        req(input$file1)
        
        df <- read.csv(file=input$file1$datapath,
                       header = TRUE,
                       sep = ","
        )
        df
    })
    output$distPlot <- renderPlot({
        req(input$file1)
        
        df <- read.csv(file=input$file1$datapath,
                       header = TRUE,
                       sep = ","
        )
        
        
        df<- df[grepl(paste(input$States, collapse = "|"), df$State),]
        
        
        #Q1 breweries per state
        #Group the data by state
        brewbystate<- data.frame(df %>% group_by(State) %>% tally() %>% arrange(desc(n)))
        
        #Plot the data using GGPlot
        brewbystateplot<-brewbystate %>% ggplot(aes(x = reorder(as.factor(State),-n), y = n)) + 
            labs(y="# of Breweries", x = "State") + 
            geom_bar(stat = "identity", position = "dodge", fill = "goldenrod3") + 
            geom_label(size = 3, label = brewbystate$n, vjust = 1) + 
            ggtitle('Breweries by State') + theme(plot.title = element_text(hjust = .5))
        
        
        
        #summary statistics and distribution of ABV
        abvsum<-summary(df$Percent_ABV)
        
        
        #get data from the summary
        abvsum<-data.frame(table(abvsum))
        abvsum$stats[1]<- "Min"
        abvsum$stats[2]<- "1st Q"
        abvsum$stats[3]<- "Med"
        abvsum$stats[4]<- "Mean"
        abvsum$stats[5]<- "3rd Q"
        abvsum$stats[6]<- "Max"
        abvsum<-subset(abvsum, select = -c(Freq))
        abvsum$abvsum<-round((as.numeric(as.character(abvsum$abvsum))), digits = 2)
        abvsumstatsstr<-as.numeric(unlist(strsplit(toString(abvsum[1:6,1]), ", ")))
        
        scatterlm <- df %>% ggplot(aes(x= IBU, y = Percent_ABV, color = Percent_ABV)) + 
            geom_point() + 
            ylim(2.5, 13) + 
            theme_dark() + 
            stat_smooth(method = "lm", color = "red") + 
            labs(title = "% ABV vs IBU") + 
            theme(plot.title = element_text(hjust = .5)) +
            scale_color_gradient(low = "gold2", high = "white") 
        
        scatternolm <- df %>% ggplot(aes(x= IBU, y = Percent_ABV, color = Percent_ABV)) + 
            geom_point() + 
            ylim(2.5, 13) + 
            theme_dark() + 
            labs(title = "% ABV vs IBU") + 
            theme(plot.title = element_text(hjust = .5)) +
            scale_color_gradient(low = "gold2", high = "white")
        
        
        
        abvplotdens<- df %>% ggplot(aes(x=Percent_ABV)) + 
            geom_density(aes(color = "red", size = 1)) + 
            geom_histogram(aes(y=..density.., alpha = .2)) + 
            labs(x = "% ABV", title = " Distribution of ABV Percentages", y = "Density") + 
            theme(plot.title = element_text(hjust = .5), legend.position="none") + 
            annotate("text", x = c(abvsumstatsstr[1],abvsumstatsstr[2], abvsumstatsstr[3]-.2, abvsumstatsstr[4]+.2, abvsumstatsstr[5]+.2, abvsumstatsstr[6]), y = .6, label = abvsumstatsstr)+ 
            annotate("text", x = c(abvsumstatsstr[1],abvsumstatsstr[2], abvsumstatsstr[3]-.2, abvsumstatsstr[4]+.2, abvsumstatsstr[5]+.2, abvsumstatsstr[6]), y = .63, label = abvsum$stats )+
         ylim(0,.63) + xlim(abvsumstatsstr[1],abvsumstatsstr[6])
        
        abvplotbox<- df %>% ggplot(aes(x=Percent_ABV)) + 
            geom_boxplot() + 
            labs(x = "% ABV", title = " Distribution of ABV Percentages") + 
            theme(plot.title = element_text(hjust = .5), legend.position="none", axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
            annotate("text", x = c(abvsumstatsstr[1],abvsumstatsstr[2], abvsumstatsstr[3]-.2, abvsumstatsstr[4]+.2, abvsumstatsstr[5]+.2, abvsumstatsstr[6]), y = .6, label = abvsumstatsstr)+ 
            annotate("text", x = c(abvsumstatsstr[1],abvsumstatsstr[2], abvsumstatsstr[3]-.2, abvsumstatsstr[4]+.2, abvsumstatsstr[5]+.2, abvsumstatsstr[6]), y = .63, label = abvsum$stats )+
            xlim(abvsumstatsstr[1],abvsumstatsstr[6])+
            geom_point(aes(x=abvsum$abvsum[4], y=0), colour="red")
        
        #summary statistics and distribution of IBU with NA values removed
        
        ibudf<-df[which(!is.na(df$IBU)),]
        
        ibusum<-summary(ibudf$IBU)
        
        
        #get data from the summary
        ibusum<-data.frame(table(ibusum))
        ibusum$stats[1]<- "Min"
        ibusum$stats[2]<- "1st Q"
        ibusum$stats[3]<- "Med"
        ibusum$stats[4]<- "Mean"
        ibusum$stats[5]<- "3rd Q"
        ibusum$stats[6]<- "Max"
        ibusum<-subset(ibusum, select = -c(Freq))
        ibusum$ibusum<-round((as.numeric(as.character(ibusum$ibusum))), digits = 2)
        ibusumstatsstr<-as.numeric(unlist(strsplit(toString(ibusum[1:6,1]), ", ")))
        
        
        
        ibuplotdens<- ibudf %>% ggplot(aes(x=IBU)) + 
            geom_density(aes(color = "red", size = 1)) + 
            geom_histogram(aes(y=..density.., alpha = .2)) + 
            labs(x = "IBU", title = " Distribution of IBU", y = "Density") + 
            theme(plot.title = element_text(hjust = .5), legend.position="none") + 
            annotate("text", x = c(ibusumstatsstr[1],ibusumstatsstr[2], ibusumstatsstr[3]-.2, ibusumstatsstr[4]+.2, ibusumstatsstr[5]+.2, ibusumstatsstr[6]), y = .2, label = ibusumstatsstr)+ 
            annotate("text", x = c(ibusumstatsstr[1],ibusumstatsstr[2], ibusumstatsstr[3]-.2, ibusumstatsstr[4]+.2, ibusumstatsstr[5]+.2, ibusumstatsstr[6]), y = .21, label = ibusum$stats )+
            ylim(0,.23) + xlim(ibusumstatsstr[1],ibusumstatsstr[6])
        
        ibuplotbox<- ibudf %>% ggplot(aes(x=IBU)) + 
            geom_boxplot() + 
            labs(x = "IBU", title = " Distribution of IBU Percentages") + 
            theme(plot.title = element_text(hjust = .5), legend.position="none", axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
            annotate("text", x = c(ibusumstatsstr[1],ibusumstatsstr[2], ibusumstatsstr[3]-.2, ibusumstatsstr[4]+.2, ibusumstatsstr[5]+.2, ibusumstatsstr[6]), y = .6, label = ibusumstatsstr)+ 
            annotate("text", x = c(ibusumstatsstr[1],ibusumstatsstr[2], ibusumstatsstr[3]-.2, ibusumstatsstr[4]+.2, ibusumstatsstr[5]+.2, ibusumstatsstr[6]), y = .63, label = ibusum$stats )+
            xlim(ibusumstatsstr[1],ibusumstatsstr[6]) +
            geom_point(aes(x=ibusum$ibusum[4], y=0), colour="red")
        
        
        
        
        if(!is.null(df)){
            
            if(input$plot == "ABV Histogram"){abvplotdens}
            else if (input$plot == "ABV Boxplot"){abvplotbox}
            else if (input$plot == "IBU Histogram"){ibuplotdens}
            else if (input$plot == "IBU Boxplot"){ibuplotbox}
            else if (input$plot == "ScatterPlot of % ABV vs IBU" & input$regline == "Yes"){scatterlm}
            else if (input$plot == "ScatterPlot of % ABV vs IBU" & input$regline == "No"){scatternolm}
            else if (input$plot == "Qty of Breweries"){brewbystateplot}
        }
        
        
    })
    
    
})