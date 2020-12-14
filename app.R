#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(quantmod)
library(lubridate)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Efficient Portfolio Plan Based on Modern Portfolio Theory"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            
            textInput("stock1", "Enter Stock 1", 
                      value = "AAPL"),
            textInput("stock2", "Enter Stock 2", 
                      value = "AMZN"),
            textInput("stock3", "Enter Stock 3", 
                      value = "GOOG"),
            textInput("stock4", "Enter Stock 4", 
                      value = "NIO"),
            textInput("stock5", "Enter Stock 5", 
                      value = "TSLA")
        ),
        
        # Show the data and information that fits your choices
        mainPanel(
            textOutput("ps1"),
            tableOutput("weight"),
            textOutput("ps2"),
            tableOutput("return"),
            textOutput("ps3"),
            plotOutput("view"),
            textOutput("ps4"),
            plotOutput("series1"),
            plotOutput("series2"),
            plotOutput("series3"),
            plotOutput("series4"),
            plotOutput("series5"),
        )
    )
)





# Define server
server <- function(input, output) {
    
    # Algorithm
    
    output$ps1<- renderText({"An initial position of $250,000 in cash starting on 1st July 2020 is used to buy for this portofolio. The portfolio plan is based on the Modern Portfolio Theroy by Harry Markowitz. The following table shows the optimized plan for the weighted proportion of distribution of position on each stock. Negative weights means short selling."})
    
    
    output$weight<- renderTable({
    
        
        
        a <- getSymbols(c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        
        stock1<- get(input$stock1)
        stock2<- get(input$stock2)
        stock3<- get(input$stock3)
        stock4<- get(input$stock4)
        stock5<- get(input$stock5)
        model<- data.frame(cbind(stock1[1:42,6],stock2[1:42,6],stock3[1:42,6],stock4[1:42,6],stock5[1:42,6]))
        invest<- data.frame(cbind(stock1[43:148,6],stock2[43:148,6],stock3[43:148,6],stock4[43:148,6],stock5[43:148,6]))
        
        return<- data.frame(matrix(data=NA,ncol=5,nrow=41))
        return_std<- data.frame(matrix(data=NA,ncol=5,nrow=41))
        for (i in 1:41){
            return[i,1]<- model[i+1,1]/model[i,1]-1
            return[i,2]<- model[i+1,2]/model[i,2]-1
            return[i,3]<- model[i+1,3]/model[i,3]-1
            return[i,4]<- model[i+1,4]/model[i,4]-1
            return[i,5]<- model[i+1,5]/model[i,5]-1
        }
        for (i in 1:5){
            return_std[,i]<- return[,i]-mean(return[,i])/sqrt(40)
        }
        covar<- cov(return_std)
        inverse<- solve(covar)
        r<- as.matrix(c(mean(return$X1),mean(return$X2),mean(return$X3),mean(return$X4),mean(return$X5)))
        invcovr<- inverse %*% r
        A<- sum(inverse)
        B<- sum(invcovr)
        C<- t(r) %*% invcovr
        D<- A*C-B*B
        # risk free rate in US is 0.2
        rf<- matrix(0.2, ncol=1, nrow=5)
        weight<- data.frame(weighted=inverse %*% (r-rf))
        weight_std<- data.frame(weight_std=matrix(NA,ncol=1, nrow=5))
        for (i in 1:5){
            weight_std[i,]<- weight[i,]/(B-0.2*A)
        }
        result<- as.matrix(data.frame(rbind(invest[1,], invest[106,], invest[106,]/invest[1,])))
        weight_std2<- as.matrix(weight_std)
        profit<- crossprod(result[3,], weight_std2)
        
        frame_weight<- data.frame(STOCK_1=weight_std[1,], STOCK_2=weight_std[2,], STOCK_3=weight_std[3,], STOCK_4=weight_std[4,], STOCK5=weight_std[5,])
        
        everyday<- return
        
        for (i in 1:105){
            for (j in 1:5){
                everyday[i,j]<- invest[i+1,j]/invest[1,j]*frame_weight[1,j]
                everyday[i,6]<- everyday$X1[i]+everyday$X2[i]+everyday$X3[i]+everyday$X4[i]+everyday$X5[i]
                everyday[i,7]<- ymd(rownames(invest[i+1,]))
            }
        }
        
        everyday$updown<- "up"
        
        for (i in 1:104){
            if (everyday$V6[i+1] < everyday$V6[i]){
                everyday$updown[i+1]<- "down"
            }
        }
        
        
        frame_weight
    })
    
    output$ps2<- renderText({"Cash Returned in USD After Transaction on 1st Dec 2020"})
     
    output$return<- renderTable({ 
        
    t <- getSymbols(c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5), from = '2020-05-01',
                                                  to = "2020-12-01",warnings = FALSE,
                                                  auto.assign = TRUE)
    
    stock1<- get(input$stock1)
    stock2<- get(input$stock2)
    stock3<- get(input$stock3)
    stock4<- get(input$stock4)
    stock5<- get(input$stock5)
    model<- data.frame(cbind(stock1[1:42,6],stock2[1:42,6],stock3[1:42,6],stock4[1:42,6],stock5[1:42,6]))
    invest<- data.frame(cbind(stock1[43:148,6],stock2[43:148,6],stock3[43:148,6],stock4[43:148,6],stock5[43:148,6]))
    
    return<- data.frame(matrix(data=NA,ncol=5,nrow=41))
    return_std<- data.frame(matrix(data=NA,ncol=5,nrow=41))
    for (i in 1:41){
        return[i,1]<- model[i+1,1]/model[i,1]-1
        return[i,2]<- model[i+1,2]/model[i,2]-1
        return[i,3]<- model[i+1,3]/model[i,3]-1
        return[i,4]<- model[i+1,4]/model[i,4]-1
        return[i,5]<- model[i+1,5]/model[i,5]-1
    }
    for (i in 1:5){
        return_std[,i]<- return[,i]-mean(return[,i])/sqrt(40)
    }
    covar<- cov(return_std)
    inverse<- solve(covar)
    r<- as.matrix(c(mean(return$X1),mean(return$X2),mean(return$X3),mean(return$X4),mean(return$X5)))
    invcovr<- inverse %*% r
    A<- sum(inverse)
    B<- sum(invcovr)
    C<- t(r) %*% invcovr
    D<- A*C-B*B
    # risk free rate in US is 0.2
    rf<- matrix(0.2, ncol=1, nrow=5)
    weight<- data.frame(weighted=inverse %*% (r-rf))
    weight_std<- data.frame(weight_std=matrix(NA,ncol=1, nrow=5))
    for (i in 1:5){
        weight_std[i,]<- weight[i,]/(B-0.2*A)
    }
    result<- as.matrix(data.frame(rbind(invest[1,], invest[106,], invest[106,]/invest[1,])))
    weight_std2<- as.matrix(weight_std)
    profit<- crossprod(result[3,], weight_std2)
    
    frame_weight<- data.frame(STOCK_1=weight_std[1,], STOCK_2=weight_std[2,], STOCK_3=weight_std[3,], STOCK_4=weight_std[4,], STOCK5=weight_std[5,])
    
    everyday<- return
    
    for (i in 1:105){
        for (j in 1:5){
            everyday[i,j]<- invest[i+1,j]/invest[1,j]*frame_weight[1,j]
            everyday[i,6]<- everyday$X1[i]+everyday$X2[i]+everyday$X3[i]+everyday$X4[i]+everyday$X5[i]
            everyday[i,7]<- ymd(rownames(invest[i+1,]))
        }
    }
    
    everyday$updown<- "up"
    
    for (i in 1:104){
        if (everyday$V6[i+1] < everyday$V6[i]){
            everyday$updown[i+1]<- "down"
        }
    }
    
    qqq<- data.frame(BalanceUSD=everyday$V6[105]*250000)
    qqq   
    })
   
    output$ps3<- renderText({"Track the trend of protofolio holding from 1st July 2020 to 1st Dec 2020. The baseline is the initial position of $250000 on 1st July 2020."})
    
    output$view <- renderPlot({
        
        
        g <- getSymbols(c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        
        stock1<- get(input$stock1)
        stock2<- get(input$stock2)
        stock3<- get(input$stock3)
        stock4<- get(input$stock4)
        stock5<- get(input$stock5)
        model<- data.frame(cbind(stock1[1:42,6],stock2[1:42,6],stock3[1:42,6],stock4[1:42,6],stock5[1:42,6]))
        invest<- data.frame(cbind(stock1[43:148,6],stock2[43:148,6],stock3[43:148,6],stock4[43:148,6],stock5[43:148,6]))
        
        return<- data.frame(matrix(data=NA,ncol=5,nrow=41))
        return_std<- data.frame(matrix(data=NA,ncol=5,nrow=41))
        for (i in 1:41){
            return[i,1]<- model[i+1,1]/model[i,1]-1
            return[i,2]<- model[i+1,2]/model[i,2]-1
            return[i,3]<- model[i+1,3]/model[i,3]-1
            return[i,4]<- model[i+1,4]/model[i,4]-1
            return[i,5]<- model[i+1,5]/model[i,5]-1
        }
        for (i in 1:5){
            return_std[,i]<- return[,i]-mean(return[,i])/sqrt(40)
        }
        covar<- cov(return_std)
        inverse<- solve(covar)
        r<- as.matrix(c(mean(return$X1),mean(return$X2),mean(return$X3),mean(return$X4),mean(return$X5)))
        invcovr<- inverse %*% r
        A<- sum(inverse)
        B<- sum(invcovr)
        C<- t(r) %*% invcovr
        D<- A*C-B*B
        # risk free rate in US is 0.2
        rf<- matrix(0.2, ncol=1, nrow=5)
        weight<- data.frame(weighted=inverse %*% (r-rf))
        weight_std<- data.frame(weight_std=matrix(NA,ncol=1, nrow=5))
        for (i in 1:5){
            weight_std[i,]<- weight[i,]/(B-0.2*A)
        }
        result<- as.matrix(data.frame(rbind(invest[1,], invest[106,], invest[106,]/invest[1,])))
        weight_std2<- as.matrix(weight_std)
        profit<- crossprod(result[3,], weight_std2)
        
        frame_weight<- data.frame(STOCK_1=weight_std[1,], STOCK_2=weight_std[2,], STOCK_3=weight_std[3,], STOCK_4=weight_std[4,], STOCK5=weight_std[5,])
        
        everyday<- return
        
        for (i in 1:105){
            for (j in 1:5){
                everyday[i,j]<- invest[i+1,j]/invest[1,j]*frame_weight[1,j]
                everyday[i,6]<- everyday$X1[i]+everyday$X2[i]+everyday$X3[i]+everyday$X4[i]+everyday$X5[i]
                everyday[i,7]<- ymd(rownames(invest[i+1,]))
            }
        }
        
        everyday$updown<- "up"
        
        for (i in 1:104){
            if (everyday$V6[i+1] < everyday$V6[i]){
                everyday$updown[i+1]<- "down"
            }
        }
        
        ggplot(data=everyday, mapping=aes(x=V7,y=V6*250000,group=1))+
            geom_point(aes(color=everyday$updown), legend.title="up or down")+
            scale_color_manual(breaks=c("up", "down"), values=c("red2","green3"))+
            geom_line(aes(x=V7, y=V6*250000))+
            labs(x="Month", y="Balance in USD")
    })
    
    output$ps4<- renderText({"The following 5 candlestick chart shows the detailed trend of the 5 stocks we have chosen. Each chart includes data from 1st May to 30th June, which we used for calculating the weights, and from 1st July to 1st Dec, which is for exercising."})
    
    output$series1 <- renderPlot({
        b <- getSymbols(c(input$stock1), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        chartSeries(get(input$stock1), theme=chartTheme('white'))
    })
    output$series2 <- renderPlot({
        c <- getSymbols(c(input$stock2), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        chartSeries(get(input$stock2), theme=chartTheme('white'))
    })
    output$series3 <- renderPlot({
        d <- getSymbols(c(input$stock3), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        chartSeries(get(input$stock3), theme=chartTheme('white'))
    })
    output$series4 <- renderPlot({
        e <- getSymbols(c(input$stock4), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        chartSeries(get(input$stock4), theme=chartTheme('white'))
    })
    output$series5 <- renderPlot({
        f <- getSymbols(c(input$stock5), from = '2020-05-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = TRUE)
        chartSeries(get(input$stock5), theme=chartTheme('white'))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)