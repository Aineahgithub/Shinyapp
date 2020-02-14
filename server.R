#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(prophet)
library(shiny)
library(forecast)
library(ggplot2)
library(ggfortify)
library(ggplot2)
library(highcharter)
library(zoo)
library(tseries)
#library(data.world)
library(rtweet)
library(quantmod)
library(RMySQL)
library(shiny)
library(RJSONIO)
library(crypto)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(stringr)
library(tidyverse)
library(tidyquant)
library(timeDate)
library(timetk)
library(DT)
library(e1071)
library(broom)
library(lattice)
library(r2d3)
library(caret)
library(class)
library(shiny.i18n)
library(shinythemes)
library(nycflights13)
library(gapminder)
library(corrplot)
library(gridExtra)
library(randomForest)
data.analys <-  data.frame(mtcars)
data.analys$am <- factor(data.analys$am)
#ana <- ifelse(data.analys$am== "0","Automatic", "Manuel")
data.analys$vs <- factor(data.analys$vs)
data.analys$gear <- factor(data.analys$gear)
data.analysis <- data.frame(data.analys)

dataset <-
    c("trainingset",
      "testingset",
      "forecast",
      "rsqr",
      "fitparameter",
      "predicted")
theme <-
    c(
        "flat",
        "f538",
        "chalk",
        "db",
        "economist",
        "elementary",
        "ffx",
        "ggplot2",
        "gridlight",
        "handrawn",
        "merge",
        "monokai",
        "sandsignika",
        "smpl",
        "sparklike",
        "superheroes",
        "tufte",
        "tufte2"
    )
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

    # data("mtcars")
    # colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])
    # mtcars$am <- factor(mtcars$am,levels=c(0,1),
    #                     labels=c("Automatic","Manual"))
    # output$boxplot1 <- renderPlot({
    #     ggplot(mtcars, aes(x = am, y = mpg)) +
    #         geom_boxplot(fill = semantic_palette[["green"]]) + 
    #         xlab("gearbox") + ylab("Miles per gallon")
    # })
    # 
    # output$dotplot1 <- renderPlotly({
    #     ggplotly(ggplot(mtcars, aes(wt, mpg))
    #              + geom_point(aes(colour=factor(cyl), size = qsec))
    #              + scale_colour_manual(values = colscale)
    #     )
   # })
    df_ldata <- reactive({
        req(input$file1)
        
        df <- read.table(
            input$file1$datapath,
            header = input$header,
            sep = input$sep,
            #quote = input$quote,
            comment.char = "#",
            row.names = NULL
        )
        df <- data.frame(df)
        
    })
    output$selecty <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        
        selectInput('Y', 'Choose Y value',
                    c(names(df_ldata()[sapply(df_ldata(), is.numeric)])))
        # checkboxInput('jitter', 'Jitter')
        # checkboxInput('smooth', 'Smooth')
        
        
    })
    output$selectivefile <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        
        
        selectInput('element', 'Choose X value',
                    c(names(df_ldata()[sapply(df_ldata(), is.numeric)])))
        #checkboxInput('boxplot', 'boxplot')
        #checkboxInput('violin', 'violin')
        
        
    })
    theme_tsc  <- reactive({
        switch(
            input$themes,
            "flat" = hc_theme_flat(),
            "darkunica" = hc_theme_darkunica(),
            "f538" =  hc_theme_538(),
            "chalk" = hc_theme_chalk(),
            "db" =  hc_theme_db(),
            "economist" =  hc_theme_economist(),
            "ffx" = hc_theme_ffx(),
            "ggplot2" = hc_theme_ggplot2(),
            "gridlight" = hc_theme_gridlight(),
            "handrawn" =  hc_theme_handdrawn(),
            "monokai" = hc_theme_monokai(),
            "elementary" =   hc_theme_elementary(),
            "smpl" = hc_theme_smpl(),
            "sparklike" =  hc_theme_sparkline(),
            "superheroes" = hc_theme_superheroes(),
            "tufte" =   hc_theme_tufte(),
            "tufte2" = hc_theme_tufte2(),
            "sandsignika" = hc_theme_sandsignika(),
            "google" = hc_theme_google()
        )
        
    })
    output$filedata <- renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        if (is.null(input$file1))
            return(gapminder)
        else {
            # req(input$file1)
            #
            # df <- read.table(input$file1$datapath,
            #                header = input$header,
            #                stringsAsFactors= input$stringsAsFactors,
            #                sep = input$sep,
            #                quote = input$quote)
            df <- df_ldata()
            
            if (input$disp == "head") {
                return(head(df))
            }
            else {
                return(df)
            }
            
        }
        
    })
    output$summer <- renderDataTable({
        if (is.null(input$file1))
            return(summary(gapminder))
        
        if (is.null(df_fcastcustom())) {
            return()
        }
        else{
            # req(input$file1)
            #
            # df <- read.table(input$file1$datapath,
            #                header = input$header,
            #                sep = input$sep,
            #                quote = input$quote)
            # df <- data.frame(df)
            #                quote = input$quote)
            df <- df_ldata()
            
            #df1 <- data.frame(select_if(df, is.numeric))
            
            if (input$disp == "head") {
                df1 <- data.frame(df)
                return(summary(head(df1)))
            }
            else {
                return(summary(df))
            }
            
        }
        
    })
    df_lfocast <- reactive({
        df <- df_ldata()
        dfn <-  df %>%
            select_if(is.numeric)
        
        df1 <- dfn[[input$element]] %>%
            data.frame() %>%
            #round(2) %>%
            mutate("date" = seq.Date(as.Date(Sys.Date() - nrow(df)), as.Date((Sys.Date(
            ) - 1)), by = "day")) %>%
            # rename("Forecast"=.)
            as.character.Date() %>%
            arrange(date)
        names(df1)[1] <- "Forecasted"
        df1
    })
    df_tssl <- reactive({
        #
        df <- df_lfocast() %>%
            select(Forecasted)
        start = Sys.Date() - nrow(df)
        # df1 <- df_tsslwm() %>%
        #   select(Forecasted)
        
        df %>%
            ts(frequency = 365.25, start = decimal_date(ymd(start)))
        
    })
    lambda_tssl  <- reactive({
        lambda <- BoxCox.lambda(df_tssl())
        lambda
    })
    lambda_fb  <- reactive({
      lam <-  BoxCox.lambda(df_lfocast()$Forecasted, method = "loglik")
      df_lfocast() <- BoxCox(df_lfocast(), lam)
      df_lfocast() 
    })
#     prophet_fb   <- reactive({
#       df <- prophet(lambda_fb())
#       
#       future <- make_future_dataframe(df,  periods = 365)
#       future
#     })
#     prophet_m   <- reactive({
#       df <- prophet(lambda_fb())
#       
# df
#     })
    
    # pred_prophet <- reactive({
    #   forecast <- predict(prophet_m(), prophet_fb())
    #   forecast
    # })
    fit_local <- reactive({
      if(is.null(input$all)){
        switch(
            input$choice,
            "One" = tbats(df_tssl(), biasadj = TRUE),
            "Two" = tbats(df_tssl(), lambda = lambda_tssl(), biasadj = TRUE),
            "Three" = tbats(df_tssl(), lambda = 0, biasadj = TRUE),
            "Four" = auto.arima(df_tssl(), lambda = 0 , biasadj = TRUE),
            "Five" = auto.arima(df_tssl(), biasadj = TRUE),
            "Six" = ets(df_tssl(), lambda = 0, biasadj = TRUE),
            "Seven" = ets(df_tssl(), lambda = lambda_tssl(), biasadj = TRUE)
        )
      }
    })
    df_fcastcustom <- reactive({
        f2 <-  forecast(fit_local(), h = input$h)
        df <-  data.frame(f2)
        # as.Date((Sys.Date()-2)+input$h)
        
        df1 <- df[[1]] %>%
            data.frame() %>%
            round(2) %>%
            mutate("date" = seq.Date(
                as.Date(Sys.Date() - 1),
                by = "day",
                length.out = nrow(df)
            ))# %>%
        # rename("Forecast"=.)
        #as.character.Date()
        names(df1)[1] <- "Forecast"
        df1
        #as.character.Date()
        
        
        
        ##0f1941
    })
    output$prophet <- renderDataTable({
      if (is.null(input$file1))
        return()

      if (is.null(df_fcastcustom())) {
        return()
      }
      else{
        # req(input$file1)
        #
        # df <- read.table(input$file1$datapath,
        #                header = input$header,
        #                sep = input$sep,
        #                quote = input$quote)
        # df <- data.frame(df)
        #                quote = input$quote)
        lambda_fb()

        #df1 <- data.frame(select_if(df, is.numeric))


      }

    })
    Classi_data <- reactive({
        #
        if (is.null(input$filec)) {
          #df <- data.analysis
         return (
           data.analysis )
            # df_ldata()[sapply(df_ldata(), is.numeric)]
        }
        
            df <- read.csv(
                input$filec$datapath,
                header = input$header1,
                sep = input$sep1
               # stringsAsFactors = input$stringsAsFactors1,
                #quote = input$quote,
                #comment.char = "#",
                #row.names = FALSE
            )
            df <- data.frame(df) %>%
                na.omit()
            na.omit(df)
        df
        
        
    })
    
    Classification_ML  <- reactive({
        set.seed(2)
        #
        # if (is.null(input$file1)){
        #
        #   return() # df_ldata()[sapply(df_ldata(), is.numeric)]
        # }
        #df  <- Classi_data()[,!grepl(input$targetv, names(Classi_data()))]
        # Data <- df %>% mutate_if(is.numeric, funs(as.numeric(scale(.))))
        #  Data <- Classi_data()[,!grepl(input$targetv, names(Classi_data()))]
        #
        # id <- sample(2,nrow(Data), p=c(1,0), replace = T)
        # Training_set <- Data[id==1,]
        # Test_set <- Data[id==2,]
        trctrl <-
            trainControl(method = "repeatedcv",
                         number = 10 ,
                         repeats = 3)
        metric <- "accuracy"
        Data <- training()[, !grepl(input$targetv, names(training()))]
        
        df2 <-  switch(
            input$knni,
            "naive" = naiveBayes(training()[[input$targetv]] ~
                                     ., data = training()),
            "randomforest" = randomForest(
                training()[[input$targetv]] ~ .,
                data = Data,
                mtry = 3,
                importance = TRUE
            )
            # "svm" = train(training()[[input$targetv]]~., data =training() , method = "svmLinear",
            #               trControl = trctrl, metric= metric)
            #
            
        )
        df2
        
  
    })
    
    testing <- reactive({
        set.seed(2)
        Data <-
            Classi_data()#[,!grepl(input$targetv, names(Classi_data()))]
        
        id <- sample(2, nrow(Data), p = c(0.8, 0.2), replace = T)
        Training_set <- Data[id == 2, ]
        
        df2 <-  switch(input$traintest,
                       # "multiple" = lm(df_ldata()[[input$dependent]]~df_ldata()[[input$element]]+df_ldata()[[input$explanatory]], data =df_ldata()[sapply(df_ldata(), is.numeric)], method = "ranger",
                       #                 importance = "impurity") ,
                       "Training" =  training(),
                       
                       "Testing" = Training_set)
        df2
    })
    output$freq <- renderUI({
        if (is.null(input$file1)) {
            return()
                
            
        }
        
        prettyRadioButtons(
            inputId = "frequency",
            label = "",
            choices = c("day", "week", "months"),
            outline = TRUE,
            selected = "day",
            plain = TRUE,
            icon = icon("thumbs-up")
        )
        
        
    })
    output$regfreq <- renderUI({
      if (is.null(input$file1)) {
        return()
        
        
      }
      
      prettyRadioButtons(
        inputId = "freqreg",
        label = "",
        choices = c("day", "week", "months"),
        outline = TRUE,
        selected = "day",
        plain = TRUE,
        icon = icon("thumbs-up")
      )
      
      
    })
    
    
    output$datat  <- renderUI({
        # if(is.null(input$filec)){
        #   return()}
        dataTableOutput("uitable")
        
        
    })
    
    output$numerici <- renderUI({
        if (is.null(input$filec)) {
            return()
        }
        
        selectInput('numerisch', 'Choose numerical column',
                    c(names(Classi_data()[sapply( Classi_data(), is.numeric)])))
        # checkboxInput('jitter', 'Jitter')
        # checkboxInput('smooth', 'Smooth')
        
        
    })
    
    output$dimension <- renderUI({
        # if (is.null(input$filec)) {
        #     return()
        # }
        
        selectInput('dimension1', 'Choose categorical column',
                    c(names(Classi_data()[sapply( Classi_data(), is.factor)])))
        # checkboxInput('jitter', 'Jitter')
        # checkboxInput('smooth', 'Smooth')
        
        
    })
    output$randomh <- renderUI({
        # if(input$knni=="randomforest"){
        #  highchartOutput("randomvarimp")
        # }
        if (input$knni == "naive") {
            return()
        }
        
        # return()
        highchartOutput("randomvarimp")
        
    })
    output$factor  <- renderUI({
        if(is.null(input$filec)){
            return()}
        highchartOutput("barfactor")
        
        
    })
    output$factorn  <- renderUI({
        if(is.null(input$filec)){
            return()}
        highchartOutput("barfactorn")
        
        
    })
    
    
    output$tablesum  <- renderUI({
        # if(is.null(input$filec)){
        #   return()}
        prettyRadioButtons(
            inputId = "knni",
            label = "Modelle Zu Auswahl",
            choices = c("naive", "randomforest"),
            outline = TRUE,
            selected = "randomforest",
            plain = TRUE,
            icon = icon("thumbs-up")
        )
        
        
    })
    output$Algc <- renderUI({
        # if(is.null(input$filec)){
        #     return()}
        
        selectInput('targetv', 'Zielvariablen zu Auswahl',
                    c(names(Classi_data()[sapply(Classi_data(), is.factor)])))
        # checkboxInput('jitter', 'Jitter')
        # checkboxInput('smooth', 'Smooth')
        
       # a <-  (nrow(df.row)-1)*7 + input#date
    })
    df_tsslwm <- reactive({
        n <- nrow(df_tssmwl())
        start <- Sys.Date() - n
        switch(
            input$frequency,
            "week" = ts(
                df_tssmwl(),
                frequency = 365.25 / 7,
                start = decimal_date(ymd(input$date))
            ),
            "months" = ts(df_tssmwl(), frequency = 12, start = start),
            "day" = ts(
                df_tssmwl(),
                frequency = 365.25,
                start = decimal_date(ymd(input$date))
            )
        )
        
    })
    
    # NNAR <- forecast(nnetar(edf_ma, lambda=lambda, size=hn), h=5, PI=TRUE)
    # NNAR
    hn <- reactive({
      alpha <- 1.5^(-10)
      alpha
      
      hn <- length(df_tsslwm())/(1.5^(-10)*(length(df_tsslwm())+30))
      hn
    })
    fit_tseries <- reactive({
      
      if(input$all){
        return()
      }
      df <-   switch(
            input$choice,
            "One" = tbats(na.omit(df_tsslwm()), biasadj = TRUE),
            "Two" = tbats(
                na.omit(df_tsslwm()),
                lambda = lambda_tssts(),
                biasadj = TRUE
            ),
            "Three" = tbats(
                na.omit(df_tsslwm()),
                lambda = 0,
                biasadj = TRUE
            ),
            "Four" = auto.arima(
                na.omit(df_tsslwm()),
                lambda = 0 ,
                biasadj = TRUE
            ),
            "Five" = auto.arima(na.omit(df_tsslwm()), biasadj = TRUE),
            "Six" = ets(
                na.omit(df_tsslwm()),
                lambda = 0,
                biasadj = TRUE
            ),
            "Seven" = ets(
                na.omit(df_tsslwm()),
                lambda = lambda_tssts(),
                biasadj = TRUE
            ),
            "stl" = stlf(na.omit(df_tsslwm()), lambda=lambda_tssts()),
            "stll" = stlf(na.omit(df_tsslwm()), lambda=0, biasadj = TRUE, damped=TRUE)
        )
     # return (df)
     # }
      return (df)
    })
    all_models <- reactive({
      if(is.null(input$all)){
        return()
      }
      ETS1 <- forecast(ets(na.omit(df_tsslwm()), lambda =0, biasadj=TRUE, damped=TRUE,model="AAA"), input$h)
      ETS2 <- forecast(ets(na.omit(df_tsslwm()), lambda =lambda_tssts(), biasadj=TRUE), input$h)
      ARIMA1 <- forecast(auto.arima(na.omit(df_tsslwm()), lambda =0, biasadj=TRUE), input$h)
      ARIMA2 <- forecast(auto.arima(na.omit(df_tsslwm()), biasadj=TRUE), input$h)
      tbats1 <- forecast(tbats(na.omit(df_tsslwm()), biasadj=TRUE), input$h)
     # tbats2 <- forecast(tbats(na.omit(df_tsslwm()), biasadj=TRUE), input$h)
      #tbats3 <- forecast(tbats(na.omit(df_tsslwm()), biasadj=TRUE), input$h)
      stll <- forecast(stlf(na.omit(df_tsslwm()), lambda=0, biasadj = TRUE, damped=TRUE), input$h)
      stl <- forecast(stlf(na.omit(df_tsslwm()), lambda=lambda_tssts()), input$h)
      
      
      Combination <- round((ARIMA1[["mean"]] + ARIMA2[["mean"]]
                            + tbats1[["mean"]]+  stll[["mean"]]+ stl[["mean"]]+
                              ETS1[["mean"]]+ETS2[["mean"]])/7)
      Combination
      # Combination.fit <- round((ARIMA1[["fitted"]] + ARIMA2[["fitted"]]
      #                           + tbats1[["fitted"]]+ tbats2[["fitted"]]+ tbats3[["fitted"]]+
      #                             ETS1[["fitted"]]+ETS2[["fitted"]])/7)
      
    })
    all_modelsfit <- reactive({
      # if(is.null(input$all)){
      #   return()
      # }
      ETS1 <- forecast(ets(na.omit(df_tsslwm()), lambda =0, biasadj=TRUE, damped=TRUE,model="AAA"), input$h)
      ETS2 <- forecast(ets(na.omit(df_tsslwm()), lambda =lambda_tssts(), biasadj=TRUE), input$h)
      ARIMA1 <- forecast(auto.arima(na.omit(df_tsslwm()), lambda =0, biasadj=TRUE), input$h)
      ARIMA2 <- forecast(auto.arima(na.omit(df_tsslwm()), biasadj=TRUE), input$h)
      tbats1 <- forecast(tbats(na.omit(df_tsslwm()), biasadj=TRUE), input$h)
      stll <- forecast(stlf(na.omit(df_tsslwm()), lambda=0, biasadj = TRUE, damped=TRUE), input$h)
      stl <- forecast(stlf(na.omit(df_tsslwm()), lambda=lambda_tssts()), input$h)
      
    
     Combination.fit <- round((ARIMA1[["fitted"]] + ARIMA2[["fitted"]]
                                + tbats1[["fitted"]]+ stll[["fitted"]]+ stl[["fitted"]]+
                                  ETS1[["fitted"]]+ETS2[["fitted"]])/7)
     Combination.fit
    
    })
    accuracy_models <- reactive({
      am <- accuracy(all_modelsfit(),df_tsslwm())
      accu.best <- round(100 - as.data.frame(am)$MAPE, 2)
      accu.best
    })
    
    combdata <- reactive({
      vl <- all_modelsfit()
      v2  <- df_tsslwm()
     df <-  cbind( vl,v2) %>% data.frame()
     names(df)[1] <- "Fit"
     names(df)[2] <- "Actual"
     df
    })
    output$modellet <- renderDataTable({
      if(is.null(input$all)){
        return()
      }
      combdata()
      
    })
    accuracy_rand <- reactive({
      if (is.null(input$file1)) {
        return()
      }
      
      accu <- accuracy(fit_tseries())

      accu.best <- round(100 - as.data.frame(accu)$MAPE, 2)
      accu.best
      
    })
    output$perfomsaccu <- renderValueBox({
      
      if (is.null(input$file1)) {
        return()
      }
      if(input$all) {
      #accuracy_rand() %>%
     df <- accuracy_models() %>%
        round(2) %>%
        paste0(" %") %>%
        valueBox(
          subtitle = "Genauigkeit des Modells"
         # color = "aqua"
         # icon = icon("gbp", lib = 'glyphicon')
        )
     return(df)
      }
      accuracy_rand() %>%
        round(2) %>%
        paste0(" %") %>%
        valueBox(
          subtitle = "Genauigkeit des Modells"
          # color = "aqua"
          # icon = icon("gbp", lib = 'glyphicon')
        )
    
    })
    
    lambda_tssts  <- reactive({
        lambda <- BoxCox.lambda(df_tsslwm())
        lambda
    })
    training <- reactive({
        set.seed(2)
        #
       
        Data <-
            Classi_data()#[,!grepl(input$targetv, names(Classi_data()))]
        
        id <- sample(2, nrow(Data), p = c(0.9, 0.1), replace = T)
        Training_set <- Data[id == 1, ]
        
        Training_set
    })
    
    Classi_Imp <- reactive({
        #
        
        
        # if (input$kanni=="randomforest"){
        #
        #   varImp(Classification_ML())
        # }
        varImp(Classification_ML())
        #return()
    })
    df_tssmwl <- reactive({
        #
        df <- df_lfocast() %>%
            select(Forecasted)
        
    })
    modelsl_upper <- reactive({
        f2 <- forecast(fit_local(), h = input$h)
        
        #data.frame() %>%
        #select(Point.Forecast)
        
        low  <- round(f2[["lower"]], 2)
        high <- round(f2[["upper"]], 2)
        
        df <-  data.frame(low, high)
        df1 <-  df %>% data.frame()
        names(df1)[1] <- "lower80"
        names(df1)[2] <- "lower95"
        names(df1)[3] <- "high80"
        names(df1)[4] <- "high95"
        df1
        
        
    })
    
    df_fcastseries <- reactive({
      # if(is.null(input$all)){
      #   f2 <-  forecast(fit_tseries(), h = input$h)
      #   b <- (nrow(df_ldata()))*7
      #   c <- round((nrow(df_ldata())*365.25/12-1))
      #   df <-  data.frame(f2)
      #   # as.Date((Sys.Date()-2)+input$h)
      #   # as.Date(Sys.Date() - 1)
      #   df1 <- na.omit(df[[1]])%>%
      #     data.frame() %>%
      #     round(2)
      #   df2 <-  switch(
      #     input$frequency,
      #     "week" = df1 %>% mutate("date" = seq.Date(
      #       as.Date(input$date+b),
      #       by = "week", length.out = nrow(df1)
      #       
      #     )) ,
      #     "months" = df1 %>% mutate("date" = seq.Date(
      #       as.Date(input$date+c),
      #       by = "months", length.out = nrow(df1)
      #     )),
      #     "day" = df1 %>% mutate("date" = seq.Date(
      #       as.Date(Sys.Date() - 1),
      #       by = "day",
      #       length.out = nrow(df1)
      #     ))
      #   )
      #   
      #   # mutate( "date"=seq.Date(as.Date(Sys.Date()-1), by ="day", length.out = nrow(df)))# %>%
      #   # rename("Forecast"=.)
      #   #as.character.Date()
      #   names(df2)[1] <- "Forecast"
      #   df2
      # }
      # else{
      #   f2 <- all_models()
      #   
      #   
      #   
      # }
        f2 <-  forecast(fit_tseries(), h = input$h)
         b <- (nrow(df_ldata()))*7
         c <- round((nrow(df_ldata())*365.25/12-1))
        df <-  data.frame(f2)
        # as.Date((Sys.Date()-2)+input$h)
        # as.Date(Sys.Date() - 1)
        df1 <- na.omit(df[[1]])%>%
            data.frame() %>%
            round(2)
        df2 <-  switch(
            input$frequency,
            "week" = df1 %>% mutate("date" = seq.Date(
                as.Date(input$date+b),
                by = "week", length.out = nrow(df1)
                                
            )) ,
            "months" = df1 %>% mutate("date" = seq.Date(
              as.Date(input$date+c),
              by = "months", length.out = nrow(df1)
            )),
            "day" = df1 %>% mutate("date" = seq.Date(
                as.Date(Sys.Date() - 1),
                by = "day",
                length.out = nrow(df1)
            ))
        )
        
        # mutate( "date"=seq.Date(as.Date(Sys.Date()-1), by ="day", length.out = nrow(df)))# %>%
        # rename("Forecast"=.)
        #as.character.Date()
        names(df2)[1] <- "Forecast"
        df2
        #as.character.Date()
        
        
        
        ##0f1941
    })
    modelslf_upper <- reactive({
        f2 <- forecast(forecast(fit_tseries(), h = input$h))
        
        #data.frame() %>%
        #select(Point.Forecast)
        
        low  <- round(f2[["lower"]], 2)
        high <- round(f2[["upper"]], 2)
        
        df <-  data.frame(low, high)
        df1 <-  df %>% data.frame()
        names(df1)[1] <- "lower80"
        names(df1)[2] <- "lower95"
        names(df1)[3] <- "high80"
        names(df1)[4] <- "high95"
        df1
        
        
    })
    
    
    
    output$plot <- renderPlotly({
        if (is.null(input$file1)) {
            return()
        }
        
        if (is.null(df_fcastcustom())) {
            return()
        }
        
        p <-
            ggplot(df_ldata(), aes_string(x = input$element, y = input$Y)) + geom_point(color =
                                                                                            "red") + theme(plot.background = element_rect(fill = input$col),
                                                                                                           legend.title = element_blank())
        # theme_d(
        #
        if (input$errorY)
            p <-
            p +   geom_errorbar(
                aes(
                    ymin = df_ldata()[[input$Y]] - sd(df_ldata()[[input$Y]]),
                    ymax = df_ldata()[[input$Y]] + sd(df_ldata()[[input$Y]])
                ),
                width = .6,
                color = "red",
                position = position_dodge(.9)
            )
        
        #
        if (input$errorX)
            p <-
            p +   geom_errorbarh(
                aes(
                    xmin = df_ldata()[[input$element]] - sd(df_ldata()[[input$element]]),
                    xmax = df_ldata()[[input$element]] + sd(df_ldata()[[input$element]])
                ),
                width = .5,
                color = "red",
                position = position_dodge(.9)
            )
        if (input$jitter)
            p <- p + geom_jitter(color = input$colploty)
        if (input$smooth)
            p <- p + geom_smooth(se = TRUE)
        if (input$boxplot)
            p <- p + geom_boxplot(color = input$colploty)
        if (input$violin)
            p <- p + geom_violin(color = input$colploty)
        
        if (input$line)
            p <- p + geom_line(color = input$colploty)
        
        
        print(ggplotly(p))
        
    })
    output$custom <- renderHighchart({
        if (is.null(input$file1)) {
            return()
        }
        if (is.null(df_fcastcustom())) {
            return()
        }
        #  df <- df_ldata()
        # names(df)[1] <- "Xselected"
        # names(df)[2] <- "Yselected"
        df1 <-    df_fcastseries() #df_fcastcustom()  modelsl_upper()
        ci <- data.frame(modelslf_upper(), df1)
        highchart(type = "stock") %>%
            hc_legend(enabled = TRUE) %>%
            hc_title(text = "Display the value forecasted for the time series set by the use.") %>% # color=c("#2175d9","#E74536","#FFB511")
            # #   hc_xAxis(categories = df$day, title = list("No of days since Jan 01")) %>%
            # #   hc_yAxis( title = list(text = "Cryptocurrency value")) %>%
            # #   hc_add_series(tput$plot <- renderPlotly({
            #
            #
            hc_add_series(df1 ,
                          "line" ,
                          hcaes(as.Date(date), Forecast),
                          name = "Forecast",
                          color = input$colc) %>%
            hc_add_series(ci,
                          "arearange",
                          hcaes(as.Date(date), low = lower95, high = high95),
                          name = "Confidential  Interval 95 %") %>%
            hc_add_series(ci,
                          "arearange",
                          hcaes(as.Date(date), low = lower80, high = high80),
                          name = "Confidential  Interval 80 %") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, low), name= "low", color="blue") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, open), name= "open", color="green") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, close), name= "close", color="orange") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, Value), name= "predicted value") %>%
            # #   hc_add_series(df_2, "scatter" , hcaes(day, Value), name = "Forecated value", color="#2175d9") %>%
            # #   # hc_xAxis(max= 52) %>%
            # #
            hc_add_theme(theme_tsc()) %>%
            hc_exporting(enabled = TRUE, filename = "custom-data-export")
    })
    output$Regressiondrop <- renderUI({
      if (is.null(input$file1)) {
        return()
      }
      
      selectInput('dependent', 'Zielvariable auswaehlen',
                  c(names(df_ldata()[sapply(df_ldata(), is.numeric)])))
      # checkboxInput('jitter', 'Jitter')
      # checkboxInput('smooth', 'Smooth')
      
      
    })
    
    output$customerdata <- renderUI({
      if (is.null(input$file1)) {
        return()
      }
      
      selectInput('dependent1', 'Abhaengigvariable auswaehlen',
                  c(names(df_ldata()[sapply(df_ldata(), is.numeric)])))
      # checkboxInput('jitter', 'Jitter')
      # checkboxInput('smooth', 'Smooth')
      
      
    })
    output$RegressionR <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        
        prettyRadioButtons(
            inputId = "Regmodel",
            label = "Modellauswahl",
            choices = c("multiple", "poly"),
            outline = TRUE,
            selected = "poly",
            plain = TRUE,
            icon = icon("thumbs-up")
        )
        
        
        
    })
    output$Regressionslider <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        if (input$Regmodel == "multiple") {
            return()
        }
        sliderInput(
            "grade",
            "Polynomgrade festlegen (von 1 bis 100):",
            min = 1,
            max = 100,
            value = 1,
            step = 1
        )
        
        
    })
    
    output$barplot <- renderUI({
        if (is.null(input$file1)) {
            return(
                 
            )
        }
        if (input$Regmodel == "poly") {
            return()
        }
        
        
        highchartOutput("varimp")
        
        
    })
    
    output$tidy <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        if (is.null(input$Regmodel)) {
            return()
        }
        
        
        dataTableOutput("meanaug")
        
        
    })
    output$explan <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        highchartOutput("fitexp")
    })
    
    output$corr <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        #dataTableOutput("rsq")
        plotOutput("corrplot")
    })
    
    
    output$fitplot  <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        # if (is.null(input$Regmodel)) {
        #     return()
        # }
        #dataTableOutput("predR")
        highchartOutput("actfitpa")
        
        
    })
    
    # output$glance <- renderUI({
    #     if (is.null(input$file1)) {
    #         return()
    #     }
    #     if (is.null(input$Regmodel)) {
    #         return()
    #     }
    #     #dataTableOutput("rsq")
    #     # highchartOutput("actfit")
    #     #plotOutput("corrplot")
    #     
    #     
    # })
    output$radiobutton <- renderUI({
        if (is.null(input$file1)) {
            return()
        }
        prettyRadioButtons(
            inputId = "tabi",
            label = "Paramter auswaehlen die angezeigt werden soll:",
            choices = c("rsqr", "fitparameters", "predicted"),
            outline = TRUE,
            selected = "fitparameters",
            plain = TRUE,
            icon = icon("thumbs-up")
        )
        
        
    })
    Regression <- reactive({
        #
        if (is.null(input$file1)) {
            return() # df_ldata()[sapply(df_ldata(), is.numeric)]
        }
        Data <-
            df_ldata()[, !grepl(input$dependent, names(df_ldata()))]
        # df.training <- data.frame(df_ldata()[sapply(df_ldata(), is.numeric)][[-c(input$dependent)]])
        
        df2 <-  switch(
            input$Regmodel,
            # "multiple" = lm(df_ldata()[[input$dependent]]~df_ldata()[[input$element]]+df_ldata()[[input$explanatory]], data =df_ldata()[sapply(df_ldata(), is.numeric)], method = "ranger",
            #                 importance = "impurity") ,
            "multiple" = lm(
                df_ldata()[[input$dependent]] ~ .,
                data = Data[sapply(Data, is.numeric)],
                method = "ranger",
                importance = "impurity"
            ),
            "poly" = lm(
                df_ldata()[[input$dependent]]  ~ poly(df_ldata()[[input$dependent1]], input$grade, raw = TRUE),
                data = df_ldata()[sapply(df_ldata(), is.numeric)],
                method = "ranger",
                importance = "impurity"
            )
        )
        df2
        
        
    })
    
    # output$meanaug <- renderDataTable({
    #     if (is.null(input$file1))
    #         return(summary(gapminder))
    # 
    #   Regression() %>%
    #         data.frame()
    # 
    # })
    output$rand <- renderDataTable({
      if (is.null(input$file1))
        return(summary(gapminder))
      
      choose_tb() %>%
        data.frame()
      #augment_fit()
      
    })
    
    
    output$rsq <- renderDataTable({
        # if (is.null(input$file1)) {
        #     return()
        # }
        
        #glance_R() %>%
        choose_tb() %>%
            data.frame()
        
        
        #tidy_R() %>%
        # varimp_R() %>%
        
        
        
    })
    choose_tb <- reactive({
      if (is.null(input$file1)) {
        return() # df_ldata()[sapply(df_ldata(), is.numeric)]
      }
      #df  <- Classi_data()[,!grepl(input$targetv, names(Classi_data()))]
      # Data <- df %>% mutate_if(is.numeric, funs(as.numeric(scale(.))))
      df2 <-  switch(
        input$tabi,
        # "multiple" = lm(df_ldata()[[input$dependent]]~df_ldata()[[input$element]]+df_ldata()[[input$explanatory]], data =df_ldata()[sapply(df_ldata(), is.numeric)], method = "ranger",
        #                 importance = "impurity") ,
        "rsqr" =  glance_R(),
        
        "fitparameters" = tidy_R(),
        "predicted" = augment_R()
      )
      df2
    })
    tidy_R <- reactive({
      # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
      
      tidy(Regression()) %>% data.frame()
      
      
    })
    glance_R <- reactive({
        # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
        if (is.null(input$file1)) {
            return()
        }
        
        
        
        
        glance(Regression()) %>% data.frame()
        
    })
    
    augment_R <- reactive({
        # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
        if (is.null(input$file1)) {
            return()
        }
        
        #names(df_ldata()[[input$dependent]]) <-"check"
        
        
        
        df <- augment(Regression()) %>% data.frame()
        names(df)[1] <- "Actual"
        #names(df[["df_ldata()[[input$dependent]]"]]) <- "Actual"
        df
        
    })
    
    
    augment_fit <- reactive({
        # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
        if (is.null(input$file1)) {
            return()
        }
        
        
        
        
        df <-  augment(Regression()) %>% data.frame()
        names(df)[1] <- "Actual"
        n <- nrow(df)
        Fit <-  df[[".fitted"]]
        df1 <- data.frame(df, Fit)
        df2 <- df1 %>%
            data.frame() %>%
            select(Actual, Fit)
        if (input$freqreg == "day") {
            df3 <-
                df2 %>%  mutate("Date" = seq.Date(
                    as.Date(Sys.Date() - n),
                    by = input$freqreg,
                    length.out = n
                ))
        }
        if (input$freqreg == "week") {
            df3 <-
                df2 %>%  mutate("Date" = seq.Date(
                    as.Date(Sys.Date()+1 - n * 7),
                    by = input$freqreg,
                    length.out = n
                ))
        }
        if (input$freqreg == "months") {
            df3 <-
                df2 %>%  mutate("Date" = seq.Date(
                    as.Date(Sys.Date() - n * 31 + 13),
                    by = input$freqreg,
                    length.out = n
                ))
        }
        df3
    })
    output$corrplot <- renderPlot({
        if (is.null(input$file1)) {
            return()
        }
        
        
        corrplot(cor(df_ldata()[sapply(df_ldata(), is.numeric)]))
    })
    
    varimp_R <- reactive({
        # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
        
        if (is.null(input$file1)) {
            return()
        }
        
        varImp(Regression())
        
        
    })
    output$actfitpa <- renderHighchart({
        if (is.null(input$file1)) {
            return()
        }
        # if (is.null( df_fcastseries())) {
        #   return()
        #}
        #  df <- df_ldata()
        # names(df)[1] <- "Xselected"
        # names(df)[2] <- "Yselected"
        df1 <- augment_fit()
        highchart(type = "stock") %>%
            hc_legend(enabled = TRUE) %>%
            hc_title(text = "Fitted as well as actual value represented as time series for the frequency set by the user.") %>% # color=c("#2175d9","#E74536","#FFB511")
            # #   hc_xAxis(categories = df$day, title = list("No of days since Jan 01")) %>%
            # #   hc_yAxis( title = list(text = "Cryptocurrency value")) %>%
            # #   hc_add_series(tput$plot <- renderPlotly({
            #
            #
            hc_add_series(df1 ,
                          "line" ,
                          hcaes(as.Date(Date), Actual),
                          name = "Actual value",
                          color = "red") %>%
            hc_add_series(df1,
                          "line" ,
                          hcaes(as.Date(Date), Fit),
                          name = "Fitted value",
                          color = "blue") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, open), name= "open", color="green") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, close), name= "close", color="orange") %>%
            # #   hc_add_series(df, "scatter" , hcaes(day, Value), name= "predicted value") %>%
            # #   hc_add_series(df_2, "scatter" , hcaes(day, Value), name = "Forecated value", color="#2175d9") %>%
            # #   # hc_xAxis(max= 52) %>%
            # #
            hc_add_theme(hc_theme_darkunica()) %>%
            hc_exporting(enabled = TRUE, filename = "custom-data-export")
    })
    output$varimp <- renderHighchart({
        if (is.null(input$file1)) {
            return()
        }
        #  df <- df_ldata()
        # names(df)[1] <- "Xselected"
        # names(df)[2] <- "Yselected"
        #df <- varimp_R()(-c[[input$dependent]])
        # df1 <- df_fcastseries()
        imp.data <- varimp_R() %>%
            # imp.data <- df %>%
            as.data.frame() %>%
            rownames_to_column() %>%
            arrange(desc(Overall)) %>%
            #round() %>%
            # filter(Overall < 1000) %>%
            mutate(rowname = forcats::fct_inorder(rowname))
        highchart() %>%
            hc_legend(enabled = FALSE) %>%
            hc_title(text = "KPIs  nach AI Analyse") %>%
            hc_xAxis(categories = imp.data$rowname,
                     title = list(text = "KPI")) %>%
            hc_yAxis(title = list(text = "Gewichtung")) %>%
            hc_add_series(imp.data ,
                          "bar" ,
                          hcaes(rowname, Overall),
                          name = "Faktoren",
                          color = "#2175d9") %>%
            # highchart(type = "stock") %>%
            #   hc_legend(enabled = TRUE) %>%
            #   hc_title(text = "At the moment only two columns are supported. Column 1 is X and column 2 = Y") %>% # color=c("#2175d9","#E74536","#FFB511")
            #   # #   hc_xAxis(categories = df$day, title = list("No of days since Jan 01")) %>%
            #   # #   hc_yAxis( title = list(text = "Cryptocurrency value")) %>%
            #   # #   hc_add_series(tput$plot <- renderPlotly({
            #   #
            #   #
            #   hc_add_series(df1 , "line" , hcaes(as.Date(date), Forecast), color="red") %>%
            #   # #   hc_add_series(df, "scatter" , hcaes(day, low), name= "low", color="blue") %>%
            #   # #   hc_add_series(df, "scatter" , hcaes(day, open), name= "open", color="green") %>%
        #   # #   hc_add_series(df, "scatter" , hcaes(day, close), name= "close", color="orange") %>%
        #   # #   hc_add_series(df, "scatter" , hcaes(day, Value), name= "predicted value") %>%
        #   # #   hc_add_series(df_2, "scatter" , hcaes(day, Value), name = "Forecated value", color="#2175d9") %>%
        #   # #   # hc_xAxis(max= 52) %>%
        #   # #
        hc_add_theme(hc_theme_ffx()) %>%
            hc_exporting(enabled = TRUE, filename = "custom-data-export")
    })
    output$randomvarimp <- renderHighchart({
        imp.data <- Classi_Imp() %>%
            # imp.data <- df %>%
            as.data.frame() %>%
            rownames_to_column()
        
        names(imp.data)[2] <- "Overall"
        # select(1) %>%
        #rename("Overall"=1) %>%
        df <-  imp.data  %>%
            arrange(desc(Overall)) %>%
            # round() %>%
            mutate(rowname = forcats::fct_inorder(rowname))
      
        highchart() %>%
            hc_legend(enabled = FALSE) %>%
            hc_title(text = "KPIs nach AI Analyse") %>%
            hc_xAxis(categories = df$rowname,
                     title = list(text = "KPI")) %>%
            hc_yAxis(title = list(text = "Gewichtung")) %>%
            hc_add_series(df ,
                          "bar" ,
                          hcaes(rowname, Overall),
                          name = "Factors",
                          color = "red") %>%
            # highchart(type = "stock") %>%
            #   hc_legend(enabled = TRUE) %>%
            #   hc_title(text = "At the moment only two columns are supported. Column 1 is X and column 2 = Y") %>% # color=c("#2175d9","#E74536","#FFB511")
            #   # #   hc_xAxis(categories = df$day, title = list("No of days since Jan 01")) %>%
            #   # #   hc_yAxis( title = list(text = "Cryptocurrency value")) %>%
            #   # #   hc_add_series(tput$plot <- renderPlotly({
            #   #
            #   #
            #   hc_add_series(df1 , "line" , hcaes(as.Date(date), Forecast), color="red") %>%
            #   # #   hc_add_series(df, "scatter" , hcaes(day, low), name= "low", color="blue") %>%
            #   # #   hc_add_series(df, "scatter" , hcaes(day, open), name= "open", color="green") %>%
        #   # #   hc_add_series(df, "scatter" , hcaes(day, close), name= "close", color="orange") %>%
        #   # #   hc_add_series(df, "scatter" , hcaes(day, Value), name= "predicted value") %>%
        #   # #   hc_add_series(df_2, "scatter" , hcaes(day, Value), name = "Forecated value", color="#2175d9") %>%
        #   # #   # hc_xAxis(max= 52) %>%
        #   # #
        hc_add_theme(hc_theme_flat()) %>%
            hc_exporting(enabled = TRUE, filename = "custom-data-export")
    })
    
    
    output$barfactor <- renderHighchart({
        # imp.data <- Classi_Imp() %>%
        #   # imp.data <- df %>%
        #   as.data.frame() %>%
        #   rownames_to_column()
        # dim <-  Classi_Imp()[, grepl(input$dimension, names(Classi_Imp()))]
        # 
        # target <- Classi_Imp()[, grepl(input$targetv, names(Classi_Imp()))]
        # df <- data.frame(target,df)
        
        dim <-   Classi_data()[,grepl(input$dimension1, names(Classi_data()))]
        dim
        target <- Classi_data()[,grepl(input$targetv, names(Classi_data()))]
        target
        # data.frame(dim, target)
        df <- data.frame(target,dim)
        df
        # names(df)[1] <- input$targetv
        # names(df)[2] <- input$dimension1
        # names(imp.data)[2] <- "Overall"
        # # select(1) %>%
        # #rename("Overall"=1) %>%
        # df <-  imp.data  %>%
        #   arrange(desc(Overall)) %>%
        #   # round(Overall,2)%>%
        #   mutate(rowname = forcats::fct_inorder(rowname))
        df %>% 
            count(dim, target) %>% 
            hchart('bar', hcaes(x = 'dim', y = 'n', group = "target")) %>%
            hc_xAxis(
                title = list(text = input$dimension1)) %>%
            hc_add_theme(hc_theme_flat()) %>%
            hc_exporting(enabled = TRUE,filename = "Kunden-analyse") 
        
    })
    output$barfactorn <- renderHighchart({
        # imp.data <- Classi_Imp() %>%
        #   # imp.data <- df %>%
        #   as.data.frame() %>%
        #   rownames_to_column()
        # dim <-  Classi_Imp()[, grepl(input$dimension, names(Classi_Imp()))]
        # 
        # target <- Classi_Imp()[, grepl(input$targetv, names(Classi_Imp()))]
        # df <- data.frame(target,df)
        
        dim <-   Classi_data()[,grepl(input$dimension1, names(Classi_data()))]
        dim
        num <-   Classi_data()[,grepl(input$numerisch, names(Classi_data()))]
        num
        target <- Classi_data()[,grepl(input$targetv, names(Classi_data()))]
        target
        # data.frame(dim, target)
        df <- data.frame(target,dim,num)
        df
        # names(df)[1] <- input$targetv
        # names(df)[2] <- input$dimension1
        # names(imp.data)[2] <- "Overall"
        # # select(1) %>%
        # #rename("Overall"=1) %>%
        # df <-  imp.data  %>%
        #   arrange(desc(Overall)) %>%
        #   # round(Overall,2)%>%
        #   mutate(rowname = forcats::fct_inorder(rowname))
        df %>% 
            select(dim, target,num) %>%
            count(num, target,dim) %>% 
            hchart('bar',dataLabels = list(enabled=TRUE, format = '{point.name}'), hcaes(x = 'num', y ='n', group = "target")) %>%
            hc_xAxis(
                title = list(text = input$numerisch)) %>%
            # hc_add_series_labels_values(dataLabels = list(enabled=TRUE, format = '{point.name}')) %>%
            hc_add_theme(hc_theme_flat()) %>%
            hc_exporting(enabled = TRUE,filename = "Kunden-analyse") 
        
    })
    
    #output$carstable <- renderDataTable(mtcars)
    output$carstable2 <- renderDataTable({
        # if(is.null(input$filec)){
        #     return(diamonds)}
        Classi_data()
    })
    
    output$uitable <- renderDataTable({
        # Classi_data()
        #training()
        # if (is.null(v$data)){return(predict_data())
        #
        #   }
        #  if(input$newdataf==TRUE){
        #
        #    df <-   new_datap()
        #  }
        #  else {
        predict_data()
        
        #
        #  }
        #new_datap()
        #predict_newdata()
        #df
        # imp.data <- Classi_Imp() %>%
        #   # imp.data <- df %>%
        #   as.data.frame() %>%
        #   rownames_to_column()
        
    })
    output$accuracyclass <- renderPrint(# {
        #if(input$knni=="randomforest")
        {
            #   varImp(Classification_ML())
            # }
            Classification_ML()
            
            
            # confusion_M() %>% data.frame()
        })
    
    
    predict_data  <- reactive({
        # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
        df <-  predict(Classification_ML(), testing()) %>% data.frame()
        names(df)[1] <- input$targetv
        
        df
        
        #glance(Regression())
        
    })
    # output$carstable4 <- renderDataTable(iris)
    # output$carstable3 <- renderDataTable(gapminder)
    datasetInput <- reactive({
        # if (input$newdataf == TRUE) {
        #     df <-    switch(
        #         input$dataset,
        #         "trainingset" = training(),
        #         "testingset" = testing(),
        #         "forecast" = forecast.df(),
        #         "rsqr" = glance_R(),
        #         "fitparameter" = tidy_R(),
        #         "predicted" =  augment_R(),
        #         "newdata" = predict_newdata()
        #     )
        # }
        df <- switch(
            input$dataset,
            "trainingset" = training(),
            "testingset" = testing(),
            "forecast" = forecast.df(),
            "rsqr" = glance_R(),
            "fitparameter" = tidy_R(),
            "predicted" =  augment_R(),
            "newdata" = predict_data()
        )
        
        df
    })
    
    # v <- reactiveValues(data = NULL)
    #
    # # observeEvent(input$runif, {
    # #   v$data <- runif(100)
    # # })
    # observeEvent(input$newdata, {
    #   # Use purrr's walk command to cycle through each
    #   # panel tabs and remove them
    #   # data.exp <-ts(rbind(edf_ma, Combination), freq=365.25/7,
    #   v$data <-  predict(Classification_ML(), testing()) %>% data.frame()
    #   names(df)[1] <- input$targetv
    #
    #   v$data
    # })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, Sys.Date(), '.csv', sep = '')
        },
        content = function(con) {
            write.csv(datasetInput(), con)
        }
    )
})
