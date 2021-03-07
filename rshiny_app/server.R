##head(test_dat)
##class(test_dat)
library(wordcloud)
library("syuzhet")
library(MASS)
checkvalidate = 0
flag <- FALSE
create_data <- function(drug,review,rating)
{
  
  standarddata <- data.frame('day'=0,'effect'=0,'feel'=0,'ive'=0,'month'=0,'period'=0,
                             'pill'=0,'side'=0,'start'=0,'take'=0,'time'=0,'week'=0,
                             'weight'=0,'work'=0,'year'=3)
  
  localdata <- data.frame('rating'=rating,'review'=review)
  ## making category class 
  y <- ifelse(localdata$rating>5,1,0)
  localdata <- cbind(localdata,y)
  
  local_corpus <- VCorpus(VectorSource(localdata$review))
  local_corpus <- tm_map(local_corpus,tolower)
  local_corpus <- tm_map(local_corpus,removePunctuation)
  local_corpus <- tm_map(local_corpus,removeNumbers)
  local_corpus <- tm_map(local_corpus,removeWords,c(stopwords("en"),stop))
  local_corpus <- tm_map(local_corpus,stripWhitespace)
  local_corpus <- tm_map(local_corpus,stemDocument)
  local_corpus <- tm_map(local_corpus,PlainTextDocument)
  
  
  dtm <- DocumentTermMatrix(local_corpus)
  #print(dtm)
  local_corpus <- removeSparseTerms(dtm, 0.8)
  
  dtm_matrix_local <- as.matrix(local_corpus)
  dtm_matrix_local <- cbind(dtm_matrix_local, localdata$y)
  colnames(dtm_matrix_local)[ncol(dtm_matrix_local)] <- "y"
  #print(dtm_matrix_local)
  
  inputdata <- as.data.frame(dtm_matrix_local)
  
  for (i in colnames(standarddata)){
    if(any(names(inputdata) == i))
    {
      if(inputdata[i] > 0)
      {
        standarddata[i] <- inputdata[i]
      }
    }
  }
  standarddata$y <- factor(y,levels=c(0,1))
  return(standarddata)
}

getcondition <- function(drugname)
{
  selectedDrug <- filter(data,data$drugName==drugname)
  drugcondition <- data.frame(unique(selectedDrug$condition))
  names(drugcondition)[names(drugcondition) == "unique.selectedDrug.condition."] <- "Conditions"
  return(drugcondition)
  
}
create_wordCloud <- function(drugname)
{
  selectedDrug <- filter(data,data$drugName==drugname)
  corpus <- Corpus(VectorSource(unique(selectedDrug$review)))
  # clean up the corpus using tm_map()
  corpus_clean <- tm_map(corpus, (tolower))
  ##inspect(corpus_clean,10)
  corpus_clean <- tm_map(corpus_clean, removeNumbers)
  stopwords("english")
  corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
  ##inspect(corpus_clean)
  corpus_clean <- tm_map(corpus_clean, removePunctuation)
  #inspect(corpus_clean)
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)
  ##inspect(corpus_clean)
  tdm <- TermDocumentMatrix(corpus_clean,
                            control = list(minwordLength = c(1,Inf)))
  m <- as.matrix(tdm)
  wordFreq <- sort(rowSums(m), decreasing=TRUE)
  return(wordFreq)
}
nrc_sentiment_generator <- function(drugname)
{
  total_len <- 0
  selectedDrug <- filter(data,data$drugName== drugname)
  x <- unique(selectedDrug$review)
  x <- trimws(x, which = "both")     
  x_sentiment <- get_nrc_sentiment(x)
  #print(x_sentiment)
  x <- x_sentiment['positive']
  arrsize <- count(x)
  arrsize <- arrsize[[1]]
  
  if(arrsize > 0)
  {
    for(i in 1:arrsize)
    {
      total_len <- total_len+x[i,]
    }
    sentmean <- as.numeric(format((total_len/arrsize), digits = 4))
    sentmean <- sentmean*10
    overall <- 100 - sentmean
    addval <-as.numeric(format((overall/2), digits = 4))
    sentmean <- sentmean+addval
    return(sentmean)
  }
  else
    return(0)
  #return(x_sentiment['positive'])
}
shinyServer(
  function(input, output, session)
  {
      # data <- reactive({
      #   validate(
      #     need(input$drugname !='', 'Please enter valid drug name!'),
      #     # need(input$rating != '', 'Rating should be 1 to 10.'),
      #     # need(input$review !='', 'Your review is required!', class(20)),
      #     # need(input$model != '', 'Please choose a Model.')
      #   )
      #   get(input$data, 'package:datasets')
      # })
      
     output$outtext <- renderText({
      validate(
        need(input$drugname !='', 'Please enter valid drug name!'),
        need(input$rating != '', 'Rating should be 1 to 10.'),
        need(input$review !='', 'Your review is required!', class(20)),
        need(input$model != '', 'Please choose a Model.')
      )
          localdata <- create_data(input$drugname,input$rating,input$review)
          local.forest <- predict(fit.forest , as.data.frame(localdata))
          algo.result <- attributes(local.forest)
          finalresult <- ifelse(algo.result$names==1,'effective','ineffective')
       
      })
     ############################# Display data into tabular format ####################
    output$outtable <- renderTable({
      model <- ''
        flag <- validate(
          need(input$drugname !='', 'Please enter valid drug name!'),
          need(input$rating != '', 'Rating should be 1 to 10.'),
          need(input$review !='', 'Your review is required!', class(20)),
          need(input$model != '', 'Please choose a Model.')
        )
        #head(data())
        if(input$model == 'Method4')
        {
          model='Random Forest'
        }
        else if(input$model == 'Method1')
        {
          model='SVM'
        }
        else if(input$model == 'Method2')
        {
          model='Multiple Regression'
        }
        else
        {
          model='Decision Tree'
        }
        
        localdata <- create_data(input$drugname,input$rating,input$review)
        if(model=='Random Forest')
        {
          local.forest <- predict(fit.forest , as.data.frame(localdata))
          algo.result <- attributes(local.forest)
          finalresult <- ifelse(algo.result$names==1,'effective','ineffective')
        }
        if(model=='Decision Tree')
        {
          local.c50 <- predict(train.tree, as.data.frame(localdata))
          finalresult <- ifelse(local.c50==1,'effective','ineffective')
        }
        if(model=='Multiple Regression')
        {
          local.multi <- predict(model, as.data.frame(localdata))
          finalresult <- ifelse(local.multi==1,'effective','ineffective')
        }
        if(model=='SVM')
        {
          local.svm <- predict(review_top10Cond_model, as.data.frame(localdata))
          finalresult <- ifelse(local.svm==1,'effective','ineffective')
        }
        localdf <- data.frame('Algorithm'=model,'Result'= finalresult)
        
      })
    ####################### Showing Sentiment percentage ######################
      output$sentiment <- renderTable({
      Sentiment <- nrc_sentiment_generator(input$drugname)
      drugsentitab <- data.frame(Sentiment)
      print(drugsentitab)
      ##names(drugsentitab)[names(drugsentitab) == "nrc_sentiment_generator.input.drugname."] <- 'Positive Sentiments'
    })
    ################## creating condition list #################################
    output$condition <- renderTable({
        locatcondition <- getcondition(input$drugname)
    })
    ####################### Creating word cloud ###################################
      output$plot <- renderPlot({
        wordF <- create_wordCloud(input$drugname)
        wordcloud(words=names(wordF), freq=wordF, min.freq = 2, random.order = F, 
                  colors=rainbow(20),scale=c(8, 0.3))
      })
  }
  
)
