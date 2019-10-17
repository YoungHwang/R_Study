library(gdata)
library(tm)
library(KoNLP)
library(sqldf)
library(stringr)
library(party)
library(rpart)
library(partykit)
library(caret)
library(doParallel)
registerDoParallel(3)


# 한글 euc-kr 안깨지게 읽어 오는 함수
#install.packages("readr")
library(readr)

read.csv.any <- function(text, sep = "", ...) {
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
}

mydata <- read.csv.any("D:\\dev\\src\\R\\big_data_test\\true_lie2.csv"
                       , header = TRUE)

#mydata <- read.csv("D:\\dev\\src\\R\\big_data_test\\true_lie2.csv"
#                   , header = TRUE
#                   , sep = ","
#                   ,quote = '"')

str(mydata)
rows <- dim(mydata)[1]
mydata$seq <- 1:rows

mydata$content <- as.character(mydata$content)
mydata$content <- gsub("\n", "", mydata$content)
mydata$content <- gsub("'", "", mydata$content)
mydata$content <- gsub("\\{", "", mydata$content)
mydata$content <- gsub(")", "", mydata$content)
mydata$content <- gsub("[\\]", "", mydata$content)
mydata$content <- gsub("[']", "", mydata$content)


false_key <- as.list(c("꼭", "않는다","!","1","사람","많이",'매우','아니다','행복'
                       ,'수가','언제나','모든','왜냐하면','말한다','무지','저는','생각합','불사','그래서'))

false_key <- unlist(false_key)

# 그냥 c 로 만든것과 false_key <- as.list(c()) 하고 unlist 한것의 차이?
c("꼭", "않는다","!","1","사람","많이",'매우','아니다','행복'
  ,'수가','언제나','모든','왜냐하면','말한다','무지','저는','생각합','불사','그래서')

true_key <- as.list(c('너무','때문','다면','좋아','좋은','때문이다','단','지만','이다','가장','생각','나는'))
true_key <- unlist(true_key)

ind <- unlist(lapply(false_key, function(x) grep(x, mydata$content,perl = T)))

mydata$falsekey <- 0
mydata[ind, "falsekey"] <- 1
mydata$falsekey <- factor(mydata$falsekey)
str(mydata)


ind <- unlist(lapply(true_key, function(x) grep(x, mydata$content,perl = T)))
mydata$truekey <- 0
mydata[ind, "truekey"] <- 1
mydata$truekey <- factor(mydata$truekey)
head(mydata,10)


tmp1 <- mydata$content
tmp2 <- lapply(tmp1, extractNoun)
tmp3 <- Corpus(VectorSource(tmp2))
dtm <- DocumentTermMatrix(tmp3)
dtm


tmp4 <- as.matrix(dtm)
dim(tmp4)

tmp5 <- as.data.frame(cbind(tmp4, class = mydata$class))
tmp5$class <- factor(tmp5$class)

str(tmp5)

# 왜 dim(tmp5)[2] 를 선택한 이유?
a <- dim(tmp5)[2] - 1

tmp5$total_word <- rowSums(tmp5[, c(1:a)])
tmp5$heu1 <- mydata$heu1


simplepos <- lapply(mydata$content, SimplePos22)


tmp51 <- grep("EC", simplepos)
tmp5$ec <- 0
tmp5[tmp51, "ec"] <- 1

tmp51 <- grep("EP", simplepos)
tmp5$ep <- 0
tmp5[tmp51, "ep"] <- 1

tmp51 <- grep("ET", simplepos)
tmp5$et <- 0
tmp5[tmp51, "et"] <- 1

tmp51 <- grep("JC", simplepos)
tmp5$jc <- 0
tmp5[tmp51, "jc"] <- 1

tmp51 <- grep("JP", simplepos)
tmp5$jp <- 0
tmp5[tmp51, "jp"] <- 1

tmp51 <- grep("JX", simplepos)
tmp5$jx <- 0
tmp5[tmp51, "jx"] <- 1

tmp51 <- grep("MA", simplepos)
tmp5$ma <- 0
tmp5[tmp51, "ma"] <- 1

tmp51 <- grep("MM", simplepos)
tmp5$mm <- 0
tmp5[tmp51, "mm"] <- 1

tmp51 <- grep("NB", simplepos)
tmp5$nb <- 0
tmp5[tmp51, "nb"] <- 1

tmp51 <- grep("NC", simplepos)
tmp5$nc <- 0
tmp5[tmp51, "nc"] <- 1

tmp51 <- grep("NP", simplepos)
tmp5$np <- 0
tmp5[tmp51, "np"] <- 1

tmp51 <- grep("NN", simplepos)
tmp5$nn <- 0
tmp5[tmp51, "nn"] <- 1

tmp51 <- grep("NQ", simplepos)
tmp5$nq <- 0
tmp5[tmp51, "nq"] <- 1

tmp51 <- grep("PA", simplepos)
tmp5$pa <- 0
tmp5[tmp51, "pa"] <- 1

tmp51 <- grep("PX", simplepos)
tmp5$px <- 0
tmp5[tmp51, "px"] <- 1

tmp51 <- grep("XP", simplepos)
tmp5$xp <- 0
tmp5[tmp51, "xp"] <- 1

tmp51 <- grep("XS", simplepos)
tmp5$xs <- 0
tmp5[tmp51, "xs"] <- 1

tmp51 <- grep("EF", simplepos)
tmp5$ef <- 0
tmp5[tmp51, "ef"] <- 1

tmp51 <- grep("PV", simplepos)
tmp5$pv <- 0
tmp5[tmp51, "pv"] <- 1

a <- dim(tmp5)[2]
colnames(tmp5[,c(645:a)])

tmp5$variety <- rowSums(tmp5[,c(645:a)])

prop.table(table(tmp5$class))

colnames(tmp5) <- gsub("[\\]","",colnames(tmp5))
colnames(tmp5) <- gsub("[//]","",colnames(tmp5))


ind <- sample(2, nrow(tmp5), replace = TRUE, prob = c(0.6, 0.4))
train <- tmp5[ind == 1, ]
test <- tmp5[ind == 2,]
rpp1 <- rpart(class~., data=train, parms=(list(prior=c(0.3, 0.7))), cp = 0.001)

str(train)
