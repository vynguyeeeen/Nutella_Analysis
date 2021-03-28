#LOAD ALL LIBRARIES
library(textreadr)
library(pdftools)
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
data(stop_words)       	      #create dummy object to remove stop words later 
library(scales) 	            #correlogram 
library(tm)		                #dtm and corpus 
library(Matrix)		            #dtm -- putting the data in a sparse matrix
library(textdata) 	          #sentiment analysis
library(tidyr)		            #sentiment analysis
library(reshape2)             #shape the sentiment
library(igraph)               #n-gram plot
library(ggraph)               #n-gram plot
library(widyr)                #correlation between word
library(quanteda)           	#Naive Bayes
library(quanteda.textmodels)  #Naive Bayes
library(RColorBrewer)         #Naive Bayes


##############################################
# LOAD DATA 
##############################################

#load path to file
setwd("/Users/vynguyen/Downloads/Team 11/Team 11 - Survey Responses") 
nm <- list.files(path="/Users/vynguyen/Downloads/Team 11/Team 11 - Survey Responses")

#read the data to make sure all is imported 
my_data <- read_document(file=nm[1])                #read 1st file, line 1-7 for answers from q1-7         
my_data_together <- paste(my_data, collapse = " ")  #1st file: group all 7 answers like a monologue 

#bind to display all file name in 1 column 
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " "))) 

#turn .txt files into DF 
mydf <- data_frame(line=1:34, text=my_txt_text)
View(mydf)

##############################################
# PREPARE DATA - TOKENIZATION
##############################################

#group data by line number; line = id for survey response 
og_nutella <- mydf %>%
  group_by(line) %>%
  ungroup()

#add more to stop_words --> customize stop words 
custom_stop_words <- tribble(
  #column names should match stop_words
  ~word,  ~lexicon,
  #add words to custom stop words
  "um", "CUSTOM",
  "yeah",  "CUSTOM",
  "ooh", "CUSTOM",
  "oh", "CUSTOM"
)

#bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

#tidy data;one token per row (removing stop words)
tidy_nutella <- og_nutella %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words2) #dictionary including customized stop words 

#counting frequencies for tokens
tidy_nutella %>%
  count(word, sort=TRUE)



##############################################
# FRAMEWORK 1: CORRELATION (corr between success groups and failed groups) 
# goal: find keywords that separate the two groups 
##############################################

#BIZ SUCCESS ONLY
df_success <- mydf[c(1,3,4,5,6,7,8,10,11,12,13,16,17,18,19,20,21,22,23,26,27,28,30,33,33,34),]
#tidy data - tokenize, remove customized stop words 
tidy_success <- df_success %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)
#count frequencies 
tidy_success %>%
  count(word, sort=TRUE)

#BIZ FAILURE ONLY
df_failure <- mydf[c(2,9,14,15,24,25,29,31),]
#tidy data - tokenize, remove customized stop words 
tidy_fail <- df_failure %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2)
#count frequencies 
tidy_fail %>%
  count(word, sort=TRUE)

#combine both data and prepare for correlation framework  
frequency <- bind_rows(mutate(tidy_success, author= "Biz Success"),
                       mutate(tidy_fail, author= "Biz Failure"))%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n /sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Biz Success`)

#CORRELOGRAM
correlogram <- ggplot(frequency, aes(x=proportion, y=`Biz Failure`, 
                      color = abs(`Biz Failure`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=4)+
  theme(legend.position = "none")+
  labs(y= "Biz Failure", x=NULL)
print(correlogram)

#CORRELATION TEST
corr_compare <- cor.test(data=frequency[frequency$author == "Biz Success",],
         ~proportion + `Biz Failure`)
print(corr_compare)


##############################################
# FRAMEWORK 2: SENTIMENT ANALYSIS
#goal: find specific keywords with underlining sentiment for BIZ SUCCESS GROUP ONLY 
##############################################

#prepare data 
og_nutella_success <- df_success %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(line, word, sort=TRUE) %>%
  ungroup()

#load sentiment dictionaries 
afinn <- get_sentiments("afinn") #Negative vs positive sentiment
nrc <- get_sentiments("nrc")     #emotions
bing <- get_sentiments("bing")   #binary

#bind all 3 dictionaries  
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

#WORDS THAT BRING NEGATIVE & POSITIVE SENTIMENTS WITH WEIGHTS
#afinn-negative --> words to avoid for business successs
afinn <- sentiments %>%
  filter(lexicon == "afinn")

neg_weight <- og_nutella_success %>%
  inner_join(afinn) %>%
  mutate(tokenfreqsentiment = n*value) %>%
  arrange(desc(-tokenfreqsentiment))

#afinn-positive --> words to focus for business sucess  
pos_weight <- og_nutella_success %>%
  inner_join(afinn) %>%
  mutate(tokenfreqsentiment = n*value) %>%
  arrange(desc(tokenfreqsentiment))

#print top 3 results 
head(neg_weight,3)
head(pos_weight,3)

#WORDS THAT BRING JOY & SADNESS 
#nrc-sadness --> words to avoid for biz success 
nrc <- get_sentiments("nrc") %>%  
  filter(sentiment == "sadness")

nrc_sad <- og_nutella_success %>%
  inner_join(nrc) %>%  			
  count(word, sort=T)

#nrc-joy --> words to focus for biz success 
nrc <- get_sentiments("nrc") %>%  
  filter(sentiment == "joy")

nrc_joy <- og_nutella_success %>%
  inner_join(nrc) %>%  			
  count(word, sort=T)

#print top 3 results 
head(nrc_sad,3)
head(nrc_joy,3)


##############################################
# FRAMEWORK 3: N-GRAMS  
#goal: find phrases that stood out for BIZ SUCCESS GROUP ONLY 
##############################################

#create trigram 
trigram <- df_success %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!is.na(trigram))%>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words2$word) %>%
  filter(!word2 %in% stop_words2$word) %>%
  filter(!word3 %in% stop_words2$word) 

#count trigram 
trigram_counts <- trigram %>%
  count(word1, word2, sort = TRUE)

#create matrix to draw trigram network
trigram_graph <- trigram_counts %>%
  graph_from_data_frame()

#visualize trigram network
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
nutella_trigram <- ggraph(trigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightsalmon2", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
print(nutella_trigram)


##############################################
# FRAMEWORK 4: PAIRWISE CORRELATION/ CORR NETWORK 
#goal: find high correlated words for BIZ SUCCESS GROUP ONLY 
##############################################

#prepare dataset
my_tidy_df <- df_success %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words2$word)

#filter least common words 
word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= 4) %>%
  pairwise_cor(word, line, sort=TRUE) #check corr based on how often words appear in the same survey 


#PLOT CORRELATION NETWORK (for corr above 0.5)
corr_network <- word_cors %>%
  filter(correlation >.5) %>%  
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightsalmon1", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()
print(corr_network)


##############################################
# OPTIONAL - FRAMEWORK 5: NAIVE BAYES  
# make prediction for biz success/failure 
##############################################

#load path to PDF files 
setwd("/Users/vynguyen/Downloads/Team 11/Team 11 - Survey Responses (pdf files)")
nm <- list.files(path="/Users/vynguyen/Downloads/Team 11/Team 11 - Survey Responses (pdf files)")

#read Vcorpus
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

#turn PDF file into regular corpus 
msg.dfm <- dfm(corpus(opinions), tolower = TRUE)	 #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq  = 1)
msg.dfm <- dfm_weight(msg.dfm)

#create training & testing set 
msg.dfm.train<-msg.dfm[1:26,] 	#train doc 1-26
msg.dfm.test<-msg.dfm[27:34,]		#test doc 31-34

#building the Naive Bayes model:
#for train test, assign label: doc 1 = 1 (success), doc 2 = 1 (success), etc for 26 docs
NB_classifier <- textmodel_nb(msg.dfm.train, c(1,0,1,1,1,1,1,1,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,1))
NB_classifier
summary(NB_classifier) 	
#result: each token will have probability of closer 0 or closer to 1 → 1 = success, 0  = fail 

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
print(pred)

###########################
# OUTPUT ANALYSIS: 
#predict business success/failure based on survey response 
###########################








