##Basic NLP - Keyword Extraction in R Udpipe package
#scotus package - source data  for Supreme Court Opinions
#tm package for cleaning
#dplyr and lattice wrangling and plots

#scotus install
install.packages("devtools")
devtools::install_github("EmilHvitfeldt/scotus")



#load packages
-----------------
library(dplyr)
library(udpipe)
library(lattice)
library(tm)
library(scotus)


##wrangling
data <- scotus_sample
head(data)

##filter on just one case
data2 <- data %>% 
  filter(case_name== 'United States v. Johnson' & year =='2000')

##cleaning optional
#data2$text <- removePunctuation(data2$text)
#data2$text <-  tolower(data2$text)
#data2$text <- removeNumbers(data2$text)
#words2remove <- stopwords(kind = 'en')
#data2$text <- removeWords(data2$text, words2remove)
#data2$text <- gsub("\n","",x = data2$text)
#data2$text <- stripWhitespace(data2$text)
#data2$text <- gsub("§","", data2$text)


##model annotation with standard english (Udpipe)

ud_model <- udpipe_download_model(language = "english")
ud_model <-  udpipe_load_model(ud_model$file_model)
u_annot <- udpipe_annotate(ud_model, x=data2$text)
x <- as.data.frame(u_annot)

# universal parts of speech basic stats
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "dodgerblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## top occurring nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring nouns", xlab = "Freq")

#top occurring adjectives

stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "tomato", 
         main = "Most occurring adjectives", xlab = "Freq")


##keywords using RAKE (rapid automatic keyword extraction) algo.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 1), 20), col = "dodgerblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")



##cooccurences to see how words are used together
cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)


