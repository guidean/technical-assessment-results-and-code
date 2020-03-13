#Problem 1
library(pacman)
pacman::p_load(readr, phonics, dplyr, ggplot2, grid, gridExtra, lme4, car, tree, randomForest, lubridate)
#read in data
ARTICLE=read.csv("C:/Users/andre/Desktop/technical_assessment/data/articles.csv.gz", stringsAsFactors = FALSE)
AUTHOR=read.csv("C:/Users/andre/Desktop/technical_assessment/data/authors.csv.gz", encoding="UTF-8", stringsAsFactors = FALSE)

#fix problem with forenames (isolate to only first name or initial given)
AUTHOR$fore_name=as.character(AUTHOR$fore_name)
AUTHOR$first_name=sapply(strsplit(AUTHOR$fore_name, " "), `[`,1)

#remove blank values for author
AUTHOR2=na.omit(AUTHOR)
#remove accents and non-English symbols
AUTHOR2$last_name=iconv(AUTHOR2$last_name,from="UTF-8",to="ASCII//TRANSLIT")
AUTHOR2$first_name=iconv(AUTHOR2$first_name,from="UTF-8",to="ASCII//TRANSLIT")
#concatenate first annd last names together
name=c()
for (i in 1:length(AUTHOR2$pmid)){
  name[i]=paste(AUTHOR2$first_name[i], AUTHOR2$last_name[i])
}

AUTHOR2$name=name

#create the list of the top 20 most common authors
author_list=data.frame(table(AUTHOR2$name))
author_list=rename(author_list, author=Var1)
attach(author_list)
sorted_list=author_list[order(-Freq),]
detach(author_list)
most_common_authors=sorted_list[1:20,]
write.csv(most_common_authors, "C:/Users/andre/Desktop/technical_assessment/results/most_common_authors.csv")

#Problem 2
length(unique(AUTHOR$pmid)) #same number of pmid in AUTHOR and ARTICLE dataframes
num_author=data.frame(table(AUTHOR$pmid))
num_author=rename(num_author, pmid=Var1, n_authors=Freq)
num_author$pmid=as.integer(as.character(num_author$pmid))
FULL=inner_join(ARTICLE, num_author, by="pmid")

length(unique(FULL$journal)) #39 journals
unique(FULL$journal) #4 journals starting with 'Nat' or 'Cell'
nat_cell=filter(FULL, sapply(strsplit(FULL$journal, " "), `[`,1)=='Nat' | sapply(strsplit(FULL$journal, " "), `[`,1)=='Cell')
journals=unique(nat_cell$journal)
temp1=filter(nat_cell, journal==paste0(journals[1]))
temp2=filter(nat_cell, journal==paste0(journals[2]))
temp3=filter(nat_cell, journal==paste0(journals[3]))
temp4=filter(nat_cell, journal==paste0(journals[4]))
plot1=ggplot(data=temp1)+aes(x=n_authors)+geom_histogram(fill='gray', color='black') + 
  ggtitle("Distribution of the number of authors per pmid for 'Cell'")+theme(plot.title = element_text(hjust=0.5, size=10))
plot2=ggplot(data=temp2)+aes(x=n_authors)+geom_histogram(fill='gray', color='black')+ 
  ggtitle("Distribution of the number of authors per pmid for 'Cell Rep'")+theme(plot.title = element_text(hjust=0.5, size=10))
plot3=ggplot(data=temp3)+aes(x=n_authors)+geom_histogram(fill='gray', color='black')+ 
  ggtitle("Distribution of the number of authors per pmid for 'Nat Genet'")+theme(plot.title = element_text(hjust=0.5,size=10))
plot4=ggplot(data=temp4)+aes(x=n_authors)+geom_histogram(fill='gray', color='black')+ 
  ggtitle("Distribution of the number of authors per pmid for 'Nat Methods'")+theme(plot.title = element_text(hjust=0.5,size=10))
authors_per_pmid=grid.arrange(plot1, plot2, plot3, plot4)

ggsave(filename="authors_per_pmid_by_journal.pdf", plot=authors_per_pmid, device='pdf', path="C:/Users/andre/Desktop/technical_assessment/results/")

#Problem 3
time=as.integer(ymd("2020-3-20")-ymd(FULL$pub_date))

FULL$time=time
LMM=lmer(log(n_citations+1,2)~n_references+n_authors+(1|journal), data=FULL)
model_summary=data.frame(summary(LMM)$coefficients)
LMM_Anova=Anova(LMM)
significance=c()
significance[1]='-'
significance[2]=LMM_Anova$`Pr(>Chisq)`[1]
significance[3]=LMM_Anova$`Pr(>Chisq)`[2]
for (j in 2:length(significance)){
  if (as.numeric(significance[j])<.001){
    significance[j]="<.001"
  }
}
model_summary$p_value=significance

write.table(model_summary, "C:/Users/andre/Desktop/technical_assessment/results/mixed_model_summary.txt")

#Problem 4
#read in data
metadata=read.csv("C:/Users/andre/Desktop/technical_assessment/data/sample_metadata.csv", stringsAsFactors = FALSE)
expression=read.csv("C:/Users/andre/Desktop/technical_assessment/data/expression_data.csv.gz", stringsAsFactors = FALSE)

#transpose the data (get features as columns), and merge the two datasets
expression=expression[ , order(names(expression))]
transpose <- data.frame(t(expression[-1]))
colnames(transpose) <- expression[, 1]
label=metadata$label
complete=cbind(label, transpose)

#a random forest model will be used as the supervised learning algorithm
library(tree)
library(randomForest)

#split the data into training and testing sets.  Here approximately 50% of the data is used in each
set.seed(2)
train=sample(1:nrow(complete),90)
complete.test=complete[-train,]

#train the random forest using 50 decision trees.  This will also tell the misclassificaiton error
set.seed(1)
rf.complete=randomForest(as.factor(label)~.,data=complete,subset=train,mtry=50,importance=TRUE)
rf.complete

#put the testing data through the random forest, observe the classifications the random forest makes vs. actual classifications
complete.pred=predict(rf.complete, newdata=complete.test,type="class")
head(complete.pred)
library(mosaic)
testing=tally(complete.pred~complete.test$label)
testing

#View which features are the most important classifications factors in the random forest
import = importance(rf.complete)
import = as.data.frame(import)
head(arrange(import,desc(MeanDecreaseGini)))
varImpPlot(rf.complete)


