
# MCQ score checker   Jon May January 2025
# ====================
#
# Faculty OCR software generates PDFs so this
# script reads student raw answerr data from the 'results breakdown' file
# and the 'correct answers' used from the 'correct answers' file to 
# extract the raw data for our own use
# This potentially allows us to check how scores would change if poor items
# were removed from the test.
#
# Tested on PSYC422 in January 2025 to confirm that it gives same
# answers as the Speedwell OCR.
#
# As the rejected items are answered at random removing them has little effect
# on rank order or candidates but would increase %age correct slightly
#
# HOW TO USE
# Just set Working Directory to the folder containing the PDFs and run.





library(pdftools)   # to extract text from pdf
library(tidyverse)
library(psych)      # for descriptives



# wipe environment
rm(list = ls())

#function to extract data from a page of PDF
parsepage<-function(p){
   srnpos<-str_locate(p, "Candidate Number: ")[1,2]
   srn<-str_sub(p,srnpos+1, srnpos+8)
   
   z<-str_length(p)
   x<-str_locate(p,"Response Correct\n\n")
   t<-str_sub(p,x[2], z)
   
   t2<-str_remove_all(t," ")
   t3<-str_remove_all(t2,"Yes")
   t4<-str_remove_all(t3,"No")
   t5<-str_remove_all(t4,"\n")
   
   
   t10<-str_remove_all(t5,"[1234567890]")
   t11<-str_sub(t10,1,99)
   
   return(tibble(SRN=srn,DATA=t11))
   
}

#### read PDFs of answers and scoring key

# find the files in the working directory
breakdown<-list.files(pattern="results breakdown.pdf")
correctanswers<-list.files(pattern="results correct answers.pdf")




#### read the file with a page for each student ----
#pdf_file<-file.path("PSYC422 results","PSYC422 results breakdown.pdf")
text<-pdf_text(breakdown)

# how many sheets are there
students<-length(text)


# initialise data
data<-tibble(SRN=NULL,DATA=NULL) 

#### loop  reading each page and adding it to data ----
for (page  in 1:students){
   data<-rbind(data,parsepage(text[page]))
}

# remember that the answers are in order 1, 36, 71, 2, 37, 72...
# 
itemnames<-paste0("Q",c(1,36,71,
         2,37,72,
         3,38,73,
         4,39,74,
         5,40,75,
         6,41,76,
         7,42,77,
         8,43,78,
         9,44,79,
         10,45,80,
         11,46,81,
         12,47,82,
         13,48,83,
         14,49,84,
         15,50,85,
         16,51,86,
         17,52,87,
         18,53,88,
         19,54,89,
         20,55,90,
         21,56,91,
         22,57,92,
         23,58,93,
         24,59,94,
         25,60,95,
         26,61,96,
         27,62,97,
         28,63,98,
         29,64,99,
         30,65,
         31,66,
         32,67,
         33,68,
         34,69,
         35,70))

# each answer has width 1
items<-c(rep(1,99))
#create a named list of items each width width 1
nameditems<-set_names(items,itemnames)

# used the named listr to separate the DATA string into 99 correctly named columns
data<-separate_wider_position(data,DATA,nameditems) %>% pivot_longer(-SRN)

# thats it. could now add scoring etc.

#### read the file with the correct answers ----
#pdf_file<-file.path("PSYC422 results","PSYC422 results correct answers.pdf")
text<-pdf_text(correctanswers)


t<-tibble(text)   # convert text to a tibble
t2<-separate_wider_delim(t, text,delim="\n",names_sep="_", too_few="align_start") # put each line on page into new column
t3<-pivot_longer(t2, text_10:text_52)  # make the page long 
t4<-t3 %>% select(value) %>% filter(!value=="")  # remove blank lines
t5<-t4 %>% mutate(value=str_replace(value," ","_"),value=str_replace_all(value," ","")) # make the first space between item and answers and underscore and remove the others
t6<-separate_wider_delim(t5,value,"_",names=c("Q","Ans"),too_few = "align_start") # split off the item number Q from the answer key  Ans
scoringkey<-t6 %>% mutate(Q=paste0("Q",Q),   # add a Q to the front of the item number 
                  key=str_sub(Ans,1,4),      # get the first four chars of the answer key
                  correct=case_when(         # decode the answer key
                    key=="1000" ~ "A",
                    key=="0100" ~ "B",
                    key=="0010" ~ "C",
                    key=="0001" ~ "D",
                    T ~ NA)
                  ) %>% 
  filter(!is.na(correct)) %>%           # remove garbage lines from end of page
  select(Q,correct)      # just keep the  item number and the correct answer

# now we have an answer key to join onto a long version of the student answers

scored<-left_join(data, scoringkey, join_by(name == Q)) %>%   # merge answers with key
  mutate(score=if_else(value==correct,1,0))                   # if answer chosen matches correct


#### score the MCQ ----

# compute overall Sum and PC

student_scores<-scored %>% group_by(SRN) %>% 
  summarise(Total=sum(score),PC=round(100*Total/n(),2))

####  item analysis ----

# point biserial correlation
# this requires student total on test and answer for each item
# first join the totals to the raw data

pbs_data<-left_join(scored,student_scores)

# now find the correlation estimates for each item to 3 dp
pbs_results <- pbs_data %>% 
  group_by(name) %>% 
  summarize(pbs=list(cor.test(score, Total)$estimate)) %>% 
  mutate(pbs=round(as.numeric(pbs),3))

# 33% Item discrimination
# we have to allocate students to the top and bottom tertiles

tertiles<-student_scores %>% mutate(tertile=ntile(Total,3)) %>% select(SRN, tertile)

tertiles<-left_join(scored,tertiles)

tertiles_results<-tertiles %>% group_by(name,tertile) %>% 
  summarise(Accuracy=sum(score)/n()) %>% 
  pivot_wider(names_from = tertile, names_prefix = "T", values_from = Accuracy) %>% 
  mutate(disc=round(T3-T1,3)) %>% 
  select(name,disc)

# this may differ slightly from pdf as ties are ignored and ranked order of candidates is ambiguous
# so speedwell might use different students at tertile boundaries

# we can join these two together now
item_statistics<-left_join(pbs_results,tertiles_results) %>% 
  mutate(rating=case_when(
    pbs<.1 & disc <.1 ~ "reject",   # if both indicators low
    pbs<.1 | disc <.1 ~ "weak" ,    # else if either one low
    pbs>.3 & disc >.3 ~ "good" ,    # else if both high
    T ~ "ok"                        # else mid/mid high/mid  mid/high
  ))

# if one of these stats is <.1, the item is weak
# if both are then the item is rejected

#### rescore without rejected items ----
rescored<-left_join(scored,
                    item_statistics %>% select(name, rating)) %>% 
  filter(!rating=="reject") %>% 
  group_by(SRN) %>% 
  summarise(RescoredTotal=sum(score),RescoredPC=round(100*RescoredTotal/n(),2))

# combine with original to see effects
student_scores<-left_join(student_scores, rescored)  

student_scores %>% ggplot(aes(x=Total,y=RescoredTotal))+
  geom_point()+
  theme_minimal()

cor.test(student_scores$Total, student_scores$RescoredTotal)

# not worth the effort!

#### report descriptives to console ----

describe(student_scores)

#### (end) ----


