#
# This is a Shiny web application. 
# January 2025 Jon May

# MCQ item analysis extracter   Jon May Febuary 2025
# ====================
#
# Faculty OCR software generates PDFs so this
# script reads the item analysis data from the


library(pdftools)   # to extract text from pdf
library(tidyverse)
library(psych)      # for descriptives
library(rio)  # file import export





#### Functions ----

# function to find text of length n immediately following a string s in text t
findstring<-function(t,s,n){
  pos<-str_locate(t,s)[1,2]  # start and end of s in t
  return(str_sub(t,pos+1, pos+n))
}


#function to extract a row of data from one page of PDF
parsepage<-function(p){
   item<-as.numeric(findstring(p, "Question ",2))
   
   accuracy<-as.numeric(findstring(p,"Mean Score:              ",5))
   
   discrimination<-as.numeric(findstring(p,"33% Item Discrimination: ",5))
   
   pbs<-as.numeric(findstring(p,"Point Biserial:          ",5))
   
   return(tibble(ITEM=item,ACC=accuracy,DISC=discrimination,PBS=pbs))
   
} #end parsepage

# given the text of a PDF file, read each items' data into a row of a tibble ITEM, ACC, DISC, PBS
readdata<-function(text){
   
   # how many sheets are there
   items<-length(text)
   
   # initialise data
   data<-tibble(ITEM=NULL,ACC=NULL,DISC=NULL,PBS=NULL) 
   
   #### loop  reading each page and adding it to data ----
   for (page  in 1:items){
      data<-rbind(data,parsepage(text[page]))
   }
   
   return(data)
}  # end readanswers



#### SHINY ----

library(shiny)

ui <- fluidPage(
   
      mainPanel(
         
         
                              htmlOutput("Intro"),
                              fileInput("pdffile", "Choose 'item analysis' PDF File", accept = ".pdf"),
                            
                              textOutput("modcode"),
                              tableOutput("itemdata"),
                              
                              downloadButton("downloadData", "Download extracted data")
                             
                     )
         
              )
   


server <- function(input, output) {
   itemanal<-reactive({
      file <- input$pdffile
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "pdf", "Please upload a pdf file"))

      text<-pdf_text(file$datapath)
      
      data<-readdata(text)
      return(data)

   })
   
  
   
    
   output$Intro<-renderUI(
      HTML("<h2>MCQ item analysis</h2>This app will read the Item Analysis PDF produced by Speedwell OCR and extract the key data for each question.<br/><br/>")
   )
   
  
   output$modcode<-renderText({
      if(is.null(input$pdffile))
         return()
      else 
      {
      modcode<- input$pdffile %>% str_extract_all("(PSYC|CPSY)\\d+") %>% unlist %>% unique
      #modcode<-paste(modcode,": ",length(itemanal())," items.")
      
      }
   })
   
 
   output$itemdata<-renderTable({
     if(is.null(input$pdffile))
       return()
     else 
     {
       return(itemanal())
       
     }
   })
   

 
 
 
 
  # Downloadable csv of scored dataset ----
  output$downloadData <- downloadHandler( 
     filename="itemanalysis.csv",
    
     content = function(file) {
        if(is.null(input$pdffile))
           return()
        else 
        {  
       
          
        export(itemanal(), file)
        }
     }
  )
 
 
 
 
} # end server

# Run the application 
shinyApp(ui = ui, server = server)
