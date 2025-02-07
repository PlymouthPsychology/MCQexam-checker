# MCQscaleR.R
Extracts data from OCR generated PDFs to check MCQ examination scoring
and allows you to scale to grades by setting min A+ and min Pass

 Faculty OCR software generates PDFs so this
 shiny app reads student raw answer data from the 'results breakdown' file
 and the 'correct answers' used from the 'correct answers' file to 
 extract the raw data for our own use
 This potentially allows us to check how scores would change if poor items
 were removed from the test.

 # MCQitems.R
 Reads the item analysis PDF and extracts data for each item to allow
 staff to identify weak items.

 Jon May January 2025
