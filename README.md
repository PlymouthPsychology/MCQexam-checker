# MCQ exam-checker
Extracts data from OCR generated PDFs to check MCQ examination scoring

 Faculty OCR software generates PDFs so this
 script reads student raw answerr data from the 'results breakdown' file
 and the 'correct answers' used from the 'correct answers' file to 
 extract the raw data for our own use
 This potentially allows us to check how scores would change if poor items
 were removed from the test.

 Tested on PSYC422 in January 2025 to confirm that it gives same
 answers as the Speedwell OCR.

 As the rejected items are answered at random removing them has little effect
 on rank order or candidates but would increase %age correct slightly

# HOW TO USE
 Just set Working Directory to the folder containing the PDFs and run MCQ.R

 Jon May January 2025
