
rm(list = ls())
graphics.off()


library(pdftools)
library(tesseract)
library(stringr)
library(dplyr)

pdf_file <- ("RPT-227872.pdf")

#image_file <- pdftools::pdf_convert(pdf_file, pages = 1, dpi =300)

#text <- tesseract::ocr(image_file)

#cat(text)

#Extract structured data from the PDF
pdf_data <- pdf_data(pdf_file)

#Extract the data from the first page
page_data <- pdf_data[[1]]

# Convert the structured data to a dataframe
df <- as.data.frame(page_data)

head(df)

print(df$text)
#1. Absolute average value
#Adjust the pattern based on what you found in df, assuming "Avg" instead of "Absolute Average"
absolute_avg_row <- df[grep("Avg", df$text), ]

print(absolute_avg_row)


# Once you locate the correct line, extract the value next to it

if(nrow(absolute_avg_row) > 0){
  # Get the position of the text (adjust as needed)
  avg_position <- absolute_avg_row$y[1]
  
  # Find the number on the same line 
  avg_value_row <- df[df$y ==avg_position & grepl("[0-9]+", df$text), ]
  
  # Output the Absolute Average value
  absolute_avg_value <- avg_value_row$text[2]
  cat("Absolute Average Value: ", absolute_avg_value, "\n")
}
absolute_avg_value <- paste0(absolute_avg_value, "%")  

######

pdf_text <- pdf_text(pdf_file)

cat(pdf_text[1])

#2. LIMS number

lims_mm_pattern <- "LIMS MM Sample\\s*(\\d+)"
lims_mm_sample <- regmatches(pdf_text[1], regexpr(lims_mm_pattern, pdf_text[1]))

lims_mm_value <- gsub("LIMS MM Sample\\s*", "", lims_mm_sample)

cat("LIMS report number ", lims_mm_value, "\n")

#3. NIR QC

nir_qc_pattern <- "QC\\d+"
nir_qc <- regmatches(pdf_text[1], regexpr(nir_qc_pattern, pdf_text[1]))


cat("NIR ", nir_qc, "\n")

##################################################################################

# Using officer to Modify DocX

library(officer)

doc <- read_docx("RPT-213947.docx")


doc_text <- docx_summary(doc)$text

#Find the old NIR QC# value automatically
old_nir_qc_pattern <- "QC\\d+"

old_nir_qc <- regmatches(doc_text, regexpr(old_nir_qc_pattern, doc_text))

old_nir_qc <- regmatches(doc_text, regexpr(old_nir_qc_pattern, doc_text))
if (length(old_nir_qc) == 0) stop("Old NIR QC# value not found.")
old_nir_qc <- old_nir_qc[1]

# If the old QC number appears near certain text (e.g., "NIR Spectrometer"), we can use that context
old_nir_qc_pattern <- "NIR Spectrometer:\\s*QC\\d+"  # Refine the pattern to look for "NIR Spectrometer" followed by the QC number
old_nir_qc <- regmatches(doc_text, regexpr(old_nir_qc_pattern, doc_text))

                                                                                                                           if (length(old_nir_qc) == 0) stop("Old NIR QC# value not found.")
# Strip out everything but the QC number
old_nir_qc <- sub("NIR Spectrometer:\\s*", "", old_nir_qc)
cat("Old NIR QC#: ", old_nir_qc, "\n")  # Debug: print old QC value



#Find the old LIMS # value automatically

old_lims_mm_pattern <- "LIMS report number\\s*(\\d+)"
#\\d{7,}

old_lims_mm_sample <- regmatches(doc_text, regexpr(old_lims_mm_pattern, doc_text))


old_lims_mm_pattern <- "LIMS report number\\s*(\\d+)"  # Case-insensitive match with flexible spaces
old_lims_mm_sample <- regmatches(doc_text, regexpr(old_lims_mm_pattern, doc_text, perl=TRUE))


# Debug: Print the old LIMS MM Sample value for verification
cat("Old LIMS MM Sample: ", old_lims_mm_sample, "\n")



# Locate the old Absolute Average value
old_absolute_avg_pattern <- "\\d+%"  # Look for a number followed by a percentage sign
old_absolute_avg <- regmatches(doc_text, regexpr(old_absolute_avg_pattern, doc_text))


# Step 3: Replace values in the DOCX file

# 1. Replace NIR QC# value
doc <- body_replace_all_text(doc, old_value = old_nir_qc, new_value = nir_qc)

# 2. Replace LIMS MM Sample value
doc <- body_replace_all_text(doc, old_value = old_lims_mm_sample, new_value = lims_mm_value)

docx_show_chunk(doc)

# 3. Replace Absolute Average value
doc <- body_replace_all_text(doc, old_value = old_absolute_avg, new_value = absolute_avg_value)

# Step 4: Save the updated DOCX file
print(doc, target = output_docx)

# Output the results to the console (optional)
cat("Old NIR QC#: ", old_nir_qc, "\n")
cat("Old LIMS MM Sample: ", old_lims_mm_sample, "\n")
cat("Old Absolute Average: ", old_absolute_avg, "\n")
cat("New NIR QC#: ", nir_qc, "\n")
cat("New LIMS MM Sample: ", lims_mm_value, "\n")
cat("New Absolute Average: ", absolute_avg_value, "\n")

                


