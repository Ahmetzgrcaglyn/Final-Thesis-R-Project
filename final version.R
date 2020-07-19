# turkish characters
Sys.setlocale(category = "LC_ALL", locale = "Turkish")

# libraries
library(tesseract)
library(magick)
library(dplyr)
library(tidyverse)
library(rvest)
library(stringr)
library(tabulizer)



# TASK-0: get all newspapers list ----
# get all newspapers list function
get_newspapers_list <- function(newspaper_name){
  
  # define url
  url <- sprintf("http://nek.istanbul.edu.tr:4444/ekos/GAZETE/gazete.php?gazete=%s", newspaper_name)
  # read html page
  page <- read_html(url)
  # all newspaper list
  pdf_files_list <- page %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset("\\.pdf") %>% 
    as.list(strsplit(" "))
  return(pdf_files_list)
  
}

# EXAMPLE USAGE
pdf_files_list <- get_newspapers_list(newspaper_name = "anadolu")


# download a newspaper from the list
download_newspaper <- function(list_object, number, name){
  
  # download newspaper
  download.file(list_object[[number]], name, mode="wb")
  
}

# EXAMPLE USAGE
download_newspaper(list_object = pdf_files_list, number = 1, name = "anadolu_example")



# TASK-1: get text from a page ----

# convert first page to ocr
convert_page_to_png <- function(pdf_path, dpi, pages){
  
  # convert pdf to png
  pngfile <- pdftools::pdf_convert(pdf_path, dpi = dpi, pages=pages)
  return(pngfile)
}

# EXAMPLE USAGE
png_file <- convert_page_to_png("anadolu_1935__mart_10_.pdf", dpi = 600, pages = 1)


# get text from a cropped area
get_text_from_image <- function(image_path, geometry){
  
  # read image
  image <- magick::image_read(image_path)
  # image crop
  cropped_image <- image_crop(image, geometry = geometry) %>% 
    image_convert(colorspace = 'gray')
  #cropped_image
  text <- tesseract::ocr(cropped_image)
  cat(text)
  
  
}

# EXAMPLE USAGE
get_text_from_image(image_path = "anadolu_1935__mart_10__1.png", geometry = "1500x5500+0+2900")


# TASK-2: get tabular data from another page ----
# define area and page
tabulizer::locate_areas("anadolu.pdf", pages = 8)

# get text from table
get_text_from_table <- function(pdf_path, pages, area){
  
  # get text
  text_list <- tabulizer::extract_tables(
    pdf_path,
    pages =pages,
    guess = FALSE,
    area = list(area)
  )
  return(text_list)
  
}

test <- get_text_from_table(pdf_path = "anadolu_1935__mart_10_.pdf", pages = 8, area = c(48.3538, 766.9399, 412.8919, 1116.9983))
test[[1]]
write(test[[1]], "test1.txt")


# TASK-3: get image ----
#pdftools::pdf_ocr_text("anadolu.pdf", language = "tur", pages = 1)
magic_image <- image_read("anadolu_1935__mart_10__1.png.png") %>% 
  image_crop(magic_image, geometry = "1500x5500+0+2900")













