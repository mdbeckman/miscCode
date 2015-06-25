

# genfiles is just a folder location where all of the files reside
genfiles <- "c:/genfiles/"
setwd(genfiles)

# b gets all of the file names in the folder
b <- list.files()
# I needed to sort the files in alphabetical order
a <- sort(b)

strlength <- nchar(as.character(a))


# I loop through the file (names listed in a)
# the data in the files I read has a | delimiter
for(r in 1:length(a)){
  filename <- paste(genfiles, a[r], sep="")
  d <- read.table(filename, sep="|")
  
  # do more stuff here
  
}
