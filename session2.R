#
# install readxl package
#
install.packages(pkgs = "readxl")
install.packages(pkgs = "stringr")
#
# load readxl package
#
library(readxl)
library(stringr)
#
# Use read_xlsx() to read apr 2015.xlsx
#
apr2015 <- read_xlsx(path = "data/apr 2015.xlsx", 
                     col_names = FALSE, 
                     skip = 3)
#
# Extract header rows from XLSX
#
headerRow <- read_xlsx(path = "data/apr 2015.xlsx",
                       col_names = FALSE, 
                       range = "A3:AH3")
#
# make headerRow into character vector
#
headerRow <- as.vector(headerRow, mode = "character")
#
# user headerRow as column names of apr2015 using names()
#
names(apr2015) <- headerRow

################################################################################
#
# Read all files in data folder and concatenate into single data object
#
#
################################################################################
#
# Get the filenames of all XLSX files in data folder
#
fileNames <- list.files(path = "data/")
#
# Create a concatenating object
#
png_maternal <- NULL
#
# Loop through each of the XLSX files in data and read them
#
for(i in fileNames) {
  #
  # Use read_xlsx() to read current filename
  #
  temp <- read_xlsx(path = paste("data/", i, sep = ""), 
                    col_names = FALSE, 
                    skip = 3)
  #
  # Extract header rows from XLSX
  #
  headerRow <- read_xlsx(path = paste("data/", i, sep = ""),
                         col_names = FALSE, 
                         range = "A3:AH3")
  #
  # make headerRow into character vector
  #
  #headerRow <- as.vector(headerRow, mode = "character")
  #
  # user headerRow as column names of apr2015 using names()
  #
  #names(temp) <- headerRow
  #
  # extract month of current data
  #
  month <- str_split(string = i, pattern = " ")[[1]][1]
  #
  # extract year of current data
  #
  year <- str_split(string = str_split(string = i, pattern = " ")[[1]][2],
                    pattern = ".xlsx")[[1]][1]
  #
  # Add month variable to temp dataset
  #
  temp$month <- month
  #
  # Add year variable to temp dataset
  #
  temp$year <- year
  #
  # concatenate current dataset with png_maternal
  #
  png_maternal <- data.frame(rbind(png_maternal, temp))
}

################################################################################
#
# Created codebook for PNG maternal mortality data
#
#
################################################################################

longName  <- c("Five-digit code of facility composed of two-digit Province code and three-digit district code",
               "Name of facility",
               "Report recieved? 1 = YES; 2 = NO",
               "New attendance breastfeeding pills",
               "New attendance combined pills",
               "New attendance injection",
               "Unkown Number 1",
               "Permanent vasectomy",
               "New attendance IUD",
               "New attendance ovulation",
               "New attendance condom",
               "Re-attendance breastfeeding pills",
               "Re-attendance combined pills",
               "Re-attendance injection",
               "Re-attendance IUD",
               "Re-attendance ovulation",
               "Re-attendance condom",
               "Antenatal first visit",
               "Antenatal fourth visit",
               "Antenatal other",
               "Antenatal TT1",
               "Antenatal TT2",
               "Antenatal booster",
               "Unknown Number 2",
               "Deliveries in health facility",
               "Maternal deaths in facility",
               "Birthweight less than 2500 grams",
               "Stillbirths",
               "Village births supervised",
               "Village births complications",
               "Born before arrival",
               "Delivery complications",
               "Maternal deaths not in facility",
               "Transferred to hospital")


shortName <- c("code", "facility", "report",
               "bfpills1", "combpills1", "inj1", "uno1", "vasectomy", "iud1", "ovulation1", "condom1",
               "bfpills2", "combpills2", "inj2", "iud2", "ovulation2", "condom2",
               "anc1", "anc4", "ancother", "tt1", "tt2", "ttbooster", "uno2",
               "delhf", "deadhf", "lbw", "still", "vbsup", "vbcomp", "bba", 
               "delcomp", "deadnothf", "transhop")

varNames <- data.frame(shortName, longName)

write.csv(varNames, "codebook.csv", row.names = FALSE)

names(png_maternal) <- c(shortName, "month", "year")


################################################################################
#
# Get province, district and facility codes
#
################################################################################

#x <- png_maternal[png_maternal$month == "apr" & png_maternal$year == 2015, ]

#pcode <- floor(x$code / 10000)

#dcode <- floor(x$code / 100)

png_maternal$pcode <- floor(png_maternal$code / 10000)
png_maternal$dcode <- floor(png_maternal$code / 100)

names(png_maternal) <- c(shortName, "month", "year", "pcode", "dcode")


################################################################################
#
# Aggregate data by province and by district per year (2015, 2016)
#
################################################################################

png_2015 <- png_maternal[png_maternal$year == 2015, ]
#
# Aggregate 2015 data by province
#
pdata2015 <- aggregate(
  cbind(bfpills1, combpills1, inj1, uno1, vasectomy, iud1, ovulation1, 
        condom1, bfpills2, combpills2, inj2, iud2, ovulation2, condom2,
        anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba, 
        delcomp, deadnothf, transhop) ~ pcode, 
  data = png_2015, FUN = sum)


ddata2015 <- aggregate(
  cbind(bfpills1, combpills1, inj1, uno1, vasectomy, iud1, ovulation1, 
        condom1, bfpills2, combpills2, inj2, iud2, ovulation2, condom2,
        anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba, 
        delcomp, deadnothf, transhop) ~ dcode, 
  data = png_2015, FUN = sum)



