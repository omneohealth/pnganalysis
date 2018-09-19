if(!require(devtools, quietly = TRUE)) install.packages("devtools") # If devtools required but not installed, install
install_github("OMNeoHealth/papuanewguinea")                        # Install OMNeoHealth/papuanewguinea from GitHub
library(papuanewguinea) 
if(!require(readxl)) install.packages("readxl")   # to read Excel files
if(!require(stringr)) install.packages("stringr") # to manipulate strings

pop_adm1     # province level population
pop_adm2     # district level population
pop_adm3     # local government population

#Get the filenames of all .XLSX files in folder named "data"
#
fileNames <- list.files(path = "data/")
##Create a concatenating object
#
png_maternal <- NULL
##Loop through each of the XLSX files in data and read them
#
for(i in fileNames) {
  ##Use read_xlsx() to read current filename
  #
  temp <- read_xlsx(path = paste("data/", i, sep = ""),
                    col_names = FALSE,
                    skip = 3)
  ##extract month of current data
  #
  month <- str_split(string = i, pattern = " ")[[1]][1]
  ##extract year of current data
  #
  year <- str_split(string = str_split(string = i, pattern = " ")[[1]][2],
                    pattern = ".xlsx")[[1]][1]
  ## Add month variable to temp dataset
  #
  temp$month <- month
  #
  # Add year variable to temp dataset
  #
  temp$year <- year
  ##concatenate current dataset with png_maternal
  #
  png_maternal <- data.frame(rbind(png_maternal, temp))
}

################################################################################
##Get province, district and facility codes
#
################################################################################
##Extract first two digits from code
#
png_maternal$pcode <- floor(png_maternal$X__1 / 10000)
##pad the pcode with a 0 at the start
#
png_maternal$pcode <- str_pad(string = png_maternal$pcode,
                              width = 2, side = "left", pad = "0")
##Extract first 4 digits from code
#
png_maternal$dcode <- floor(png_maternal$X__1 / 100)
##pad the dcode with a 0 at the start
#
png_maternal$dcode <- str_pad(string = png_maternal$dcode,
                              width = 4, side = "left", pad = "0")
##pad the code with a 0 at the start
#
png_maternal$X__1 <- str_pad(string = png_maternal$X__1,
                             width = 6, side = "left", pad = "0")
################################################################################
##Created codebook for PNG maternal mortality data
#
################################################################################
longName <- c("Five to six-digit facility code",
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
              "Transferred to hospital",
              "Month",
              "Year",
              "Province code",
              "District code")
shortName <- c("code", "facility", "report",
               "bfpills1", "combpills1", "inj1", "uno1", "vasectomy", "iud1",
               "ovulation1", "condom1", "bfpills2", "combpills2", "inj2",
               "iud2", "ovulation2", "condom2", "anc1", "anc4", "ancother",
               "tt1", "tt2", "ttbooster", "uno2", "delhf", "deadhf", "lbw",
               "still", "vbsup", "vbcomp", "bba", "delcomp", "deadnothf",
               "transhop", "month", "year", "pcode", "dcode")
names(png_maternal) <- shortName

##Aggregate data by province and per year
#
provincedata <- aggregate(
  cbind(bfpills1, combpills1, inj1, uno1, vasectomy, iud1, ovulation1,
        condom1, bfpills2, combpills2, inj2, iud2, ovulation2, condom2,
        anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba,
        delcomp, deadnothf, transhop) ~ pcode + year,
  data = png_maternal, FUN = sum)

##Aggregate data by district and per year
#
districtdata <- aggregate(
  cbind(bfpills1, combpills1, inj1, uno1, vasectomy, iud1, ovulation1,
        condom1, bfpills2, combpills2, inj2, iud2, ovulation2, condom2,
        anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba,
        delcomp, deadnothf, transhop) ~ dcode + year,
  data = png_maternal, FUN = sum)

## orginise population data to be merge with province data using the women reproductive age
#
##for provinve
wra_adm1 <- pop_adm1[ , c("ADM1_PCODE", "ADM1_EN", "WRA")]

##for district
wra_adm2 <- pop_adm2[ , c("ADM2_PCODE", "ADM2_EN", "ADM1_PCODE", "ADM1_EN", "WRA")]

## organise pop data to have a sequential pcode and dcode that matchs the dataset
wra_adm1 <- wra_adm1[order(wra_adm1$ADM1_PCODE), ]
wra_adm2 <- wra_adm2[order(wra_adm2$ADM2_PCODE), ]

## remove the PG infront of the pcode in the pop data to match pcode of the main dataset
wra_adm1$ADM1_PCODE <- as.numeric(str_replace(wra_adm1$ADM1_PCODE, "PG", ""))
wra_adm2$ADM2_PCODE <- as.numeric(str_replace(wra_adm2$ADM2_PCODE, "PG", ""))
wra_adm2$ADM1_PCODE <- as.numeric(str_replace(wra_adm2$ADM1_PCODE, "PG", ""))

##calculate a standardising factor which we will call sf. Using the population size for women of reproductive age (WRA), we divide this by 100,000 to get a standardising factor that will give an indicator value that is per 100,000 WRA population.
wra_adm1$sf <- wra_adm1$WRA / 100000
wra_adm2$sf <- wra_adm2$WRA / 100000

## merge pop standardised factor (sf) data with province and district data
# for province
provincedata <- merge(wra_adm1,
                      provincedata,
                      by.x = "ADM1_PCODE",
                      by.y = "pcode")
#for district
x <- colSums(districtdata[districtdata$dcode %in% c(401, 402, 403) &
                            districtdata$year == 2015, ])
y <- colSums(districtdata[districtdata$dcode %in% c(401, 402, 403) &
                            districtdata$year == 2016, ])
xy <- rbind(x, y)
xy[1,1] <- 401
xy[2,1] <- 401
xy[1,2] <- 2015

xy[2,2] <- 2016
districtdata <- data.frame(rbind(
  districtdata[!districtdata$dcode %in% c(401, 402, 403), ], xy))

districtdata <- merge(wra_adm2,
                      districtdata,
                      by.x = "ADM2_PCODE",
                      by.y = "dcode")

###################################################################
#map maternal mortality
###################################################################
#
pdata2015 <- pdata[pdata$year == 2015, ]
# Add deadhf + deadnothf for total deaths
#
pdata2015$totaldead <- pdata2015$deadhf + pdata2015$deadnothf
#
## normalised maternal deaths per province per WRA/ 100 000 (sf)
deathprov2015 <- (pdata2015$totaldead*wra_adm1$sf)

# map maternal deaths per province normalised by WRA x 100000
#
#Install Class package to create quntile class
install.packages("classInt")
library(classInt)

# first create classification of maternal deaths
deathprov2015class <- cut (x = deathprov2015,
                      breaks = classIntervals(deathprov2015,
                      n = 5, style = "quantile") $brks, 
                      labels = FALSE)

##Recode NA class to 0
deathprov2015class <- ifelse(is.na(deathprov2015class), 0, deathprov2015class)
#
##################################################
##map maternal deaths per province 2015
##################################################
#
colourscheme <- c("#eff3ff", "#c6dbef", "#9ecae1",
                  "#6baed6", "#3182bd", "#08519c")
plot(province,
     col = colourscheme[deathprov2015class + 1],
     border = "gray90",
     lwd = 0.5)