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
pdata2016 <- pdata[pdata$year == 2016, ]
# Add deadhf + deadnothf for total deaths
#
pdata2015$totaldead <- pdata2015$deadhf + pdata2015$deadnothf
pdata2016$totaldead <- pdata2016$deadhf + pdata2016$deadnothf
#
## normalised maternal deaths per province per WRA/ 100 000 (sf)
deathprov2015 <- (pdata2015$totaldead*wra_adm1$sf)
deathprov2016 <- (pdata2016$totaldead*wra_adm1$sf)
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

deathprov2016class <- cut (x = deathprov2016,
                           breaks = classIntervals(deathprov2016,
                                                   n = 5, style = "quantile") $brks, 
                           labels = FALSE)
##Recode NA class to 0
deathprov2015class <- ifelse(is.na(deathprov2015class), 0, deathprov2015class)
deathprov2016class <- ifelse(is.na(deathprov2016class), 0, deathprov2016class)
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
title(main = "Maternal deaths 2015", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(deathprov2015,
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

###########################################################################
# map mat deaths per province 2016
###########################################################################
plot(province,
     col = colourscheme[deathprov2016class + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Maternal deaths 2016", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(deathprov2016,
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

#######################################################################
#plot both mat deaths 2015, 2016 maps side by side
#######################################################################
par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
plot(province,
     col = colourscheme[deathprov2015class + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Maternal deaths 2015", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(deathprov2015,deathprov2015),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

plot(province,
     col = colourscheme[deathprov2016class + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Maternal deaths 2016", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(deathprov2015,deathprov2015),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

#########################################################################
# map stillbirths per province 
#########################################################################
#
## normalised stillbirths per province per WRA/ 100 000 (sf)
still2015p <- (pdata2015$still/pop_adm1$WRA) * 100000
still2016p <- (pdata2016$still/pop_adm1$WRA) * 100000
#
# map stillbirths per province normalised by WRA x 100000
#
# first create classification of stillbirths
still2015pclass <- cut (x = still2015p,
                           breaks = classIntervals(still2015p,
                                                   n = 5, style = "quantile") $brks, 
                           labels = FALSE)

still2016pclass <- cut (x = still2016p,
                        breaks = classIntervals(still2016p,
                                                n = 5, style = "quantile") $brks, 
                        labels = FALSE)
##Recode NA class to 0
still2015pclass <- ifelse(is.na(still2015pclass), 0, still2015pclass)
still2016pclass <- ifelse(is.na(still2016pclass), 0, still2016pclass)

###################################################################
# plot side by side stillbirths map 2015, 2016 per province
###################################################################
par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
plot(province,
     col = colourscheme[still2015pclass + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Stillbirths 2015", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(still2015p, still2015p),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                                 between = "to",
                                                 cutlabels = FALSE))),
                                                 pch = 15, pt.cex = 2,
                                                  col = colourscheme)

plot(province,
     col = colourscheme[still2016pclass + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Stillbirths 2016", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(still2015p, still2015p),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                                   between = "to",
                                                 cutlabels = FALSE))),
                                                  pch = 15, pt.cex = 2,
                                                    col = colourscheme)

#########################################################################
# map LBW per province 
#########################################################################
#
## normalised LBW per province per WRA/ 100 000 (sf)
lbw2015p <- (pdata2015$lbw/pop_adm1$WRA) * 100000
lbw2016p <- (pdata2016$lbw/pop_adm1$WRA) * 100000
#
# map lbw per province normalised by WRA x 100000
#
# first create classification of LBW
lbw2015pclass <- cut (x = lbw2015p,
                        breaks = classIntervals(lbw2015p,
                                                n = 5, style = "quantile") $brks, 
                        labels = FALSE)

lbw2016pclass <- cut (x = lbw2016p,
                        breaks = classIntervals(lbw2016p,
                                                n = 5, style = "quantile") $brks, 
                        labels = FALSE)
##Recode NA class to 0
lbw2015pclass <- ifelse(is.na(lbw2015pclass), 0, lbw2015pclass)
lbw2016pclass <- ifelse(is.na(lbw2016pclass), 0, lbw2016pclass)

###################################################################
# plot side by side LBW map 2015, 2016 per province
###################################################################
par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
plot(province,
     col = colourscheme[lbw2015pclass + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Low Birth Weight 2015", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(lbw2015p, lbw2015p),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

plot(province,
     col = colourscheme[lbw2016pclass + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Low Birth Weight 2016", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(lbw2015p, lbw2015p),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

#########################################################################
# map delivered in health facility per province 
#########################################################################
#
## normalised delhf per province per WRA/ 100 000 (sf)
delhf2015p <- (pdata2015$delhf/pop_adm1$WRA) * 100000
delhf2016p <- (pdata2016$delhf/pop_adm1$WRA) * 100000
#
# map delhf per province normalised by WRA x 100000
#
# first create classification of delhf
delhf2015pclass <- cut (x = delhf2015p,
                      breaks = classIntervals(delhf2015p,
                                              n = 5, style = "quantile") $brks, 
                      labels = FALSE)

delhf2016pclass <- cut (x = delhf2016p,
                      breaks = classIntervals(delhf2016p,
                                              n = 5, style = "quantile") $brks, 
                      labels = FALSE)
##Recode NA class to 0
delhf2015pclass <- ifelse(is.na(delhf2015pclass), 0, delhf2015pclass)
delhf2016pclass <- ifelse(is.na(delhf2016pclass), 0, delhf2016pclass)

###################################################################
# plot side by side delhf map 2015, 2016 per province
###################################################################
par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
plot(province,
     col = colourscheme[delhf2015pclass + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Delivered in health facility 2015", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(delhf2015p, delhf2015p),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)

plot(province,
     col = colourscheme[delhf2016pclass + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "Delivered in health facility 2016", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.1,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(c(delhf2015p, delhf2015p),
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0),
                                   between = "to",
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2,
       col = colourscheme)


###################################################################################
# create trends by months
###################################################################################
##Aggregate data by month and per year
#
mProvince <- aggregate(
  cbind(bfpills1, combpills1, inj1, uno1, vasectomy, iud1, ovulation1,
        condom1, bfpills2, combpills2, inj2, iud2, ovulation2, condom2,
        anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba,
        delcomp, deadnothf, transhop) ~ month + pcode + year,
  data = png_maternal, FUN = sum)

##Aggregate data by district and per year
#
mDistrict <- aggregate(
  cbind(bfpills1, combpills1, inj1, uno1, vasectomy, iud1, ovulation1,
        condom1, bfpills2, combpills2, inj2, iud2, ovulation2, condom2,
        anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba,
        delcomp, deadnothf, transhop) ~ month + dcode + year,
  data = png_maternal, FUN = sum)

########### Convert dcode to numeric
mDistrict$dcode <- as.numeric(mDistrict$dcode)
mDistrict$year <- as.numeric(mDistrict$year)

#merge these datasets with the population data that contains the standardising factor.
# for monthly province
mProvince <- merge(wra_adm1, mProvince, by.x = "ADM1_PCODE", by.y = "pcode")

#for monthly district
x <- aggregate(
  cbind(dcode, year, bfpills1, combpills1, inj1, uno1, vasectomy, iud1,
        ovulation1, condom1, bfpills2, combpills2, inj2, iud2, ovulation2,
        condom2, anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba, delcomp, deadnothf,
        transhop) ~ month,
  data = mDistrict[mDistrict$dcode %in% c(401, 402, 403) &
                     mDistrict$year == 2015, ],
  FUN = sum)
y <- aggregate(
  cbind(dcode, year, bfpills1, combpills1, inj1, uno1, vasectomy, iud1,
        ovulation1, condom1, bfpills2, combpills2, inj2, iud2, ovulation2,
        condom2, anc1, anc4, ancother, tt1, tt2, ttbooster, uno2,
        delhf, deadhf, lbw, still, vbsup, vbcomp, bba, delcomp, deadnothf,
        transhop) ~ month,
  data = mDistrict[mDistrict$dcode %in% c(401, 402, 403) &
                     mDistrict$year == 2016, ],
  FUN = sum)
xy <- rbind(x, y)
xy$dcode <- 401
xy[ 1:12, 3] <- 2015
xy[13:24, 3] <- 2016
#mDistrict <- subset (mDistrict, select = -year)
mDistrict <- data.frame(rbind(
  mDistrict[!mDistrict$dcode %in% c(401, 402, 403), ], xy))

mDistrict <- merge(wra_adm2, mDistrict, by.x = "ADM2_PCODE", by.y = "dcode")

#######################################################
# ANC1 and temporal trends
#
######################################################
#install zoo, tidyr, ggplot2, dplyr package to run the functions
#######################################################
install.packages("zoo")
library(zoo)

install.packages("tidyr")
library(tidyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)



temp1 <- aggregate(anc1 ~ ADM1_PCODE + ADM1_EN + month + year,
                   data = mProvince,
                   FUN = sum)
temp2 <- aggregate(sf ~ ADM1_PCODE + ADM1_EN + month + year,
                   data = mProvince,
                   FUN = unique)
temp1$anc1Std <- temp1$anc1 / temp2$sf
temp1$month <- as.character(temp1$month)
temp1$month[temp1$month == "jan"] <- 1
temp1$month[temp1$month == "feb"] <- 2
temp1$month[temp1$month == "mar"] <- 3
temp1$month[temp1$month == "apr"] <- 4
temp1$month[temp1$month == "may"] <- 5
temp1$month[temp1$month == "jun"] <- 6
temp1$month[temp1$month == "jul"] <- 7
temp1$month[temp1$month == "aug"] <- 8
temp1$month[temp1$month == "sep"] <- 9
temp1$month[temp1$month == "oct"] <- 10
temp1$month[temp1$month == "nov"] <- 11
temp1$month[temp1$month == "dec"] <- 12
temp1$date <- paste(temp1$year, temp1$month, sep = "-")
temp1$date <- zoo::as.yearmon(temp1$date)
temp1 <- temp1[order(temp1$date), ]
temp1 <- temp1 %>%
  group_by(ADM1_EN) %>%
  mutate(anc1Sm = rollmean(x = anc1Std, k = 3, na.pad = TRUE))
temp1long <- gather(data = temp1,
                    key = "anc1",
                    value = "value",
                    anc1Std, anc1Sm,
                    factor_key = TRUE)
themeSettings <- theme_bw() +
  theme(panel.grid.major = element_line(linetype = 1,
                                        size = 0.2,
                                        colour = "gray80"),
        axis.text.x = element_text(size = 6, angle = 90),
        legend.key = element_rect(linetype = 0),
        legend.key.size = unit(1, "cm"),
        legend.position = "top")

ggplot(temp1, aes(as.Date(date), anc1Std)) +
  geom_line() +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b %y") +
  scale_y_continuous(name = "ANC1",
                     breaks = seq(from = 0,
                                  to = max(temp1$anc1Std),
                                  by = 500)) +
  facet_wrap(vars(ADM1_EN)) +
  themeSettings

####################################
# plot trends per district
###################################

dist1 <- aggregate(anc1 ~ ADM2_PCODE + ADM2_EN + ADM1_PCODE + ADM1_EN + month + year,
                   data = mDistrict,
                   FUN = sum)
dist2 <- aggregate(sf ~ ADM2_PCODE + ADM2_EN + ADM1_PCODE + ADM1_EN + month + year,
                   data = mDistrict,
                   FUN = unique)
dist1$anc1Std <- dist1$anc1 / dist2$sf
dist1$month <- as.character(dist1$month)
dist1$month[dist1$month == "jan"] <- 1
dist1$month[dist1$month == "feb"] <- 2
dist1$month[dist1$month == "mar"] <- 3
dist1$month[dist1$month == "apr"] <- 4
dist1$month[dist1$month == "may"] <- 5
dist1$month[dist1$month == "jun"] <- 6
dist1$month[dist1$month == "jul"] <- 7
dist1$month[dist1$month == "aug"] <- 8
dist1$month[dist1$month == "sep"] <- 9
dist1$month[dist1$month == "oct"] <- 10
dist1$month[dist1$month == "nov"] <- 11
dist1$month[dist1$month == "dec"] <- 12
dist1$date <- paste(dist1$year, dist1$month, sep = "-")
dist1$date <- zoo::as.yearmon(dist1$date)

###############################################################################
#plot ANC1 per district in central province
################################################################################
ggplot(dist1[dist1$ADM1_PCODE == 1, ], aes(as.Date(date), anc1Std)) +
  geom_line(colour = "#08519c", size = 1) +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b %y") +
  scale_y_continuous(name = "ANC1",
                     breaks = seq(from = 0,
                                  to = max(dist1$anc1Std),
                                  by = 500)) +
  facet_grid(ADM1_EN ~ ADM2_EN) + themeSettings

ggplot(dist1[dist1$ADM1_PCODE == 4, ], aes(as.Date(date), anc1Std)) +
  geom_line(colour = "#08519c", size = 1) +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b %y") +
  scale_y_continuous(name = "ANC1",
                     breaks = seq(from = 0,
                                  to = max(dist1$anc1Std),
                                  by = 100)) +
  facet_grid(ADM1_EN ~ ADM2_EN) +
  themeSettings
#
# smooth the trends using rollmean to create averages
#
#
temp1 <- temp1[order(temp1$date), ]
temp1 <- temp1 %>%
  group_by(ADM1_EN) %>%
  mutate(anc1Sm = rollmean(x = anc1Std, k = 3, na.pad = TRUE))
temp1long <- gather(data = temp1,
                    key = "anc1",
                    value = "value",
                    anc1Std, anc1Sm,
                    factor_key = TRUE)

#plot by province

ggplot(temp1long, aes(as.Date(date), value, colour = anc1)) +
  geom_line() +
  scale_colour_manual(labels = c("raw", "smooth"),
                      values = c("#e41a1c", "#377eb8")) +
  scale_x_date(name = "Month",
               date_breaks = "1 month",
               date_labels = "%b %y") +
  scale_y_continuous(name = "ANC1",
                     breaks = seq(from = 0,
                                  to = max(temp1$anc1Std),
                                  by = 500)) +
  facet_wrap(vars(ADM1_EN)) +
  themeSettings

#
# smooth for district data
#
dist1 <- dist1[order(dist1$date), ]
dist1 <- dist1 %>%
  group_by(ADM2_EN) %>%
  mutate(anc1Sm = rollmean(x = anc1Std, k = 3, na.pad = TRUE))
dist1long <- gather(data = dist1,
                    key = "anc1",
                    value = "value",
                    anc1Std, anc1Sm,
                    factor_key = TRUE)

#
# plot smooth and raw district data for central province
#
ggplot(dist1long[dist1long$ADM1_PCODE == 3, ], 
       aes(as.Date(date), value, colour = anc1)) + 
  geom_line(size = 1) +
  scale_colour_manual(labels = c("raw", "smooth"),
                      values = c("#e41a1c", alpha("#377eb8", 0.3))) +
  scale_x_date(name = "Month", 
               date_breaks = "1 month", 
               date_labels = "%b %y") +
  scale_y_continuous(name = "ANC1", 
                     breaks = seq(from = 0, 
                                  to = max(dist1$anc1Std), 
                                  by = 100)) + 
  facet_grid(rows = vars(ADM1_EN), cols = vars(ADM2_EN)) +
  themeSettings