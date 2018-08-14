# Save province and district aggregats as CSV file
#
write.csv(provincedata, "provincedata.csv", row.names = FALSE)
write.csv(provincedata, "districtdata.csv", row.names = FALSE)

if(!require(devtools, quietly = TRUE)) install.packages("devtools") # If devtools required but not installed, install
install_github("OMNeoHealth/papuanewguinea")                        # Install OMNeoHealth/papuanewguinea from GitHub
library(papuanewguinea)                                             # Load papuanewguinea package

pop_adm1     # province level population
pop_adm2     # district level population
pop_adm3     # local government population

###################################################################
#map stillbirths 2015 and 2016
###################################################################
pdata <- read.csv("provincedata.csv")
head(pdata)

pdata2015 <- pdata[pdata$year == 2015, ]
pdata2016 <- pdata[pdata$year == 2016, ]
#
# stillbirths
#
still2015p <- pdata2015$still
still2016p <- pdata2016$still

#
# create variable females in province
#
femprov <- pop_adm1$FEMALES

# use femprov to normalise maternal deaths per province

still2015p <- (pdata2015$still/femprov)*100000
still2016p <- (pdata2016$still/femprov)*100000

pop2015 <- data.frame("pcode" = 1:22, "pop2015" = pdata2015$still)
pop2016 <- data.frame("pcode" = 1:22, "pop2016" = pdata2016$still)

pdata2015 <- merge(pdata2015, pop2015, by = "pcode")
pdata2016 <- merge(pdata2016, pop2016, by = "pcode")

# map stillbirths per province normalised by total women per province x 100000
#
# first create classification of stillbirths for each year
#
still2015pclass <- base::cut(x=still2015p, 
                                breaks = c(0, 10, 20, 40, 60, 80, 100), 
                                labels = FALSE)
still2016pclass <- base::cut(x=still2016p, 
                             breaks = c(0, 10, 20, 40, 60, 80, 100), 
                             labels = FALSE)

###### create colourscheme and provide color for classification per province of mat deaths
colourscheme <- c("#eff3ff", "#c6dbef", "#9ecae1", 
                  "#6baed6", "#3182bd", "#08519c")

plot (province, lwd = 1, border = "gray50", 
      col = ifelse(still2015pclass == 0, colourscheme[1],
                   ifelse(still2015pclass == 1, colourscheme[2],
                          ifelse(still2015pclass == 2, colourscheme[3],
                                 ifelse(still2015pclass == 3, colourscheme[4],
                                        ifelse(still2015pclass == 4, colourscheme[5],
                                               ifelse(still2015pclass == 5, colourscheme[6], colourscheme[7])))))))

plot (province, lwd = 1, border = "gray50", 
      col = ifelse(still2016pclass == 0, colourscheme[1],
                   ifelse(still2016pclass == 1, colourscheme[2],
                          ifelse(still2016pclass == 2, colourscheme[3],
                                 ifelse(still2016pclass == 3, colourscheme[4],
                                        ifelse(still2016pclass == 4, colourscheme[5],
                                               ifelse(still2016pclass == 5, colourscheme[6], colourscheme[7])))))))

##################################################################################################
# plots for ANC visits and province per year
##################################################################################################

#create a variable of ANC1 and ANC4 normalised
anc1norm2015 <-(pdata2015$anc1/femprov)*100000
anc1norm2016 <-(pdata2016$anc1/femprov)*100000

anc4norm2015 <-(pdata2015$anc4/femprov)*100000
anc4norm2016 <-(pdata2016$anc4/femprov)*100000


# plot ANC1 per year per province
plot (pdata2015$pcode, anc1norm2015)
plot (pdata2016$pcode, anc1norm2016)

# plot ANC4 visit per year per province
plot (pdata2015$pcode, anc4norm2015)
plot (pdata2016$pcode, anc4norm2016)

#label axis
plot(pdata2015$pcode, anc1norm2015 , xlab="Province", ylab="Number of ANC1 visits")
plot(pdata2015$pcode, anc4norm2015, xlab="Province", ylab="Number of ANC4 visits")

# chart lines for ANC1 and ANC4 2015
plot(anc1norm2015,type = "o",col = "red", xlab = "Province", ylab = "Number of ANC visits", 
     main = "ANC visits per province in 2015")
lines(anc4norm2015, type = "o", col = "blue")

#char line for ANC1 and ANC4 2016
plot(anc1norm2016,type = "o",col = "red", xlab = "Province", ylab = "Number of ANC visits", 
     main = "ANC visits per province in 2016")
lines(anc4norm2016, type = "o", col = "blue")

# compare ANC1 2015 vs 2016
plot(anc1norm2015,type = "o",col = "red", xlab = "Province", ylab = "Number of ANC visits", 
     main = "ANC1 visits per province in 2015 and 2016")
lines(anc1norm2016, type = "o", col = "blue")

# compare ANC4 2015 vs 2016
plot(anc4norm2015,type = "o",col = "red", xlab = "Province", ylab = "Number of ANC visits", 
     main = "ANC4 visits per province in 2015 and 2016")
lines(anc4norm2016, type = "o", col = "blue")