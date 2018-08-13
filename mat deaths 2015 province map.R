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
#map maternal mortality
###################################################################
pdata <- read.csv("provincedata.csv")
head(pdata)

pdata2015 <- pdata[pdata$year == 2015, ]

#
# Add deadhf + deadnothf for total deaths
#
pdata2015$totaldead <- pdata2015$deadhf + pdata2015$deadnothf

#
# create variable females in province
#
femprov <- pop_adm1$FEMALES

# use femprov to normalise maternal deaths per province

deathprov2015 <- (pdata2015$totaldead/femprov)*100000

pop <- data.frame("pcode" = 1:22, "pop" = deathprov2015)

pdata2015 <- merge(pdata2015, pop, by = "pcode")

# map FEMALES=maternal deaths per province normalised by total women per province x 100000
#
# first create classification of maternal deaths
#
deathprov2015class <- base::cut(x=deathprov2015, 
                                breaks = c(0, 10, 20, 40, 60, 80, 100), 
                                labels = FALSE)

###### create colourscheme and provide color for classification per province of mat deaths
colourscheme <- c("#eff3ff", "#c6dbef", "#9ecae1", 
                  "#6baed6", "#3182bd", "#08519c")

plot (province, lwd = 1, border = "gray50", 
      col = ifelse(deathprov2015class == 0, colourscheme[1],
                   ifelse(deathprov2015class == 1, colourscheme[2],
                          ifelse(deathprov2015class == 2, colourscheme[3],
                                 ifelse(deathprov2015class == 3, colourscheme[4],
                                        ifelse(deathprov2015class == 4, colourscheme[5],
                                               ifelse(deathprov2015class == 5, colourscheme[6], colourscheme[7])))))))

