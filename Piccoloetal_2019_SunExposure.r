########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##																																	  ##
##                     Circulating 25(OH)D Concentrations in Overweight and Obese Adults is Explained by Sun Exposure, 				  ##
##			                          Skin Reflectance, and Body Composition 					                  ##
##                  Brian D. Piccolo1,2 Laura M. Hall3, Charles B. Stephensen3,4 Erik R. Gertz4 and Marta D. Van Loan3,4			  ##
##																																	  ##
##                                    5.	Arkansas Children’s Nutrition Center, Little Rock, AR						##
##				      6.	University of Arkansas for Medical Sciences, Little Rock, AR					 ##
##				      7.	University of California, Davis, CA								##
##				      8.	USDA, ARS, Western Human Nutrition Research Center, Davis, CA					##
##																												  ##
## 								Accepted Current Development in Nutrition
##                                    Published online (https://academic.oup.com/cdn/advance-article/doi/10.1093/cdn/nzz065/5499015)		##
##																																	  ##
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################


        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     In House Functions      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
		

niceBOX <- function(DF, ID, Group, bpcol = NULL, bp_xlab = NULL, logT=FALSE, bp_ylab=NULL){

	## Required packages
	require(RColorBrewer)
	require(reshape2)
	
	## Set line color
	linepal <- RColorBrewer::brewer.pal("Greys", n=9)
	## Extract the plotting data label
	index <- colnames(DF)[!(colnames(DF) %in% c(ID, Group))]
	## Coerce into wide data format
	DF_dcast <- dcast(DF, as.formula(paste0(ID,"~",Group)))
	## Remove ID column
	DF_dcast[,ID] <- NULL
	## coerce to matrix
	DF_dcast <- as.matrix(DF_dcast)
	
	## Extract the grouping and plotting data
	plotDF <- DF[,c(Group, index)]
	## Change plotting data label to "index"
	colnames(plotDF)[colnames(plotDF) %in% index] <- "index"
	
	## Transform data if necessary.
	if(logT == TRUE) {
		
		plotDF$index <- log(plotDF$index)
		if(-Inf %in% plotDF$index) {
			plotDF$index[plotDF$index %in% -Inf] <- 0
		}
	}

	## Calculate jitter for plotting
	jitmatrix <- (matrix(runif(nrow(DF_dcast)*ncol(DF_dcast)), nrow=nrow(DF_dcast), ncol=ncol(DF_dcast)) / 2) - 0.75
	for(i in 1:ncol(jitmatrix))  jitmatrix[,i] <- ifelse(DF_dcast[,i] %in% NA, NA, jitmatrix[,i])

	## Make color matrix that coincides with DF_dcast
	if(is.null(bpcol)) {
		NULL
	} else {
		bpcolmatrix <- matrix(rep(bpcol, each=nrow(DF_dcast)), ncol=ncol(DF_dcast))
		for(i in 1:ncol(bpcolmatrix))  bpcolmatrix[,i] <- ifelse(DF_dcast[,i] %in% NA, NA, bpcolmatrix[,i])
	}
	
	## Determine boxplot characteristics
	bps <- graphics::boxplot(as.formula(paste0("index~",Group)), data=plotDF, plot=FALSE)
	
	## Determine y axis label limits
	pretty_yrange <- pretty(range(c(bps[[1]], bps[[4]])))
	
	## Number of group levels
	plevels <- nlevels(DF[,Group])

	
	## Plot data
	plot(0:plevels, rep(1,plevels+1), axes=FALSE, type="n", ylim=range(pretty_yrange), xlab="", ylab="")		

	## Add horizontal grid lines
	for (i in pretty_yrange) {
		lines(c(0,plevels+1), c(i,i), col=linepal[4])
	}	
	
	## Add vertical grid lines
	for (i in (1:plevels)-0.5) {
		lines(c(i,i), range(pretty_yrange) + (c(-0.05,0.05) * range(pretty_yrange)), col=linepal[4])
	}	
	
	## Use Times font
	# windowsFonts(A = windowsFont("Times New Roman"))
	
	## Add xaxis and xlabels
	text((1:plevels)-0.7, par("usr")[3], labels = levels(DF[,Group]), srt = 45, pos = 1, xpd = TRUE, cex=1.5)
	if(is.null(bp_xlab)) {
		NULL
	} else {
		axis(side=1, at=mean((1:plevels)-0.5), labels=bp_xlab, col.axis=linepal[8], lty=0, tick=NA, las=1, line=1)

	}
	
	## Add yaxis and ylabel
	axis(2, at=pretty_yrange, col.axis=linepal[8], cex.axis=1.5, lty=0, tick=NA, line=-2, las=2)
	if(is.null(bp_ylab)) {
		NULL
	} else {		
		axis(side=2, at=mean(pretty_yrange), labels=bp_ylab, lty=0, tick=NA, col.axis=linepal[8], line=1, cex.axis=1.5, srt = 45, xpd = TRUE)
	}
	
	## Add Boxes
	if(is.null(bpcol)) {
		rect(
			1:plevels - 0.9, 
			bps[[1]][2,1:plevels], 
			1:plevels - 0.1, 
			bps[[1]][4,1:plevels], lwd=2.5)
	} else {
		rect(1:plevels - 0.9, bps[[1]][2,1:plevels], 1:plevels - 0.1, bps[[1]][4,1:plevels], col=bpcol[1:plevels], lwd=3)
	}		
	
	## Whiskers
	segments(
		matrix(rep(1:plevels - 0.5, 2), nrow=2, byrow=TRUE), 
		bps[[1]][c(1,5),1:plevels], 
		matrix(rep(1:plevels - 0.5, 2), nrow=2, byrow=TRUE), 
		bps[[1]][c(2,4),1:plevels], lwd=3
	)
	## Median Line
	segments(
		1:plevels - 0.9, 
		bps[[1]][3,1:plevels], 
		1:plevels - 0.1, 
		bps[[1]][3,1:plevels], 
		lwd=3
	)
	
	## Add points
	if(is.null(bpcol)) {
		points((col(DF_dcast) - 0.0 ) + jitmatrix, DF_dcast, 
			col="black", bg="black", pch=21, lwd=0.5, cex=1.5)
	} else {
		points((col(DF_dcast) - 0.00 ) + jitmatrix, DF_dcast, bg=bpcolmatrix, 
			col="black", pch=21, lwd=0.5, cex=1.5)
	}
}

	

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Required Libraries      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##		

## Install required packages if necessary
install_packages <- c("tidyverse","multcomp","reshape2", "dunn.test", "elevatr","geosphere","Hmisc","MASS","car","RColorBrewer")
if (length(setdiff(install_packages, rownames(installed.packages()))) > 0) {
            install.packages(setdiff(install_packages, rownames(installed.packages())))
}
 		

require(tidyverse)
require(multcomp)
require(reshape2)
require(dunn.test)
# require(multcompView)
require(elevatr)
require(geosphere)
require(Hmisc)
require(MASS)
require(car)
require(RColorBrewer)
require(knitr)

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Importing Data      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

# Set working directory
setwd()

## Data collected from Sun Exposure Diaries and clothing key (Figure 2).
## Data from logs were inputed in Microsoft Access, then converted to Excel format, and then converted to csv.
SEDataRAW <- read.csv("./data/SunExposureDiaryData5619.csv", header=TRUE, comment.char = "", check.names = FALSE)

## Remove data with no information
SEDataRAW <- SEDataRAW[,apply(SEDataRAW, 2, function(x) !(sum(x %in% NA) == nrow(SEDataRAW)))]

######### Key
colnames(SEDataRAW)
# DCohort = Study Cohort
# DSubjectID = Subject ID
# DDate = Date of exposure
# DSunscrn = Sunscreen (Yes/No)
# DSPF = Sunscreen SPF
# DTimeofday = Hour of exposure, categorical (e.g., 2-2:59pm)
# DL = Location
# DOA = Outside Activity
# DDS = Time in direct sun, min
# DSh = Time in shade, min
# DNeck = Clothing around neck (Yes/No; e.g., scarf)
# DGlvs = Gloves worn (Yes/No)
# DBEA = Integer representing area of body exposure on head (A; 1-4)
# DBEB = Integer representing area of body exposure on torso/arms (B; 1-6)
# DBEC = Integer representing area of body exposure on legs (C; 1-4)
# DBED = Integer representing area of body exposure on feet (D; 1-3)
# DBSA = Integer representing area of sunscreen application on head (A; 1-4)
# DBSB = Integer representing area of sunscreen application on torso (B; 1-6)
# DBSC = Integer representing area of sunscreen application on legs (C; 1-4)
# DBSD = Integer representing area of sunscreen application on feet (C; 1-3)  
######### Key

## Import Metadata, skin reflectance, and body composition data.
MetaDAT <- read.csv("./data/Meta_SkinR_BodyCData5619.csv", header=TRUE, comment.char = "", check.names = FALSE)

######### Key
colnames(MetaDAT)
# ID = Subject ID
# Treat = Diet treatment in clinical trial (1 - Low Dairy or 2 - Adequate Dairy)
# Season = Season of enrollment in clinical trial (1 - Winter, 2 - Spring, 3 - Summer, 4 - Fall)
# Sex = Self reported sex of participant (1 - Female, 2 - Male)
# Age = Age (yr) at time of enrollment
# SREthinicity = Self reported ethnicity of participant
# dintakei3/dintakei15 = Dietary vitamin D intake (UI/day; 3 and 15 represent trial week)
# W3A/W15A = Skin reflectance of inner arm (*L; 3 and 15 represent trial week)
# W3H/W15H = Skin reflectance of outside of hand between index and thumb (*L; 3 and 15 represent trial week)
# W3F/W15F = Skin reflectance of forehead (*L; 3 and 15 represent trial week)
# W3HF/W15HF = Average of skin reflectance between hand and forehead (*L; 3 and 15 represent trial week)
# s25OHD3/s25OHD15 = Serum 25(OH)D concentration (nmol/L; 3 and 15 represent trial week)
# wt3/wt15 = Total body weight (kg; 3 and 15 represent trial week)
# pctbf3/pctbf15 = Percent body fat (%; 3 and 15 represent trial week)
# fatkg3/fatkg15 = Total fat mass (kg; 3 and 15 represent trial week)
# leankg3/leankg15 = Total lean mass (kg; 3 and 15 represent trial week)
# androidf3/androidf15 = Android fat mass (kg; 3 and 15 represent trial week)
# gynoidf3/gynoidf15 = Gynoid fat mass (kg; 3 and 15 represent trial week)
# iaat3/iaat15 = Intra-abdominal fat volume (cubic cm (cc); 3 and 15 represent trial week)
# BMI = Body Mass Index (kg/meter square; 3 and 15 represent trial week)
# waist3/waist15 = waist circumference (cm; 3 and 15 represent trial week)  
######### Key

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Pre-processing of Imported data      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Diary Data
## Remove special character in subject IDs
SEDataRAW$DSubjectID <- factor(gsub("-", "", as.character(SEDataRAW$DSubjectID)))

## Convert meta categorical data into appropriate factors
MetaDAT$ID <- factor(MetaDAT$ID)
MetaDAT$Treat <- factor(with(MetaDAT, ifelse(Treat == 1, "LD", "AD"))) 
MetaDAT$Season <- factor(with(MetaDAT, 
	ifelse(Season == 1, "Winter", 
	ifelse(Season == 2, "Spring", 
	ifelse(Season == 3, "Summer", "Fall" 
)))))
MetaDAT$Sex <- factor(with(MetaDAT, ifelse(Sex == 1, "F", "M"))) 
## Remove unnecessary space in SREthinicity string
MetaDAT$SREthnicity <- factor(gsub("Asian ", "Asian", MetaDAT$SREthnicity))

## Show n for each categorical variable
lapply(MetaDAT[,c("Treat", "Season", "Sex", "SREthnicity")], function(x) summary(x))

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Calculation of Body Surface Area      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Convert body exposure integers to body surface area using adapted "rule of nines".
## Please refer to Barger-Lux et & Heaney (J Clin Endocrinol Metab, 2002; https://academic.oup.com/jcem/article/87/11/4952/2823136) for detailed explanation
## Based on Hall et al (J Nutr, 2010; https://academic.oup.com/jn/article/140/3/542/4600420)
SEDataRAW$pcexposureHEAD <- with(SEDataRAW, 
	ifelse(DBEA == 1, 0.04, 
	ifelse(DBEA == 2, 0.03, 
	ifelse(DBEA == 3, 0.02, 
	ifelse(DBEA == 4, 0.01, 0
)))))
SEDataRAW$pcexposureTRUNK <- with(SEDataRAW, 
	ifelse(DBEB == 1, 0.47, 
	ifelse(DBEB == 2, 0.42, 
	ifelse(DBEB == 3, 0.18,
	ifelse(DBEB == 4, 0.10,
	ifelse(DBEB == 5, 0.03,
	ifelse(DBEB == 6, 0.00, 0
)))))))
SEDataRAW$pcexposureLEGS <- with(SEDataRAW, 
	ifelse(DBEC == 1, 0.38, 
	ifelse(DBEC == 2, 0.24, 
	ifelse(DBEC == 3, 0.08,
	ifelse(DBEC == 4, 0.00, 0
)))))
SEDataRAW$pcexposureFEET <- with(SEDataRAW, 
	ifelse(DBED == 1, 0.02, 
	ifelse(DBED == 2, 0.015, 
	ifelse(DBED == 3, 0.00, 0
))))
SEDataRAW$pcexposureHANDS <-with(SEDataRAW, 
	ifelse(DGlvs == "No", 0.04, 0.00
))	
SEDataRAW$pcexposureNECK <-with(SEDataRAW, 
	ifelse(DNeck == "No", 0.02, 0.00
))

## Sum each % exposure for total % body surface area exposed
SEDataRAW$pcBSAE <- with(SEDataRAW, 
	pcexposureHEAD + pcexposureTRUNK + pcexposureLEGS + pcexposureFEET + pcexposureHANDS + pcexposureNECK
)

## Extract meta and body composition data needed calculate total body surface area in meter squared.
BSADAT <- MetaDAT[,colnames(MetaDAT) %in% c("ID", "ht", "wt", "wt15")]
## Mosteller formula: BSA = sqrt((W x H)/3600), where W = weight (kg) and H = height (cm).
## Calculate BSA
BSADAT$BSA <- with(BSADAT, 
	sqrt(((ht * 100) * wt15)/3600)
)

## Subset data needed to join with diary data
BSAjoin_DAT <- BSADAT[,colnames(BSADAT) %in% c("ID", "BSA")]
## Change ID label to match with label in diary data
colnames(BSAjoin_DAT)[colnames(BSAjoin_DAT) %in% "ID"] <- "DSubjectID"

## Only join matching data
SEDataRAW <- inner_join(SEDataRAW, BSAjoin_DAT, by="DSubjectID")
## Calculate total body surface area exposed for each exposure
SEDataRAW$BSAE <- with(SEDataRAW, 
	BSA * pcBSAE
)
## Caclulate sun exposure index
## Please refer to Barger-Lux et & Heaney (J Clin Endocrinol Metab, 2002; https://academic.oup.com/jcem/article/87/11/4952/2823136) for detailed explanation
SEDataRAW$SEI <- with(SEDataRAW, 
	BSAE * DDS
)
## Calculate Total Time outside (time in direct sun and in shade)
SEDataRAW$TimeOut <- SEDataRAW$DDS + SEDataRAW$DSh
		
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Identification of Climate Station Locations      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

		## Change location data to character 
SEDataRAW$DL <- as.character(SEDataRAW$DL)
## Add US state information.  Since the majority of cities are in CA, we will first add CA for all cities and correct non-CA manually.
unique(SEDataRAW$DL)
## Note there is a missing location entry.
SEDataRAW$State <- "CA"
SEDataRAW[SEDataRAW$DL %in% "Las Vegas","State"] <- "NV"
SEDataRAW[SEDataRAW$DL %in% "Chicago","State"] <- "IL"
SEDataRAW[SEDataRAW$DL %in% "Ashdown Forest, England","State"] <- NA
SEDataRAW[SEDataRAW$DL %in% "Reno","State"] <- "NV"
SEDataRAW[SEDataRAW$DL %in% "Orlando, Florida","State"] <- "FL"
SEDataRAW[SEDataRAW$DL %in% "Mt. Rushmore, South Dakota","State"] <- "SD"
SEDataRAW[SEDataRAW$DL %in% "Boston","State"] <- "MA"
SEDataRAW[SEDataRAW$DL %in% "Atlanta","State"] <- "GA"
SEDataRAW[SEDataRAW$DL %in% "Puerto Vallarta, Mexico","State"] <- NA
## Check other missing locations for missing entry
SEDataRAW[SEDataRAW$DSubjectID %in% SEDataRAW[SEDataRAW$DL %in% "","DSubjectID"],"DL"] 
## All are Davis/Sacramento.  Will impute missing entry as Davis.
SEDataRAW[SEDataRAW$DL %in% "","DL"] <- "Davis"

## Update non-standard, neighborhood, or misspelled entries
unique(SEDataRAW[,c("DL", "State")])
SEDataRAW$DL[SEDataRAW$DL %in% "N. Highlands"] <- "North Highlands"
SEDataRAW$DL[SEDataRAW$DL %in% "Suisun City (Solano County)"] <- "Suisun"
SEDataRAW$DL[SEDataRAW$DL %in% "Orlando, Florida"] <- "Orlando"
SEDataRAW$DL[SEDataRAW$DL %in% "Vacville"] <- "Vacaville"
SEDataRAW$DL[SEDataRAW$DL %in% "Elverts"] <- "Elverta"
SEDataRAW$DL[SEDataRAW$DL %in% "Stony Ford"] <- "Stonyford"
SEDataRAW$DL[SEDataRAW$DL %in% "West Sacarmento"] <- "West Sacramento"
SEDataRAW$DL[SEDataRAW$DL %in% "Natomas"] <- "Sacramento" ## Natomas is a northwestern neighborhood in Sacramento, CA
SEDataRAW$DL[SEDataRAW$DL %in% "Denios"] <- "Roseville" ## Denios is a large Farmers Market/swap meet in Roseville, CA
SEDataRAW$DL[SEDataRAW$DL %in% "Zamora"] <- "Knights Landing" ## Zamora is an unincorporated community. Knights landing is closest city (10 miles due east) 
SEDataRAW$DL[SEDataRAW$DL %in% "Bareyssa"] <- "San Jose" ## Berryessa is either a neighborhood in San Jose, or refers to Lake Berryessa. Both will be within Davis CS.
SEDataRAW$DL[SEDataRAW$DL %in% "Tahoe"] <- "South Lake Tahoe" ## Tahoe refers to Lake Tahoe.  Will use South Lake Tahoe, CA as reference.
SEDataRAW$DL[SEDataRAW$DL %in% "Nelson Grove"] <- "Woodland" ## Nelson's Grove is a park within Woodland, CA.
SEDataRAW$DL[SEDataRAW$DL %in% "Folsom Lake"] <- "Folsom" ## Folsom Lake is a reservoir within the Sacramento metropolitan area. Will use Folsom, CA as reference

## Google maps requires purchase of an API key to access geocoding data, so we used the simplemaps US city database (https://simplemaps.com/data/us-cities)
## to determine latitude and longitudinal data of provided cities.  We then used the elevatr package to determine altitude.
## Identify cities
SLcities <- unique(SEDataRAW[,c("DL", "State")])
SLcities

## Import csv from https://simplemaps.com/data/us-cities.
cityDAT <- read.csv("./data/uscitiesv1.5.csv", header=TRUE, comment.char = "", check.names = FALSE)

## Identify cities that do not match
UScities <- SLcities[tolower(SLcities$DL) %in% tolower(as.character(cityDAT$city)),]

## Create columns for longitude and latitude.
SEDataRAW$lng <- NA
SEDataRAW$lat <- NA
## use loop to update long/lat info
for(i in UScities$DL) {
	state <- UScities[UScities$DL %in% i, "State"]
	lng <- cityDAT[cityDAT$city %in% i & cityDAT$state_id %in% state,"lng"]
	lat <- cityDAT[cityDAT$city %in% i & cityDAT$state_id %in% state,"lat"]
	SEDataRAW[SEDataRAW$DL %in% i,"lng"] <- lng
	SEDataRAW[SEDataRAW$DL %in% i,"lat"] <- lat
}

## Fill in missing lng and lat for US non-city locations or non-US locations
## Longitude and Latitude for these locations were obtained listed next to printed text below
unique(SEDataRAW[SEDataRAW$lng %in% NA,c("DL", "State", "lng", "lat")])
# 773     Ashdown Forest, England  <NA>  NA  NA # 0.043056, 51.0725
# 1404 Mt. Rushmore, South Dakota    SD  NA  NA # -103.459722, 43.878889
# 1480                Donner Lake    CA  NA  NA # -120.264444, 39.322778
# 1875                 Lake Tahoe    CA  NA  NA # -120.041667, 39.091667
# 2732            Red Rock Canyon    CA  NA  NA # -117.990686, 35.372986 
# 2978    Puerto Vallarta, Mexico  <NA>  NA  NA # -105.266667, 20.666667
# 3265               Rollins Lake    CA  NA  NA # -120.952182, 39.136189
# 3521              Drake's Beach    CA  NA  NA # -122.919167, 38.011944 
# 3555             Lake Berryessa    CA  NA  NA # -122.23, 38.59

## Manually add longitude and latitude
# Ashdown Forest, England
SEDataRAW[SEDataRAW$DL %in% "Ashdown Forest, England",c("lng", "lat")] <- 
	rep(c(0.043056, 51.0725), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Ashdown Forest, England",c("lng", "lat")]))
# Mt. Rushmore, South Dakota	
SEDataRAW[SEDataRAW$DL %in% "Mt. Rushmore, South Dakota",c("lng", "lat")] <- 
	rep(c(-103.459722, 43.878889), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Mt. Rushmore, South Dakota",c("lng", "lat")]))
# Donner Lake
SEDataRAW[SEDataRAW$DL %in% "Donner Lake",c("lng", "lat")] <- 
	rep(c(-120.264444, 39.322778), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Donner Lake",c("lng", "lat")]))
# Lake Tahoe
SEDataRAW[SEDataRAW$DL %in% "Lake Tahoe",c("lng", "lat")] <- 
	rep(c(-120.041667, 39.091667), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Lake Tahoe",c("lng", "lat")]))
# Red Rock Canyon
SEDataRAW[SEDataRAW$DL %in% "Red Rock Canyon",c("lng", "lat")] <- 
	rep(c(-117.990686, 35.372986), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Red Rock Canyon",c("lng", "lat")]))
# Puerto Vallarta, Mexico
SEDataRAW[SEDataRAW$DL %in% "Puerto Vallarta, Mexico",c("lng", "lat")] <- 
	rep(c(-105.266667, 20.666667), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Puerto Vallarta, Mexico",c("lng", "lat")]))
# Rollins Lake
SEDataRAW[SEDataRAW$DL %in% "Rollins Lake",c("lng", "lat")] <- 
	rep(c(-120.952182, 39.136189), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Rollins Lake",c("lng", "lat")]))
# Drake's Beach (Point Reyes)
SEDataRAW[SEDataRAW$DL %in% "Drake's Beach",c("lng", "lat")] <- 
	rep(c(-122.919167, 38.011944), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Drake's Beach",c("lng", "lat")]))
# Lake Berryessa
SEDataRAW[SEDataRAW$DL %in% "Lake Berryessa",c("lng", "lat")] <- 
	rep(c(-122.23, 38.59), each=nrow(SEDataRAW[SEDataRAW$DL %in% "Lake Berryessa",c("lng", "lat")]))

## Subset location, longitudinal, and latitude data and modify new data frame to requirements for elevatr package.
lnglatDAT <- unique(SEDataRAW[,c("DL", "lng", "lat")])
rownames(lnglatDAT) <- lnglatDAT$DL
lnglat_erDF <- lnglatDAT
lnglat_erDF$DL <- NULL
colnames(lnglat_erDF) <- c("x", "y")
## Following example provided in help page for PROJ.4 string.
ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
## Calculate elevations
elevreturn <- get_elev_point(locations = lnglat_erDF, prj = ll_prj, src = "epqs")
## Merge longitude, latitude, and elevation data.
lnglatDAT <- data.frame(lnglatDAT, elevreturn@data[,1, drop=FALSE])
## Non-US locations not provided in database, change Non-US locations to NA
lnglatDAT[lnglatDAT$elevation < -1000,"elevation"] <- NA
## Create columns for elevation.
SEDataRAW$Elevation <- NA
## use loop to update long/lat info
for(i in lnglatDAT$DL) {
	elev <- lnglatDAT[lnglatDAT$DL %in% i,"elevation"]
	SEDataRAW[SEDataRAW$DL %in% i,"Elevation"] <- elev
}

## Import UV-B Monitoring Climatological and Research Network Location Metadata
UVBMRPmeta <- read.csv("./data/networkList.csv", header=TRUE, comment.char = "", check.names = FALSE)
## Create new columns with numerical data for longitude, latitude, and elevation.
UVBMRPmeta$lng <- gsub(" W", "", as.character(UVBMRPmeta$Longitude))
UVBMRPmeta$lng <- gsub(" E", "", as.character(UVBMRPmeta$lng))
UVBMRPmeta$lng <- as.numeric(UVBMRPmeta$lng)
for(i in as.character(UVBMRPmeta$Longitude)) {
	if(grepl("W", i)) UVBMRPmeta[UVBMRPmeta$Longitude %in% i,"lng"] <- as.numeric(UVBMRPmeta[UVBMRPmeta$Longitude %in% i,"lng"]) * -1
} 	
UVBMRPmeta$lat <- gsub(" N", "", as.character(UVBMRPmeta$Latitude))
UVBMRPmeta$lat <- gsub(" S", "", as.character(UVBMRPmeta$lat))
UVBMRPmeta$lat <- as.numeric(UVBMRPmeta$lat)
for(i in as.character(UVBMRPmeta$Latitude)) {
	if(grepl("S", i)) UVBMRPmeta[UVBMRPmeta$Latitude %in% i,"lat"] <- as.numeric(UVBMRPmeta[UVBMRPmeta$Latitude %in% i,"lat"]) * -1
} 
UVBMRPmeta$elev <- as.numeric(gsub(" m", "", as.character(UVBMRPmeta$Elevation)))

## Create data frame to collect distances between all climate stations and sun exposure cities
Citydistmat <- data.frame(matrix(NA, nrow=nrow(lnglat_erDF), ncol=nrow(UVBMRPmeta), dimnames=list(rownames(lnglat_erDF), UVBMRPmeta$Town)))
## Calculate distances between Climate stations	
for(i in 1:nrow(UVBMRPmeta)) {
	CSlnglat <- unlist(UVBMRPmeta[i,c("lng", "lat")])
	Citydistmat[,i] <- distm(lnglat_erDF, CSlnglat, fun = distHaversine)
}
## Identify closet climate station to each sun exposure city
Citydistmat$Closest_CS <- apply(Citydistmat, 1, function(x) colnames(Citydistmat)[which.min(x)])
## Although Panther Junction Station is closer to Puerto Vallarta, will use Holtville because it is closer to Pacific Ocean and at more compareable elevation.Citydistmat[rownames(Citydistmat) %in% c("Puerto Vallarta, Mexico", "Ashdown Forest, England")]
Citydistmat[grepl("Panther", Citydistmat$Closest_CS),"Closest_CS"] <- "Holtville"
## Remove Ashdown Forest, England US climate station
Citydistmat[grepl("Presque", Citydistmat$Closest_CS),"Closest_CS"] <- NA
## Create columns for elevation.
SEDataRAW$CSLocation <- NA
## use loop to update long/lat info
for(i in rownames(Citydistmat)) {
	CS <- Citydistmat[rownames(Citydistmat) %in% i,"Closest_CS"]
	SEDataRAW[SEDataRAW$DL %in% i,"CSLocation"] <- CS
}

## Make unique identifier for Climate Station data merge
SEDataRAW$CSL_D_T <- apply(SEDataRAW[,c("CSLocation", "DDate", "DTimeofday")], 1, paste, collapse="_")


        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Import Climate Station Data      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Manually downloaded UVBB data here: https://uvb.nrel.colostate.edu/UVB/da_queryErythemal.jsf
## Data is provided in Data folder
## Will download entire study period for Davis station
range(as.Date(SEDataRAW$DDate, format = "%m/%d/%Y"))
## Downloads will fail to show all data if too many days.
## Downloaded in 4-mo batches.

## CA01 Davis
DAVISyears <- c("040507 - 070507","070507 - 100507","100507 - 010408","010408 - 040508","040508 - 070508",
	"070508 - 100508","100508 - 010509","010509 - 040509","040509 - 070509","070509 - 082509")
Davis_CS_Data <- lapply(DAVISyears, function(x) {
	DAT <- read.csv(paste0("./data/CA01 ", x, ".csv"), header=TRUE, comment.char = "", check.names = FALSE)
	## Format date so it is consistent across locations
	DAT[["YYYY-MM-DD HH:MM:SS"]] <- with(DAT, 
		paste0(LYYY, "-", LM, "-", LD, " ", Lh, ":", Lm, ":", Ls)
	)
	## Remove un-needed columns
	DAT[,!(colnames(DAT) %in% c("","Qc Code"))]
})
## Collapse each file into a single data frame
Davis_CS_DataTALLEST <- do.call(rbind, Davis_CS_Data)
## Remove redundant data
Davis_CS_DataTALL <- unique(Davis_CS_DataTALLEST)
dim(Davis_CS_DataTALLEST)
dim(Davis_CS_DataTALL)
## Label files for future merging with other locations
Davis_CS_DataTALL$CS_Code <- "CA01"
Davis_CS_DataTALL$CS_Location <- "Davis"

## Determine ranges of dates for nonDavis climate station downloads
nonDavisCS <- unique(SEDataRAW$CSLocation)
nonDavisCS <- nonDavisCS[!(nonDavisCS %in% c("Davis", NA))]
nonDAVIS_daterange <- lapply(nonDavisCS, function(x) {
	range(as.Date(SEDataRAW[SEDataRAW$CSLocation %in% x,"DDate"], format = "%m/%d/%Y"))
})
names(nonDAVIS_daterange) <- nonDavisCS
nonDAVIS_daterange
## Flagstaff and Holtville stations have wide ranges, so will look at these deeper.

as.character(unique(SEDataRAW[SEDataRAW$CSLocation %in% "Flagstaff","DDate"]))
# [1] "6/9/2007"  "8/7/2008"  "8/8/2008"  "8/9/2008"  "4/9/2009"  "4/10/2009"
as.character(unique(SEDataRAW[SEDataRAW$CSLocation %in% "Holtville","DDate"]))
 # [1] "6/17/2007" "6/21/2008" "6/22/2008" "6/23/2008" "6/24/2008" "4/9/2009"  "5/25/2009" "5/26/2009" "5/27/2009" "5/28/2009" "5/29/2009"
# [12] "5/30/2009" "5/31/2009" "4/24/2009" "8/7/2009"
## Will download these dates

## CA02 Holtville
HOLTyears <- c("061607 - 061807","062008 - 062508","052409 - 060109","040809 - 041009","042309 - 042509","080609 - 080809")
HOLT_CS_Data <- lapply(HOLTyears, function(x) {
	DAT <- read.csv(paste0("./data/CA02 ", x, ".csv"), header=TRUE, comment.char = "", check.names = FALSE)
	## Format date so it is consistent across locations
	DAT[["YYYY-MM-DD HH:MM:SS"]] <- with(DAT, 
		paste0(LYYY, "-", LM, "-", LD, " ", Lh, ":", Lm, ":", Ls)
	)
	## Remove un-needed columns
	DAT[,!(colnames(DAT) %in% c("","Qc Code"))]
})
## Collapse each file into a single data frame
HOLT_CS_DataTALLEST <- do.call(rbind, HOLT_CS_Data)
## Remove redundant data
HOLT_CS_DataTALL <- unique(HOLT_CS_DataTALLEST)
dim(HOLT_CS_DataTALLEST)
dim(HOLT_CS_DataTALL)
## Label files for future merging with other locations
HOLT_CS_DataTALL$CS_Code <- "CA02"
HOLT_CS_DataTALL$CS_Location <- "Holtville"

## AZ01 Logan
FLAGyears <- c("040809 - 041109","080608 - 081008","060807 - 061007")
FLAG_CS_Data <- lapply(FLAGyears, function(x) {
	DAT <- read.csv(paste0("./data/AZ01 ", x, ".csv"), header=TRUE, comment.char = "", check.names = FALSE)
	## Format date so it is consistent across locations
	DAT[["YYYY-MM-DD HH:MM:SS"]] <- with(DAT, 
		paste0(LYYY, "-", LM, "-", LD, " ", Lh, ":", Lm, ":", Ls)
	)
	## Remove un-needed columns
	DAT[,!(colnames(DAT) %in% c("","Qc Code"))]
})
## Collapse each file into a single data frame
FLAG_CS_DataTALLEST <- do.call(rbind, FLAG_CS_Data)
## Remove redundant data
FLAG_CS_DataTALL <- unique(FLAG_CS_DataTALLEST)
dim(FLAG_CS_DataTALLEST)
dim(FLAG_CS_DataTALL)
## Label files for future merging with other locations
FLAG_CS_DataTALL$CS_Code <- "AZ01"
FLAG_CS_DataTALL$CS_Location <- "Flagstaff"

CS_singlesites <- 
singlesite_nonDAV_CSloc <- data.frame(
	FileName = c("IN01 051907 - 052117","FL01 072908 - 081408","CO01 070608 - 071308","VT01 110208 - 110408","GA01 092108 - 092108"), 
	Location=nonDavisCS[!(nonDavisCS %in% c("Flagstaff","Holtville"))], stringsAsFactors=FALSE
)
singlestation_CS_Data <- lapply(1:nrow(singlesite_nonDAV_CSloc), function(x) {
	FileName <- CS_singlesites[x,"FileName"]
	Location <- singlesite_nonDAV_CSloc[x,"Location"]
	DAT <- read.csv(paste0("./data/", FileName, ".csv"), header=TRUE, comment.char = "", check.names = FALSE)
	## Format date so it is consistent across locations
	DAT[["YYYY-MM-DD HH:MM:SS"]] <- with(DAT, 
		paste0(LYYY, "-", LM, "-", LD, " ", Lh, ":", Lm, ":", Ls)
	)
	## Remove un-needed columns
	DAT <- DAT[,!(colnames(DAT) %in% c("","Qc Code"))]
	## Label files for future merging with other locations
	DAT$CS_Code <- strsplit(FileName, " ")[[1]][1]
	DAT$CS_Location <- Location
	DAT
})
## Collapse each file into a single data frame
singlestation_CS_DataTALL <- do.call(rbind, singlestation_CS_Data)

## Merge all climate station data together as a single data frame
CS_DATAmerge <- do.call(rbind, list(Davis_CS_DataTALL, HOLT_CS_DataTALL, FLAG_CS_DataTALL, singlestation_CS_DataTALL))
dim(CS_DATAmerge)
# [1] 446712     25

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Calculate Mean Hourly UVB      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	
		
## Climate station data provides UVB measurements every 3 minutes. 
## We will capture the mean hourly UVB measurements for hours surveyed by the sun exposure diaries (e.g., 7am-7pm)
CS_HOURLYmean <- CS_DATAmerge %>% 
	# Filter required hours
	filter(Lh %in% 7:18) %>% 
	# group by climate station location, year, month, day, and hour
	group_by(CS_Location, LYYY, LM, LD, Lh) %>% 
	# calculate mean
	summarise(avgH_uvbb=mean(uvbb)) %>% 
	# Ensure all readings are >= 0
	mutate(avgH_uvbb=ifelse(avgH_uvbb <= 0, 0, avgH_uvbb)) %>% as.data.frame
## Make new date column that has a similar format to SEDataRAW
CS_HOURLYmean$Date <- with(CS_HOURLYmean, paste(LM, LD, LYYY, sep="/"))
## Create vector with the hour classifications that are similar to that in SEDataRAW
hourorder <- c("7-7:59am","8-8:59am","9-9:59am","10-10:59am","11-11:59am","12-12:59pm","1-1:59pm","2-2:59pm","3-3:59pm","4-4:59pm","5-5:59pm","6-6:59pm")
## Use hourorder to make a new Time column in a format similar to SEDataRAW
CS_HOURLYmean$Time <- factor(with(CS_HOURLYmean, 
	ifelse(Lh == 7, hourorder[1],
	ifelse(Lh == 8, hourorder[2],
	ifelse(Lh == 9, hourorder[3],
	ifelse(Lh == 10, hourorder[4],
	ifelse(Lh == 11, hourorder[5],
	ifelse(Lh == 12, hourorder[6],
	ifelse(Lh == 13, hourorder[7],
	ifelse(Lh == 14, hourorder[8],
	ifelse(Lh == 15, hourorder[9],
	ifelse(Lh == 16, hourorder[10],
	ifelse(Lh == 17, hourorder[11],hourorder[12]
)))))))))))), levels=hourorder)
	
## Make unique identifier for SEDataRAW data merge
CS_HOURLYmean$CSL_D_T <- apply(CS_HOURLYmean[,c("CS_Location", "Date", "Time")], 1, paste, collapse="_")

## Merge diary data with hourly averaged climate station data.  
## Use left_join to ensure that all SEDataRAW data is retained and only matching CS_HOURLYmean is joined
SEData <- left_join(SEDataRAW, CS_HOURLYmean[,c("CS_Location","avgH_uvbb","Date","Time","CSL_D_T")], by="CSL_D_T")
## Make sure number of rows did not change
nrow(SEDataRAW) == nrow(SEData)

## Check for missing UVB data.
SEData[SEData$avgH_uvbb %in% NA,]
## Incomplete UVB data due to missing Climate Station data - It appears there was no UVB measurements for these time periods.
## What are the locations and dates
SEData %>% filter(avgH_uvbb %in% NA) %>% dplyr::select(CSLocation, DDate) %>% unique()
   # CSLocation     DDate
# 1       Davis 6/16/2007
# 10       <NA> 3/12/2008 Ashdown
# 12       <NA> 3/16/2008 Ashdown
# 13       <NA> 3/21/2008 Ashdown
# 14  Homestead 8/13/2008
## Will impute Davis and Homestead days with average of days surrounding missing date
## Ashdown Forest, England, will remain missing.

## Calculate times for "Davis" date
Davis_IMPUTE_61607 <- CS_HOURLYmean %>% 
	filter(CS_Location %in% "Davis", Date %in% c("6/15/2007", "6/17/2007")) %>% # Extract measurements from days before and after 6/16/07
	group_by(Time) %>% # Group by hours
	summarise(avgH_uvbb = mean(avgH_uvbb)) %>% as.data.frame() # calculate the mean UVB between 6/15 and 6/16 by hour.
## Which times need to be imputed?
Davis_IMPUTE_times <- unique(as.character(SEData[SEData$CSLocation %in% "Davis" & SEData$DDate %in% "6/16/2007","DTimeofday"]))
# "6-6:59pm"   "8-8:59am"   "11-11:59am" "3-3:59pm"   "9-9:59am"   "7-7:59am"   "5-5:59pm"   "4-4:59pm"
## Impute uvb data based on exposure times on 6/16/2007
for(i in Davis_IMPUTE_times){
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"CS_Location"] <- "Davis" # Fill in CS location
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"Date"] <- "6/16/2007" # Fill in date
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"Time"] <- i # fill in hour
	## Impute uvb measurement
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"avgH_uvbb"] <- Davis_IMPUTE_61607[Davis_IMPUTE_61607$Time %in% i,"avgH_uvbb"]
	
}

## Calculate times for "Homestead" date
HOME_IMPUTE_81308 <- CS_HOURLYmean %>% 
	filter(CS_Location %in% "Homestead", Date %in% c("8/12/2008", "8/14/2008")) %>% # Extract measurements from days before and after 8/13/2008
	group_by(Time) %>% # Group by hours 
	summarise(avgH_uvbb = mean(avgH_uvbb)) %>% as.data.frame() # calculate the mean UVB between 6/15 and 6/16 by hour.
## Which times need to be imputed?
HOME_IMPUTE_times <- unique(as.character(SEData[SEData$CSLocation %in% "Homestead" & SEData$DDate %in% "8/13/2008","DTimeofday"]))
# "7-7:59am"
## Impute uvb data
for(i in HOME_IMPUTE_times){
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"CS_Location"] <- "Homestead" # Fill in CS location
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"Date"] <- "8/13/2008" # Fill in date
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"Time"] <- i # fill in hour
	SEData[SEData$avgH_uvbb %in% NA & SEData$DTimeofday %in% i,"avgH_uvbb"] <- HOME_IMPUTE_81308[HOME_IMPUTE_81308$Time %in% i,"avgH_uvbb"]
	## Impute uvb measurement	
}

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Calculate Personal UVB Exposure      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Convert Date into R Date class		
SEData$Date <- as.Date(SEData$Date, format = "%m/%d/%Y")
## Convert uvb measurement in watts per meters square to joules per meters square
SEData$Joule_m2 <- SEData$avgH_uvbb * 60
## Determine sun exposure in total joules, SEI * uvb measurement.
SEData$UVB <- SEData$SEI * SEData$Joule_m2 
## Calculate sun exposure indices, , , average time outside, cumulative
JDat <- SEData %>% group_by(DSubjectID, Date) %>% 
	summarise(
		TotalUVB = sum(UVB), # cumulative UVB exposure
		TotalSEI=sum(SEI), # cumulative SEI
		TotalTO = sum(TimeOut, na.rm=TRUE), # cumulative time outside
		AVGto = mean(TimeOut, na.rm=TRUE), # average time outside
		Timedirect=sum(DDS), # cumulative time in direct sun
		meanpcBSAE=mean(pcBSAE, na.rm=TRUE) # average percent body surface area exposed
	) %>% 
	as.data.frame()
## Create Day column	
JDat$Day <- lubridate::wday(JDat$Date, label=TRUE, abbr=FALSE)
## Create Month column	
JDat$Month <- lubridate::month(JDat$Date, label=TRUE, abbr=FALSE)
## Create Year column	
JDat$Year <- lubridate::year(JDat$Date)
## Create Weekend/Weekday column	
JDat$DayEnd <- factor(ifelse(JDat$Day %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
## Change DSubjectID label to ID label	
colnames(JDat)[colnames(JDat) %in% "DSubjectID"] <- "ID"


## Prior publications suggests different sun exposure is seen between weekdays and weekend days 
## Will sum weekday and weekend exposures separately and then adjust for the number of reported days
## Weekdays
allUVB_WD <- JDat %>% filter(DayEnd %in% "Weekday") %>% group_by(ID) %>% 
	summarise(
		WDreportE=length(TotalUVB[TotalUVB != 0]), ## number of days with measured exposure
		WDsumUVB = sum(TotalUVB, na.rm=TRUE) ## cumulative exposure
	) %>% 
	mutate(WD_UVBadj = WDsumUVB/WDreportE) ## Adjust cumulative exposure by number of measured exposure 
## Change any NaN to 0.  This should be 0/0, i.e., no recorded exposure or measured uvbb over the entire study period
allUVB_WD[allUVB_WD$WD_UVBadj %in% NaN,c("WD_UVBadj")] <- 0 

## Weekend
allUVB_WE <- JDat %>% filter(DayEnd %in% "Weekend") %>% group_by(ID) %>% 
	summarise(
		WEreportE=length(TotalUVB[TotalUVB != 0]), 
		WEsumUVB = sum(TotalUVB)
	) %>% mutate(WE_UVBadj = WEsumUVB/WEreportE)
allUVB_WE[allUVB_WE$WE_UVBadj %in% NaN,c("WE_UVBadj")] <- 0
	## Join datasets and sum results
allUVB_WDWE <- full_join(allUVB_WD, allUVB_WE, by="ID") %>% 
	mutate(sumUVB_WDWE = WD_UVBadj + WE_UVBadj)

## Reported days by subject
allUVB <- JDat %>% group_by(ID) %>% 
	summarise(
		Timeoutside = sum(TotalTO, na.rm=TRUE), # cumulative time outside 
		reportE=length(Timedirect[Timedirect != 0]), # number of exposures
		sumUVB = sum(TotalUVB, na.rm=TRUE), ## cumulative UVB exposure
		AVGto=mean(AVGto, na.rm=TRUE), # average time of exposure
		TSEI=sum(TotalSEI, na.rm=TRUE), # cumulative SEI
		meanpcBSAE=mean(meanpcBSAE, na.rm=TRUE)) %>% # average % body surface are exposed
	as.data.frame()
## Join allUVB and allUVB_WDWE
allUVB <- full_join(allUVB, allUVB_WDWE %>% dplyr::select(ID, sumUVB_WDWE), by="ID")
## Adjust cumulative sun exposure by number of exposures
allUVB$UVBperreport <- allUVB$sumUVB/allUVB$reportE
## Change any NaN to 0.
allUVB$UVBperreport[allUVB$UVBperreport %in% NaN] <- 0

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Table 1      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	
		
## Add needed metadata for Table 1
TABLE1data <- inner_join(
	MetaDAT[,!(colnames(MetaDAT) %in% c("Treat", "Season", "SREthnicity","W3A","W15A","W3H","W15H","W3F","W15F"))], 
	allUVB[, colnames(allUVB) %in% c("ID","sumUVB","AVGto","UVBperreport","sumUVB_WDWE","meanpcBSAE")], by="ID")
## Convert mean percent body surface area exposed to 1-100 scale.
TABLE1data$meanpcBSAE <- round(100 * TABLE1data$meanpcBSAE, 1)
## convert height to cm
TABLE1data$ht <- 100 * TABLE1data$ht

T1ParamKey <- data.frame(Label=c("Sex", "Age", "ht", "wt", "fatkg", "pctbf", "leankg", "iaat", "waist", "gynoidf",
		"androidf","bmi", "s25OHD","WHF","dintakei","sumUVB","UVBperreport","sumUVB_WDWE","AVGto", "meanpcBSAE"),
	TableLabel = c("Sex, F/M", "Age, yrs", "Height, cm", "Weight, kg", "Total Fat Mass, kg", "Body Fat, %", "Total Lean Mass, kg", 
		"Intra-Abdominal Adipose Tissue2, cc", "Waist Circumference3, cm", "Gynoid Fat, kg","Android Fat, kg","BMI, kg/m2", 
		"25(OH)D, nmol/L","Skin Reflectance4, L*","Vitamin D Intake, IU/d","Total Sun Exposure5, J","Average Recorded Daily Sun Exposure6, J",
		"Scaled Total Sun Exposure7, J","Average Time Outside8, min", "Skin Area Exposed, %"), stringsAsFactors=FALSE
)

TABLE1data_melt <- TABLE1data %>% gather(Measures, value, -(ID:Sex))
TABLE1raw <- TABLE1data_melt %>% group_by(Measures) %>% 
	summarise(
		MEAN=round(mean(value, na.rm=TRUE),1),
		SD=round(sd(value, na.rm=TRUE),1),
		n=length(value[!(value %in% NA)])
	) %>% as.data.frame()

## Determine if there is a time difference in week 3 and week 15 measurements
## Make new data frame with only week 3 and 15 measurements 
TABLE1data_melt_W315 <- droplevels(TABLE1data_melt[grepl("15", TABLE1data_melt$Measures) | grepl("3", TABLE1data_melt$Measures),])
## Make Week column for week classification
TABLE1data_melt_W315$Week <- NA
TABLE1data_melt_W315[grepl("3", TABLE1data_melt_W315$Measures), "Week"] <- "W3"
TABLE1data_melt_W315[grepl("15", TABLE1data_melt_W315$Measures), "Week"] <- "W15"
TABLE1data_melt_W315$Week <- factor(TABLE1data_melt_W315$Week, levels=c("W3", "W15"))
TABLE1data_melt_W315[grepl("3", TABLE1data_melt_W315$Measures), "Measures"] <- 
	gsub("3", "", TABLE1data_melt_W315[grepl("3", TABLE1data_melt_W315$Measures), "Measures"])
TABLE1data_melt_W315[grepl("15", TABLE1data_melt_W315$Measures), "Measures"] <-
	gsub("15", "", TABLE1data_melt_W315[grepl("15", TABLE1data_melt_W315$Measures), "Measures"])
TABLE1data_melt_W315$Measures <- factor(TABLE1data_melt_W315$Measures)

## Run t-test on Measurements
TABLE1_ttP <- sapply(levels(TABLE1data_melt_W315$Measures), function(x) {
	DAT <- na.omit(droplevels(TABLE1data_melt_W315[TABLE1data_melt_W315$Measures %in% x,]))
	round(t.test(value ~ Week, data=DAT)$p.value, 5)
})
## Make data frame to merge with Table 1
TABLE1_ttP_DF <- data.frame(
	Measures = names(TABLE1_ttP),
	P = ifelse(TABLE1_ttP < 0.01, "<0.01",ifelse(TABLE1_ttP > 0.05, round(TABLE1_ttP, 3), "<0.05")), stringsAsFactors=FALSE
)

Table1 <- TABLE1raw %>% 
	mutate(Week =  
		ifelse(grepl("3", Measures), "Week3", 
		ifelse(grepl("15", Measures), "Week15",
		ifelse(Measures %in% c("sumUVB","AVGto","UVBperreport","sumUVB_WDWE","meanpcBSAE"), "Week15", "Week3"
		))) 
	)%>% 
	mutate(value=paste0(MEAN, " (", SD, ")")) %>% 
	mutate(Measures=gsub("3", "", Measures)) %>% 
	mutate(Measures=gsub("15", "", Measures)) %>% 
	dplyr::select(Measures, Week, value) %>% 
	dcast(Measures ~ Week) 

Table1 <- rbind(Table1, data.frame(Measures="Sex", Week3=paste0(summary(TABLE1data$Sex), collapse="/"), Week15="-")) 
TABLE1 <- full_join(Table1, TABLE1_ttP_DF, by="Measures") %>%
	mutate(
		Week3 = ifelse(Week3 %in% NA, "-", Week3),
		Week15 = ifelse(Week15 %in% NA, "-", Week15),
		P = ifelse(P %in% NA, "-", P)
	) %>%
	mutate(Measures = factor(Measures, levels=T1ParamKey$Label)) %>% 
	arrange(Measures) %>%
	mutate(Parameter = T1ParamKey$TableLabel) %>%
	dplyr::select(Parameter, Week3, Week15, P)
	
kable(TABLE1)	

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Figures 3 & 4     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Add needed data for Figures 3 and 4
Figure34data <- inner_join(
	MetaDAT[,colnames(MetaDAT) %in% c("ID", "Season","W15HF","s25OHD15","dintakei15")], 
	allUVB[, colnames(allUVB) %in% c("ID","sumUVB","AVGto","TSEI","UVBperreport","sumUVB_WDWE","meanpcBSAE")], by="ID")
Figure34data$Season <- factor(Figure34data$Season, levels=c("Fall", "Winter", "Spring", "Summer"))

Figure34labels <- data.frame(ColNames=colnames(Figure34data)[!(colnames(Figure34data) %in% c("ID", "Season"))], 
	YLabels=c("Dietary Vitamin D Intake, IU/d",
		"Skin Reflectance, L*",
		"Serum 25(OH)D, nmol/L",
		"Sun Exposure, Joules",
		"Average Time Outside, mins",
		"Sun Exposure Index, BSA x mins",
		"Average % Body Surface Area Exposed, %",
		"Sun Exposure, Joules",
		"Sun Exposure, Joules"), stringsAsFactors=FALSE
)

par(mfrow=c(3, 3))
lapply(Figure34labels$ColNames, function(x) {
	ylabel <- Figure34labels[Figure34labels$ColNames %in% x, "YLabels"]
	niceBOX(Figure34data[,c("ID", "Season", x)], "ID", "Season", bp_ylab=ylabel)
})

Figure34datamelt <- Figure34data %>% gather(Measures, value, -(ID:Season))
Figure34_KW <- sapply(unique(Figure34datamelt$Measures), function(x) {
	dat <- Figure34datamelt[Figure34datamelt$Measures %in% x,]
	dunn <- kruskal.test(value ~ Season, data=dat)$p.value
})
Figure34_KW
Figure34_DUNN <- sapply(unique(Figure34datamelt$Measures), function(x) {
	dat <- Figure34datamelt[Figure34datamelt$Measures %in% x,]
	dunn <- dunn.test(x = dat$value, g=dat$Season, method="bh")
	dunn_P <- dunn$P.adjusted
	names(dunn_P) <- gsub(" - ", "-", dunn$comparisons)
	Letters <- multcompLetters(dunn_P)$Letters
	Letters[levels(dat$Season)]
})
Figure34_DUNN

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Table 2     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
		
## Correlations among sun exposure indicies and body composition measurements.
## Add needed data for Table 2
Table2data <- inner_join(
	MetaDAT[,colnames(MetaDAT) %in% c("ID", "W15HF","s25OHD15","dintakei15","wt15","pctbf15","fatkg15","leankg15",
		"androidf15","gynoidf15","iaat15","bmi15","waist15")], 
	allUVB[, colnames(allUVB) %in% c("ID","sumUVB","AVGto","TSEI","meanpcBSAE")], by="ID")

T2ParamKey <- data.frame(Label=c("TSEI", "AVGto", "meanpcBSAE","s25OHD15","W15HF", "dintakei15", "pctbf15", "fatkg15", "leankg15",
		"gynoidf15","androidf15","iaat15","wt15","bmi15","waist15"),
	Parameter = c("SEI3", "Average Time Outside4, min", "Skin Area Exposed, %", "25(OH)D, nmol/L","Skin Reflectance, L*",
		"Vitamin D Intake, IU/d","Body Fat, %",  "Total Fat Mass, kg", "Total Lean Mass, kg","Gynoid Fat, kg", "Android Fat, kg",
		"Intra-Abdominal Adipose Tissue5, cc", "Weight, kg", "BMI, kg/m2","Waist Circumference6, cm"),
	stringsAsFactors=FALSE
)
	
## Run correlations using rcorr() function from Hmisc package
Tab2Pears <- rcorr(as.matrix(Table2data[,!(colnames(Table2data) %in% "ID")]))
## Extract measurements needed for table
Tab2Pears_r_mat <- Tab2Pears$r[!(rownames(Tab2Pears$r) %in% "sumUVB"),colnames(Tab2Pears$r) %in% c("sumUVB","TSEI","AVGto","meanpcBSAE","s25OHD15")]
Tab2Pears_P_mat <- Tab2Pears$P[!(rownames(Tab2Pears$P) %in% "sumUVB"),colnames(Tab2Pears$P) %in% c("sumUVB","TSEI","AVGto","meanpcBSAE","s25OHD15")]

## Add * for P < 0.05 and ** for P < 0.01
Table2corrDF <- as.data.frame(
	ifelse(round(Tab2Pears_P_mat, 2) < 0.01, paste0(round(Tab2Pears_r_mat, 2), "**"), 
		ifelse(round(Tab2Pears_P_mat, 2) > 0.05, as.character(round(Tab2Pears_r_mat, 2)), paste0(round(Tab2Pears_r_mat, 2), "*"
	))), stringsAsFactors=FALSE
) 
## Make column of labels to join with T2ParamKey
Table2corrDF$Label <- rownames(Table2corrDF)
## Join T2ParamKey, coerce Label into factor with levels ordered to T2ParamKey$Label, arrange by factor level order, and reorder columns
Table2 <- inner_join(Table2corrDF, T2ParamKey, by="Label") %>% 
	mutate(Label = factor(Label, levels=as.character(T2ParamKey$Label))) %>% 
	arrange(Label) %>% 
	dplyr::select(Parameter, sumUVB, TSEI, AVGto, meanpcBSAE, s25OHD15)
## Identify NA in vectors and then update redundant coefficients to '-'
TABLE2 <- as.data.frame(lapply(Table2, function(x) {
	vec <- x
	if(NA %in% vec){
		NApos <- which(vec %in% NA)
		newvec <- vec
		newvec[1:NApos] <- "-"
	} else {
		newvec <- vec
	}
	newvec
}))
## Rename column labels
colnames(TABLE2)[!(colnames(TABLE2) %in% "Parameter")] <- c(
	"Sun Exposure2",
	"SEI3",
	"Avg. Time Outside4",
	"Skin Area Exposed",
	"Serum 25(OH)D"
)

kable(TABLE2)

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Predict serum 25(OH)D using modified Hall model     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Add required data 
LModDAT <- inner_join(
	MetaDAT[,colnames(MetaDAT) %in% c("ID", "Season","W15HF","s25OHD15","dintakei15")], 
	allUVB[, colnames(allUVB) %in% c("ID","sumUVB","UVBperreport","sumUVB_WDWE")], by="ID")

## Hall Model from Hall et al (J Nutr, 2010; https://academic.oup.com/jn/article/140/3/542/4600420)
## ln(25(OHD)D) = 1.57739 + (1.12760 * J^0.1) + (0.12436 * ln(DietD)) + (0.0000003754065 * SR^3.4) - (0.57209 * winter) - (0.60856 * spring) - (0.37243 * summer)
## Where J = sun exposure in joules, DietD = dietary vitamin D in IU, SR = skin reflectance in *L	

## Calculate beta parameter for Joules
## Cumulative Joules
LModDAT$Adj_Jsum <- LModDAT$sumUVB^0.1
LModDAT$MOD_Jsum <- LModDAT$Adj_Jsum * 1.12760
## Cumulative Joules adjusted for reported exposure
LModDAT$Adj_Jperreport <- LModDAT$UVBperreport^0.1
LModDAT$MOD_Jperreport <- LModDAT$Adj_Jperreport * 1.12760
## Cumulative Joules adjusted forweekdays/weekend
LModDAT$Adj_JWDWE <- LModDAT$sumUVB_WDWE^0.1
LModDAT$MOD_JWDWE <- LModDAT$Adj_JWDWE * 1.12760

## Calculate beta parameter for Diet D
LModDAT$Adj_DietDIU <- log(LModDAT$dintakei15)
LModDAT$MOD_DietDIU <- LModDAT$Adj_DietDIU * 0.12436

## Calculate beta parameter for Skin Reflectance
LModDAT$Adj_SRHF <- LModDAT$W15HF^3.4
LModDAT$MOD_SRHF <- LModDAT$Adj_SRHF * 0.0000003754065

## Calculate Season
LModDAT$MOD_Season <- with(LModDAT, 
	ifelse(Season %in% "Fall", 0,
	ifelse(Season %in% "Winter", 0.57209,
	ifelse(Season %in% "Spring", 0.60856, 0.37243
))))

## Predict 25(OH)D with regression equation
LModDAT$Pred25raw_Jsum <- with(LModDAT, 1.57739 + MOD_Jsum + MOD_DietDIU + MOD_SRHF - MOD_Season)
LModDAT$Pred25raw_Jperreport <- with(LModDAT, 1.57739 + MOD_Jperreport + MOD_DietDIU + MOD_SRHF - MOD_Season)
LModDAT$Pred25raw_JWDWE <- with(LModDAT, 1.57739 + MOD_JWDWE + MOD_DietDIU + MOD_SRHF - MOD_Season)
## Calculate exponential for predicted 25(OH)D to get nmol/L
LModDAT$Pred25_Jsum <- exp(LModDAT$Pred25raw_Jsum)
LModDAT$Pred25_Jperreport <- exp(LModDAT$Pred25raw_Jperreport)
LModDAT$Pred25_JWDWE <- exp(LModDAT$Pred25raw_JWDWE)

## Predict 25(OH)D with regression equation minus season variable
LModDAT$Pred25rawnoS_Jsum <- with(LModDAT, 1.57739 + MOD_Jsum + MOD_DietDIU + MOD_SRHF)
LModDAT$Pred25rawnoS_Jperreport <- with(LModDAT, 1.57739 + MOD_Jperreport + MOD_DietDIU + MOD_SRHF)
LModDAT$Pred25rawnoS_JWDWE <- with(LModDAT, 1.57739 + MOD_JWDWE + MOD_DietDIU + MOD_SRHF)
## Calculate exponential for predicted 25(OH)D to get nmol/L
LModDAT$Pred25noS_Jsum <- exp(LModDAT$Pred25rawnoS_Jsum)
LModDAT$Pred25noS_Jperreport <- exp(LModDAT$Pred25rawnoS_Jperreport)
LModDAT$Pred25noS_JWDWE <- exp(LModDAT$Pred25rawnoS_JWDWE)	

## Calculate mean predicted 25(OH)D
LModDAT %>% dplyr::select(Pred25_Jsum, Pred25_Jperreport, Pred25_JWDWE, Pred25rawnoS_Jsum, Pred25rawnoS_Jperreport, Pred25rawnoS_JWDWE) %>% 
	gather(Predictions, value) %>% group_by(Predictions) %>% summarise(MEAN = mean(value), SD = sd(value)) %>% as.data.frame()
	
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Table 3     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##		

## Model with Season		
## Calculate difference between predicted and actual 25(OH)D
LModDAT$P25diff_Jsum <- LModDAT$Pred25_Jsum - LModDAT$s25OHD15
LModDAT$P25diff_Jperreport <- LModDAT$Pred25_Jperreport - LModDAT$s25OHD15
LModDAT$P25diff_JWDWE <- LModDAT$Pred25_JWDWE - LModDAT$s25OHD15

## Model without Season
## Calculate difference between predicted and actual 25(OH)D.
LModDAT$P25noSdiff_Jsum <- LModDAT$Pred25noS_Jsum - LModDAT$s25OHD15
LModDAT$P25noSdiff_Jperreport <- LModDAT$Pred25noS_Jperreport - LModDAT$s25OHD15
LModDAT$P25noSdiff_JWDWE <- LModDAT$Pred25noS_JWDWE - LModDAT$s25OHD15

## Calculate mean differences between predicted and actual 25(OH)D
LMod_wSEAS_diffmelt <- LModDAT %>% dplyr::select(P25diff_Jsum, P25diff_Jperreport, P25diff_JWDWE) %>% gather(Predictions, value)

## Calculate mean, SE and assess t-test to test whether differences does not equal zero. 
Table3wSEAScalc <- LMod_wSEAS_diffmelt %>%
	group_by(Predictions) %>% 
	summarise(
		AvgBias = round(mean(value),2), 
		BiasSEM = round(sd(value)/sqrt(length(value)),2),
		BiasSEM = round(sd(value)/sqrt(length(value)),2),
		"Tvalue" = as.character(round(t.test(value, mu=0)$statistic, 2)),
		P = ifelse(t.test(value, mu=0)$p.value < 0.0001, "<0.0001", ">0.0001")
	)
	
## Format table to match format in manuscript
Table3wSEAS <- Table3wSEAScalc %>%	mutate(
		"AverageBias1" = paste0(AvgBias, " (", BiasSEM, ")"),
		Season = rep("wSeason",length=3)
	) %>% 
	dplyr::select_(.dots=c("Predictions", "AverageBias1", "Tvalue", "P","Season")) %>%
	as.data.frame()

## Add row to distinguish 'With Season' data.
Table3wSEAS <- rbind(Table3wSEAS, data.frame(Predictions="With Season2", AverageBias1="-", Tvalue="-", P="-", Season="wSeason"))

## Test whether there are differences among sun exposure calculations
LMod_wSEAS_diffmelt$Prediction <- factor(LMod_wSEAS_diffmelt$Prediction)
## run ANOVA
wSEASaov <- aov(value ~ Prediction, data=LMod_wSEAS_diffmelt)
## Run Tukey test
wSEAS_Tuk <- glht(wSEASaov, linfct = multcomp::mcp(Prediction = "Tukey"))
## Determine letter differences
wSEAS_Tuk_letters <- cld(wSEAS_Tuk)$mcletters$Letters
## Append letters to AverageBias1 column
for(i in names(wSEAS_Tuk_letters)) {
	Table3wSEAS[Table3wSEAS$Predictions %in% i,"AverageBias1"] <- 
		paste0(Table3wSEAS[Table3wSEAS$Predictions %in% i,"AverageBias1"], wSEAS_Tuk_letters[i])
}

## Hall model without season
## Calculate mean differences between predicted and actual 25(OH)D
LMod_noSEAS_diffmelt <- LModDAT %>% dplyr::select(P25noSdiff_Jsum, P25noSdiff_Jperreport, P25noSdiff_JWDWE) %>% gather(Predictions, value)

## Calculate mean, SE and assess t-test to test whether differences does not equal zero. 
Table3noSEAScalc <- LMod_noSEAS_diffmelt %>%
	group_by(Predictions) %>% 
	summarise(
		AvgBias = round(mean(value),2), 
		BiasSEM = round(sd(value)/sqrt(length(value)),2),
		BiasSEM = round(sd(value)/sqrt(length(value)),2),
		"Tvalue" = as.character(round(t.test(value, mu=0)$statistic, 2)),
		P = ifelse(t.test(value, mu=0)$p.value < 0.0001, "<0.0001", ">0.0001")
	)   

## How much greater are the predictions without season compared to predictions with season
Table3noSEAScalc$AvgBias/Table3wSEAScalc$AvgBias
	
## Format table to match format in manuscript
Table3noSEAS <- Table3noSEAScalc  %>% mutate(
		"AverageBias1" = paste0(AvgBias, " (", BiasSEM, ")"),
		Season = rep("woutSeason",length=3)
	) %>% 
	dplyr::select_(.dots=c("Predictions", "AverageBias1", "Tvalue", "P","Season")) %>%
	as.data.frame()
	
## Add row to distinguish 'With Season' data.
Table3noSEAS <- rbind(Table3noSEAS, data.frame(Predictions="Without Season3", AverageBias1="-", Tvalue="-", P="-", Season="woutSeason"))
## Test whether there are differences by sun exposure calculations
LMod_noSEAS_diffmelt$Prediction <- factor(LMod_noSEAS_diffmelt$Prediction)
## run ANOVA
noSEASaov <- aov(value ~ Prediction, data=LMod_noSEAS_diffmelt)
## run Tukey
noSEAS_Tuk <- glht(noSEASaov, linfct = multcomp::mcp(Prediction = "Tukey"))
## Determine letter differences
noSEAS_Tuk_letters <- cld(noSEAS_Tuk)$mcletters$Letters
## Append letters to AverageBias1 column
for(i in names(noSEAS_Tuk_letters)) {
	Table3noSEAS[Table3noSEAS$Predictions %in% i,"AverageBias1"] <- 
		paste0(Table3noSEAS[Table3noSEAS$Predictions %in% i,"AverageBias1"], noSEAS_Tuk_letters[i])
}

## rbind with and without tables
Table3 <- rbind(Table3wSEAS, Table3noSEAS)

## Change row labels to match manuscript
## Set levels of factors and then sort based on factor order.
TABLE3 <- Table3 %>% mutate(
		Predictions = factor(ifelse(Predictions %in%  c("P25diff_Jperreport","P25noSdiff_Jperreport"), "Average Recorded Sun Exposure5, J",
			ifelse(Predictions %in% c("P25diff_Jsum","P25noSdiff_Jsum"), "Total Sun Exposure4, J",
			ifelse(Predictions %in% c("P25diff_JWDWE","P25noSdiff_JWDWE"), "Scaled Total Sun Exposure6, J", Predictions))), 
			levels=c("With Season2","Without Season3","Total Sun Exposure4, J", "Average Recorded Sun Exposure5, J", 
				"Scaled Total Sun Exposure6, J"
			)
		), 
		Season = factor(Season, levels=c("wSeason", "woutSeason"))
	) %>%
	arrange(Season, Predictions) %>%
	dplyr::select(-Season) 

## Set column names to match manuscript
colnames(TABLE3) <- c("Model", "Average Bias1", "T-value2", "P-value")

## View table
kable(TABLE3)

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Table 4     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Add needed data for Table 4
Table4data <- inner_join(
	MetaDAT[,colnames(MetaDAT) %in% c("ID", "Season", "W15HF","s25OHD15","dintakei15","wt15","pctbf15","fatkg15","leankg15",
		"androidf15","gynoidf15","iaat15","bmi15","waist15")], 
	allUVB[, colnames(allUVB) %in% c("ID","sumUVB")], by="ID")
	
## Multiple Linear Regressions
## Only using Hall parameters

## Linear Regression model
Table4data$Season <- factor(Table4data$Season, levels=c("Winter", "Spring", "Summer", "Fall"))
## lm summary
HallmodT4 <- lm(s25OHD15 ~ sumUVB + dintakei15 + W15HF + Season, data=Table4data)
## Check assumptions
plot(HallmodT4)
## No major pattern in Residuals vs Fitted
## Slight deviation from normality, but not too far

## Check variable inflation factor
vif(HallmodT4)
               # GVIF Df GVIF^(1/(2*Df))
# sumUVB     1.279411  1        1.131111
# dintakei15 1.042227  1        1.020895
# W15HF      1.053212  1        1.026261
# Season     1.252698  3        1.038264

## Extract coefficient matrix
HallmodT4summary <- as.data.frame(coef(summary(HallmodT4)))
## Make a column for R2, but add NAs at this stage
HallmodT4summary$R2 <- NA
## Make separate column for parameters
HallmodT4summary$Parameters <- rownames(HallmodT4summary)
## Update column names so there are no non-standard values
colnames(HallmodT4summary)[colnames(HallmodT4summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
HallmodT4summary$Estimate <- ifelse(HallmodT4summary$Parameters == "sumUVB", signif(HallmodT4summary$Estimate, 1), round(HallmodT4summary$Estimate, 2))
HallmodT4summary[,"SE"] <- ifelse(HallmodT4summary$Parameters == "sumUVB", signif(HallmodT4summary[,"SE"], 1), round(HallmodT4summary[,"SE"], 2))
HallmodT4summary[,"tval"] <- round(HallmodT4summary[,"tval"],2)
HallmodT4summary[,"P"] <- round(HallmodT4summary[,"P"],3)
## Create column to differentiate Hall model from Backwards Stepwise regression
HallmodT4summary$Model <- rep("HallMod", nrow(HallmodT4summary))
HallmodT4summary <- rbind(HallmodT4summary, 
## Add 2 rows (Winter reference, and R2) to match manuscript format
	data.frame("Estimate"=c(NA, NA), "SE"=c(NA, NA),"tval"=c(NA, NA), "P"=c(0, NA),R2=c(NA,round(summary(HallmodT4)$r.squared,3)), Parameters=c("SeasonWinter", "Hall Mod"),Model=rep("HallMod", 2)))

## LM with all parameters
allfit <- lm(s25OHD15 ~ sumUVB + dintakei15 + W15HF + wt15 + pctbf15 + fatkg15 + leankg15 + 
	androidf15 + gynoidf15 + iaat15 + bmi15 + waist15, data=Table4data)
summary(allfit)
## VIF
vif(allfit)

# Backwards Stepwise Regression
stepresults <- stepAIC(allfit, direction="backward")
summary(stepresults)
vif(stepresults)
  # sumUVB    W15HF  fatkg15 leankg15   iaat15  waist15 
# 1.156587 1.098648 1.735666 1.921879 1.444084 2.479529 
## waist circumference has vif > 2

## remove wc
BackSteplm <- lm(s25OHD15 ~ sumUVB + W15HF + fatkg15 + leankg15 + iaat15, data=Table4data)

## Check assumptions
plot(BackSteplm)
## No major pattern in Residuals vs Fitted
## Slight deviation from normality, but not too far

vif(BackSteplm, data=Table4data)
  # sumUVB    W15HF  fatkg15 leankg15   iaat15 
# 1.126927 1.073425 1.246162 1.179470 1.090310 

## Extract coefficient matrix
BackStepT4summary <- as.data.frame(coef(summary(BackSteplm)))
## Make a column for R2, but add NAs at this stage
BackStepT4summary$R2 <- NA
## Make separate column for parameters
BackStepT4summary$Parameters <- rownames(BackStepT4summary)
## Update column names so there are no non-standard values
colnames(BackStepT4summary)[colnames(BackStepT4summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
BackStepT4summary$Estimate <- ifelse(BackStepT4summary$Parameters == "sumUVB", signif(BackStepT4summary$Estimate, 1), round(BackStepT4summary$Estimate, 2))
BackStepT4summary[,"SE"] <- ifelse(BackStepT4summary$Parameters == "sumUVB", signif(BackStepT4summary[,"SE"], 1), round(BackStepT4summary[,"SE"], 2))
BackStepT4summary[,"tval"] <- round(BackStepT4summary[,"tval"],2)
BackStepT4summary[,"P"] <- round(BackStepT4summary[,"P"],3)
## Create column to differentiate Hall model from Backwards Stepwise regression
BackStepT4summary$Model <- rep("BackStep", nrow(BackStepT4summary))
## Add single row for R2 to match manuscript format
BackStepT4summary <- rbind(BackStepT4summary, 
	data.frame("Estimate"=NA, "SE"=NA,"tval"=NA, "P"=NA,R2=round(summary(BackSteplm)$r.squared,3), Parameters="Back Step",Model="BackStep"))

## rbind tables
Table4 <- rbind(HallmodT4summary, BackStepT4summary)

## Format table to match manuscript
TABLE4 <- Table4 %>% 
	mutate(
		Estimate = ifelse(Estimate %in% NA, "-", as.character(Estimate)),
		SE = ifelse(SE %in% NA, "-", as.character(SE)),
		tval = ifelse(tval %in% NA, "-", as.character(tval)),
		P = ifelse(P %in% NA, "-", 
			ifelse(P %in% 0, "Ref", 
			ifelse(P < 0.01, "<0.01", 
			ifelse(P > 0.05, as.character(P), "<0.05")))),
		R2 = ifelse(R2 %in% NA, "-", as.character(R2))
	) %>% 
	mutate(
		Estimate = paste0(Estimate, " (", SE, ")"),
		Parameters = factor(ifelse(Parameters == "(Intercept)", "Intercept",
			ifelse(Parameters == "sumUVB", "Sun Exposure2, J",
			ifelse(Parameters == "dintakei15", "Dietary Vitamin D3, IU/d",
			ifelse(Parameters == "W15HF", "Skin Reflectance4, L*",
			ifelse(Parameters == "SeasonSpring", "Spring",
			ifelse(Parameters == "SeasonSummer", "Summer",
			ifelse(Parameters == "SeasonFall", "Fall",
			ifelse(Parameters == "SeasonWinter", "Winter",
			ifelse(Parameters == "fatkg15", "Fat Mass, kg",
			ifelse(Parameters == "leankg15", "Lean Mass, kg",
			ifelse(Parameters == "iaat15", "Intra-Abdominal Adipose Tissue, cc",
			ifelse(Parameters == "Hall Mod", "Parameters from *Hall Model","Backwards Stepwise Regression5")))))))))))),
				levels=c("Parameters from *Hall Model","Backwards Stepwise Regression5","Intercept","Sun Exposure2, J","Dietary Vitamin D3, IU/d",
					 "Skin Reflectance4, L*","Winter","Spring","Summer","Fall","Fat Mass, kg","Lean Mass, kg","Intra-Abdominal Adipose Tissue, cc"
			)
		),
		Model = factor(Model, levels=c("HallMod", "BackStep"))
	) %>% 
	mutate(Estimate = gsub("\\- \\(\\-\\)", "-", Estimate)) %>% 
	arrange(Model, Parameters) %>%
	dplyr::select(Parameters, Estimate, tval, P, R2)
	
## Update colnames to match manuscript
colnames(TABLE4) <- c("Model", "Coefficient Estimate","t-value","P","R2")	

## View Table
kable(TABLE4)

        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Table 5     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	

## Add needed data for Table 5
Table5data <- inner_join(
	MetaDAT[,colnames(MetaDAT) %in% c("ID", "W15HF","s25OHD15","dintakei15","fatkg15","leankg15","iaat15","waist15")], 
	allUVB[, colnames(allUVB) %in% c("ID","sumUVB")], by="ID")
	
## Base Model
Basefit <- lm(s25OHD15 ~ sumUVB + W15HF, data=Table4data)
summary(Basefit)
vif(Basefit)

## Extract coefficient matrix
BasefitT5summary <- as.data.frame(coef(summary(Basefit)))
## Make a column for R2, but add NAs at this stage
BasefitT5summary$R2 <- NA
## Make separate column for parameters
BasefitT5summary$Parameters <- rownames(BasefitT5summary)
## Update column names so there are no non-standard values
colnames(BasefitT5summary)[colnames(BasefitT5summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
BasefitT5summary$Estimate <- ifelse(BasefitT5summary$Parameters == "sumUVB", signif(BasefitT5summary$Estimate, 1), round(BasefitT5summary$Estimate, 2))
BasefitT5summary[,"SE"] <- ifelse(BasefitT5summary$Parameters == "sumUVB", signif(BasefitT5summary[,"SE"], 1), round(BasefitT5summary[,"SE"], 2))
BasefitT5summary[,"tval"] <- round(BasefitT5summary[,"tval"],2)
BasefitT5summary[,"P"] <- round(BasefitT5summary[,"P"],3)
## Create column to differentiate models from each other
BasefitT5summary$Model <- rep("Base", nrow(BasefitT5summary))
## Add single row for R2 to match manuscript format
BasefitT5summary <- rbind(BasefitT5summary, 
	data.frame("Estimate"=NA, "SE"=NA,"tval"=NA, "P"=NA,R2=round(summary(Basefit)$r.squared,3), Parameters="Base",Model="Base"))

## IAAT Model
IAATfit <- lm(s25OHD15 ~ sumUVB + W15HF + iaat15, data=Table4data)
summary(IAATfit)
vif(IAATfit)

## Extract coefficient matrix
iaatfitT5summary <- as.data.frame(coef(summary(IAATfit)))
## Make a column for R2, but add NAs at this stage
iaatfitT5summary$R2 <- NA
## Make separate column for parameters
iaatfitT5summary$Parameters <- rownames(iaatfitT5summary)
## Update column names so there are no non-standard values
colnames(iaatfitT5summary)[colnames(iaatfitT5summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
iaatfitT5summary$Estimate <- ifelse(iaatfitT5summary$Parameters == "sumUVB", signif(iaatfitT5summary$Estimate, 1), round(iaatfitT5summary$Estimate, 2))
iaatfitT5summary[,"SE"] <- ifelse(iaatfitT5summary$Parameters == "sumUVB", signif(iaatfitT5summary[,"SE"], 1), round(iaatfitT5summary[,"SE"], 2))
iaatfitT5summary[,"tval"] <- round(iaatfitT5summary[,"tval"],2)
iaatfitT5summary[,"P"] <- round(iaatfitT5summary[,"P"],3)
## Create column to differentiate models from each other
iaatfitT5summary$Model <- rep("iaat", nrow(iaatfitT5summary))
## Add single row for R2 to match manuscript format
iaatfitT5summary <- rbind(iaatfitT5summary, 
	data.frame("Estimate"=NA, "SE"=NA,"tval"=NA, "P"=NA,R2=round(summary(IAATfit)$r.squared,3), Parameters="iaat",Model="iaat"))

## Total Lean Mass Model
Leanfit <- lm(s25OHD15 ~ sumUVB + W15HF + leankg15, data=Table4data)
summary(Leanfit)
vif(Leanfit)

## Extract coefficient matrix
LeanfitT5summary <- as.data.frame(coef(summary(Leanfit)))
## Make a column for R2, but add NAs at this stage
LeanfitT5summary$R2 <- NA
## Make separate column for parameters
LeanfitT5summary$Parameters <- rownames(LeanfitT5summary)
## Update column names so there are no non-standard values
colnames(LeanfitT5summary)[colnames(LeanfitT5summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
LeanfitT5summary$Estimate <- ifelse(LeanfitT5summary$Parameters == "sumUVB", signif(LeanfitT5summary$Estimate, 1), round(LeanfitT5summary$Estimate, 2))
LeanfitT5summary[,"SE"] <- ifelse(LeanfitT5summary$Parameters == "sumUVB", signif(LeanfitT5summary[,"SE"], 1), round(LeanfitT5summary[,"SE"], 2))
LeanfitT5summary[,"tval"] <- round(LeanfitT5summary[,"tval"],2)
LeanfitT5summary[,"P"] <- round(LeanfitT5summary[,"P"],3)
## Create column to differentiate models from each other
LeanfitT5summary$Model <- rep("Lean", nrow(LeanfitT5summary))
## Add single row for R2 to match manuscript format
LeanfitT5summary <- rbind(LeanfitT5summary, 
	data.frame("Estimate"=NA, "SE"=NA,"tval"=NA, "P"=NA,R2=round(summary(Leanfit)$r.squared,3), Parameters="Lean",Model="Lean"))

## WC Model
WCfit <- lm(s25OHD15 ~ sumUVB + W15HF + waist15, data=Table4data)
summary(WCfit)
vif(WCfit)

## Extract coefficient matrix
WCfitT5summary <- as.data.frame(coef(summary(WCfit)))
## Make a column for R2, but add NAs at this stage
WCfitT5summary$R2 <- NA
## Make separate column for parameters
WCfitT5summary$Parameters <- rownames(WCfitT5summary)
## Update column names so there are no non-standard values
colnames(WCfitT5summary)[colnames(WCfitT5summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
WCfitT5summary$Estimate <- ifelse(WCfitT5summary$Parameters == "sumUVB", signif(WCfitT5summary$Estimate, 1), round(WCfitT5summary$Estimate, 2))
WCfitT5summary[,"SE"] <- ifelse(WCfitT5summary$Parameters == "sumUVB", signif(WCfitT5summary[,"SE"], 1), round(WCfitT5summary[,"SE"], 2))
WCfitT5summary[,"tval"] <- round(WCfitT5summary[,"tval"],2)
WCfitT5summary[,"P"] <- round(WCfitT5summary[,"P"],3)
## Create column to differentiate models from each other
WCfitT5summary$Model <- rep("WC", nrow(WCfitT5summary))
## Add single row for R2 to match manuscript format
WCfitT5summary <- rbind(WCfitT5summary, 
	data.frame("Estimate"=NA, "SE"=NA,"tval"=NA, "P"=NA,R2=round(summary(WCfit)$r.squared,3), Parameters="WC",Model="WC"))

## Total Fat Mass Model
Fatfit <- lm(s25OHD15 ~ sumUVB + W15HF + fatkg15, data=Table4data)
summary(Fatfit)
vif(Fatfit)

## Extract coefficient matrix
FatfitT5summary <- as.data.frame(coef(summary(Fatfit)))
## Make a column for R2, but add NAs at this stage
FatfitT5summary$R2 <- NA
## Make separate column for parameters
FatfitT5summary$Parameters <- rownames(FatfitT5summary)
## Update column names so there are no non-standard values
colnames(FatfitT5summary)[colnames(FatfitT5summary) %in% c("Std. Error","t value","Pr(>|t|)")] <-c("SE", "tval", "P")
## Round all data to match manuscript
FatfitT5summary$Estimate <- ifelse(FatfitT5summary$Parameters == "sumUVB", signif(FatfitT5summary$Estimate, 1), round(FatfitT5summary$Estimate, 2))
FatfitT5summary[,"SE"] <- ifelse(FatfitT5summary$Parameters == "sumUVB", signif(FatfitT5summary[,"SE"], 1), round(FatfitT5summary[,"SE"], 2))
FatfitT5summary[,"tval"] <- round(FatfitT5summary[,"tval"],2)
FatfitT5summary[,"P"] <- round(FatfitT5summary[,"P"],3)
## Create column to differentiate models from each other
FatfitT5summary$Model <- rep("Fat", nrow(FatfitT5summary))
## Add single row for R2 to match manuscript format
FatfitT5summary <- rbind(FatfitT5summary, 
	data.frame("Estimate"=NA, "SE"=NA,"tval"=NA, "P"=NA,R2=round(summary(Fatfit)$r.squared,3), Parameters="Fat",Model="Fat"))

## rbind tables
Table5 <- rbind(BasefitT5summary,iaatfitT5summary, LeanfitT5summary,WCfitT5summary, FatfitT5summary)

## Format table to match manuscript
TABLE5 <- Table5 %>% 
	mutate(
		Estimate = ifelse(Estimate %in% NA, "-", as.character(Estimate)),
		SE = ifelse(SE %in% NA, "-", as.character(SE)),
		tval = ifelse(tval %in% NA, "-", as.character(tval)),
		P = ifelse(P %in% NA, "-", 
			ifelse(P %in% 0, "Ref", 
			ifelse(P < 0.01, "<0.01", 
			ifelse(P > 0.05, as.character(P), "<0.05")))),
		R2 = ifelse(R2 %in% NA, "-", as.character(R2))
	) %>% 
	mutate(
		Estimate = paste0(Estimate, " (", SE, ")"),
		Parameters = factor(ifelse(Parameters == "(Intercept)", "Intercept",
			ifelse(Parameters == "sumUVB", "Sun Exposure2, J",
			ifelse(Parameters == "W15HF", "Skin Reflectance3, L*",
			ifelse(Parameters == "fatkg15", "Total Fat Mass, kg",
			ifelse(Parameters == "leankg15", "Total Lean Mass, kg",
			ifelse(Parameters == "iaat15", "IAAT, cc",
			ifelse(Parameters == "waist15", "Waist Circumference, cm",
			ifelse(Parameters == "Base", "Base Model",
			ifelse(Parameters == "iaat", "Base Model with IAAT4 included",
			ifelse(Parameters == "Lean", "Base Model with lean mass included",
			ifelse(Parameters == "WC", "Base Model with waist circumference5 included","Base Model with total fat mass included"))))))))))),
				levels=c("Base Model","Base Model with IAAT4 included","Base Model with lean mass included", "Base Model with waist circumference5 included",
				"Base Model with total fat mass included","Intercept","Sun Exposure2, J","Skin Reflectance3, L*",
				"IAAT, cc","Total Lean Mass, kg","Waist Circumference, cm","Total Fat Mass, kg"
			)
		),
		Model = factor(Model, levels=c("Base", "iaat","Lean", "WC","Fat"))
	) %>% 
	mutate(Estimate = gsub("\\- \\(\\-\\)", "-", Estimate)) %>% 
	arrange(Model, Parameters) %>%
	dplyr::select(Parameters, Estimate, tval, P, R2)
	
## Update colnames to match manuscript
colnames(TABLE5) <- c("Model", "Coefficient Estimate","t-value","P","R2")	

## View Table
kable(TABLE5)







        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     R and Package versions     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##	


sessionInfo()
# > sessionInfo() 
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1

# Matrix products: default

# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
 # [1] knitr_1.20         RColorBrewer_1.1-2 bindrcpp_0.2.2     car_3.0-0          carData_3.0-1      Hmisc_4.1-1        Formula_1.2-3     
 # [8] lattice_0.20-35    geosphere_1.5-7    elevatr_0.2.0      dunn.test_1.3.5    reshape2_1.4.3     multcomp_1.4-8     TH.data_1.0-9     
# [15] MASS_7.3-50        survival_2.42-3    mvtnorm_1.0-8      forcats_0.3.0      stringr_1.3.1      dplyr_0.7.6        purrr_0.2.5       
# [22] readr_1.1.1        tidyr_0.8.1        tibble_2.0.1       ggplot2_3.1.0      tidyverse_1.2.1   

# loaded via a namespace (and not attached):
 # [1] httr_1.3.1          jsonlite_1.5        splines_3.5.1       modelr_0.1.2        assertthat_0.2.0    highr_0.7          
 # [7] sp_1.3-1            latticeExtra_0.6-28 cellranger_1.1.0    progress_1.2.0      pillar_1.3.1        backports_1.1.2    
# [13] glue_1.3.0          digest_0.6.15       checkmate_1.8.5     rvest_0.3.2         colorspace_1.3-2    sandwich_2.4-0     
# [19] htmltools_0.3.6     Matrix_1.2-14       plyr_1.8.4          pkgconfig_2.0.2     broom_0.5.0         haven_2.0.0        
# [25] scales_0.5.0        openxlsx_4.1.0      rio_0.5.10          htmlTable_1.12      withr_2.1.2         nnet_7.3-12        
# [31] lazyeval_0.2.1      cli_1.0.1           magrittr_1.5        crayon_1.3.4        readxl_1.1.0        fansi_0.4.0        
# [37] nlme_3.1-137        xml2_1.2.0          foreign_0.8-70      prettyunits_1.0.2   tools_3.5.1         data.table_1.11.8  
# [43] hms_0.4.2           munsell_0.5.0       zip_1.0.0           cluster_2.0.7-1     compiler_3.5.1      rlang_0.3.1        
# [49] grid_3.5.1          rstudioapi_0.9.0    htmlwidgets_1.3     base64enc_0.1-3     gtable_0.2.0        codetools_0.2-15   
# [55] abind_1.4-5         curl_3.2            R6_2.2.2            gridExtra_2.3       zoo_1.8-3           lubridate_1.7.4    
# [61] rgdal_1.4-3         utf8_1.1.4          bindr_0.1.1         stringi_1.1.7       Rcpp_1.0.0          rpart_4.1-13       
# [67] acepack_1.4.1       tidyselect_0.2.4   




























