#1 ST QUESTION  ANSWER 

london_crime =read.csv("london-crime-data.csv") 
View(london_crime) #structure of the dataset str(london_crime) london_crime$Date <- paste(london_crime$day, london_crime$month, london_crime$year, sep = "-") london_crime <- london_crime[, !names(london_crime) %in% c("day", "month", 
"year")] str(london_crime) 


#2nd Question answer 


london_crime <- london_crime[, c("borough", "major_category", "minor_category", "value", "Date")] names(london_crime) <- c("Borough", "MajorCategory", "SubCategory", "Value", "CrimeDate") str(london_crime) 


#3rd Question answer 

london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, format = 
                                    "%d-%m-%Y") 
str(london_crime$CrimeDate) 
head(london_crime$CrimeDate) 

#4 th Question answer 

borough_summary <- table(london_crime$Borough) 
barplot(borough_summary, main = "Crime Summary by Borough", xlab = "Borough", ylab = "Number of Crimes") 
borough_Highcrime <- names(borough_summary)[which.max(borough_summary)] 
print(paste("Borough  highest level of crime:", borough_Highcrime)) borough_Lowcrime <- names(borough_summary)[which.min(borough_summary)] print(paste("Borough  lowest level of crime:", borough_Lowcrime))  
#5 TH Question answer 


# Aggregate crime values by MajorCategory 
crime_values_by_category <- aggregate(Value ~ MajorCategory, data=london_crime1, FUN=sum) 

# Convert the aggregated data into a named vector for the pie chart crime_values <- setNames(crime_values_by_category$Value, crime_values_by_category$MajorCategory) 

# Determine the highest and lowest categories of crime highest_crime_category <- names(which.max(crime_values)) lowest_crime_category <- names(which.min(crime_values)) highest_crime_category 
lowest_crime_category  

windows(16,10) 

# Creating a pie chart to display the data 
pie(crime_values, main="Major Categories of Crime in London", col=rainbow(length(crime_values)), labels=names(crime_values)) 



#6 th Question answer 


# Create a mapping of Borough to Region borough_to_region <- list(   "Barking and Dagenham" = "East", 
"Barnet" = "North", 
"Bexley" = "East", 
"Brent" = "West", 
"Bromley" = "South", 
"Camden" = "North", 
"Croydon" = "South", 
"Ealing" = "West", 
"Enfield" = "North", 
"Greenwich" = "East", 
"Hackney" = "North", 
"Hammersmith and Fulham" = "West", 
"Haringey" = "North", 
"Harrow" = "West", 
"Havering" = "East", 
"Hillingdon" = "West", 
"Hounslow" = "West", 
"Islington" = "Central", 
"Kensington and Chelsea" = "Central", 
"Kingston upon Thames" = "East",  
"Lambeth" = "Central", 
"Lewisham" = "Central", 
"Merton" = "South", 
"Newham" = "East", 
"Redbridge" = "East", 
"Richmond upon Thames" = "West", 
"Southwark" = "Central", 
"Sutton" = "South", 
"Tower Hamlets" = "Central", 
"Waltham Forest" = "Central", 
"Wandsworth" = "East",  
"Westminster" = "Central" 
) 

# Assign Region to each Borough in the dataframe 
london_crime1$Region <- sapply(london_crime1$Borough, function(x) ifelse(is.null(borough_to_region[[x]]), NA, borough_to_region[[x]])) 

# Automatically replace NA regions with a default value if needed # For example, assigning 'Unknown' to any NA regions 
london_crime1$Region[is.na(london_crime1$Region)] <- 'Unknown' View(london_crime1) 




#7 th Question answer 


# Aggregate crime values by Region 
crime_values_by_region <- aggregate(Value ~ Region, data=london_crime1, FUN=sum) 

# Sort the aggregated data to make it easier to identify the regions with the highest and lowest crimes crime_values_by_region <- crime_values_by_region[order(-crime_values_by_region$Value), ] 

# Identify the region with the highest and lowest number of crimes highest_crime_region <- crime_values_by_region$Region[1] highest_crime_value <- crime_values_by_region$Value[1] 
lowest_crime_region <- crime_values_by_region$Region[nrow(crime_values_by_region)] lowest_crime_value <- crime_values_by_region$Value[nrow(crime_values_by_region)] highest_crime_region highest_crime_value  lowest_crime_region lowest_crime_value windows() 
plot(crime_values_by_region$Region, crime_values_by_region$Value, type="o", col="blue",       main="Reported Crimes by Region in London", xlab="Region", ylab="Number of Reported Crimes") 
text(crime_values_by_region$Region, crime_values_by_region$Value, labels=crime_values_by_region$Value, pos=3, cex=0.8 
     
     
     
     
     
 #    8 th Question answer 
     
     
     # Extract subset of data with the highest number of crimes 
     highest_crime_region <- names(region_summary)[which.max(region_summary)] subset_highest_crime <- london_crime[london_crime$Region == highest_crime_region, ] 
     
     # Extract subset of data with the lowest number of crimes lowest_crime_region <- names(region_summary)[which.min(region_summary)] subset_lowest_crime <- london_crime[london_crime$Region == lowest_crime_region, ] 
     
     # Comment the major crime category of both regions 
     # Major crime category of region with the highest number of crimes: 
     table(subset_highest_crime$MajorCategory) 
     # Major crime category of region with the lowest number of crimes: 
      table(subset_lowest_crime$MajorCategory) 
     
     
     
     
     #9 th Question answer 
     
     par(mfrow=c(1,2))  # Set up the plotting area for side-by-side plots 
     
     # Plot for subset with the highest number of crimes barplot(table(subset_highest_crime$MajorCategory),         main = paste("Major Categories of Crime in", highest_crime_region),         xlab = "Major Category",         ylab = "Number of Crimes", 
     ylim = c(0, max(region_summary)),  # Set y-axis limit to the maximum number of crimes across all regions         las = 2)  # Rotate x-axis labels vertically 
     
     # Plot for subset with the lowest number of crimes barplot(table(subset_lowest_crime$MajorCategory), 
     main = paste("Major Categories of Crime in", lowest_crime_region),         xlab = "Major Category",         ylab = "Number of Crimes", 
     ylim = c(0, max(region_summary)),  # Set y-axis limit to the maximum number of crimes across all regions         las = 2)  # Rotate x-axis labels vertically 
     
     
     
     #10 th Question answer 
     
     
     
     # Saving modified london_crime data frame as london-crimemodified.csv
     write.csv(london_crime, "london-crimemodified.csv", row.names = FALSE) 
     
     
