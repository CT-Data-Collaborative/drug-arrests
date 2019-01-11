library(acs)
source('./scripts/acsHelpers.R')

# ACS B01001
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2016,
    table = "B01001"
)

pops <- data.table()
for (data in acsdata) {
    year <- data@endyear
    pop.total <- acsSum(data, 1, "Total")
    pop.over9 <- acsSum(data, c(5:25, 29:49), "10 years and over")
    pop.1017 <- acsSum(data, c(5:6, 29:30), "10 to 17 years")	
    pop.over17 <- acsSum(data, c(7:25, 31:49), "18 years and over") 
    pop.1824 <- acsSum(data, c(7:10, 31:34), "18 to 24 years") 
    
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(pop.total), 
        estimate(pop.over9), 
        estimate(pop.1017),
        estimate(pop.over17),
        estimate(pop.1824)
    )
    
    names(estimates)[names(estimates) == "HD01_VD01.Estimate; Total:"] <- "Total"

    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "Pop",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(pop.total) * 1.645, 
        standard.error(pop.over9) * 1.645, 
        standard.error(pop.1017) * 1.645,
        standard.error(pop.over17) * 1.645,
        standard.error(pop.1824) * 1.645
    )
    
    names(moes)[names(moes) == "HD01_VD01.Estimate; Total:"] <- "Total"
    

    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Age Range",
        variable.factor = F,
        value.name = "MOE",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Age Range`)
    setkey(moes, FIPS, Year, `Age Range`)

    pops <- rbind(pops, estimates[moes])
}

pops <- pops[pops$FIPS != "0900100000",]

# Write to File
write.table(
    pops,
    file.path(getwd(), "raw", "populations.csv"),
    sep = ",",
    row.names = F
)
