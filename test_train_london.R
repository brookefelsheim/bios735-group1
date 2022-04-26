load("package/bikeSharing/data/london.rda")

train = london[london$Year == "Year 1",]
test = london[london$Year == "Year 2",]
