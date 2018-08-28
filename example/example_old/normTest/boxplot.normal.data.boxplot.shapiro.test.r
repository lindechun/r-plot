
if ("shapiro.test" == "lillieTest"){
	library('fBasics')
}

if(! FALSE){

	data <- read.table(file="boxplot.normal.data", sep="\t", header=TRUE,
	row.names=1, quote="")
	if ("GeType" != "variable"){
		data_m <- melt(data, id.vars=c("GeType"))
	} else {
		data_m <- melt(data)
	}
} else {
	data_m <- read.table(file="boxplot.normal.data", sep="\t",
	header=TRUE)
}

if ("" != ""){
	data_m$GeType <- cut(data_m$GeType, )
} 
if ("" != ""){
	data_m$GeType <- cut(data_m$GeType,)
}

if ("GeType" == "GeType"){
	#No Group information
	variableL <- unique(data_m$GeType)
	len_var <- length(variableL)
	for(i in 1:len_var){
		var1 <- variableL[i]
		new_data <- data_m[data_m$GeType == var1,]$value
		if ("shapiro.test" == "lillieTest"){
			if (length(new_data) > 4) {
				print(paste("### Compute normality for", var1,  "###"))
				print(shapiro.test(new_data))
			} else {
				print(paste("### No enough data for lillieTest:", var1, "###"))
			}
		} else if ("shapiro.test" == "shapiro.test") {
			if (length(new_data) > 2) {
				print(paste("### Compute normality for", var1, "###"))
				print(shapiro.test(new_data))
			} else {
				print(paste("### No enough data for shapiro.test:",
				var1, "###"))
			}
		} else {
			print(paste("### Compute normality for", var1,  "###"))
			print(shapiro.test(new_data))
		}
	}
} else {
	#Compute several groups
	group <- names(summary(data_m$GeType))
	for (i in group){
		tmp <- data_m[data_m$GeType==i,]
		print(paste("*** Compute for Group ", i, " ***"))

		variableL <- unique(tmp$variable)
		len_var <- length(variableL)
		for(i in 1:len_var){
			var1 <- variableL[i]
			new_data <- tmp[tmp$variable == var1,]$value
			if ("shapiro.test" == "lillieTest"){
				if (length(new_data) > 4) {
					print(paste("### Compute normality for", var1, "###"))
					print(shapiro.test(new_data))
				} else {
					print(paste("### No enough data for lillieTest:",
					var1, "###"))
				}
			} else if ("shapiro.test" == "shapiro.test") {
				if (length(new_data) > 2) {
					print(paste("### Compute normality for", var1, "###"))
					print(shapiro.test(new_data))
				} else {
					print(paste("### No enough data for shapiro.test:",
					var1, "###"))
				}
			}else {
				print(paste("### Compute normality for", var1,  "###"))
				print(shapiro.test(new_data))
			}
		}
	}
}

