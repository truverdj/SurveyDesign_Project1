library(dplyr)
load("sample.Rdata")
sample.df$numtexts = rep(NA, nrow(sample.df))
sample.df$newprice = rep(NA, nrow(sample.df))
sample.df$usedprice = rep(NA, nrow(sample.df))
n = nrow(sample.df)
##############################################
# This function does not always return a valid url
# Some classes may be listed as section 001 reather than 01
##############################################
make_store_url = function(department, number){
  url_part1 = "http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&this_category=1&term=SP18&store=320&step=5&qty=1000&listtype=begin&go=Go&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&department="
  url_part2 = department
  url_part3 = "&course="
  url_part4 = as.character(number)
  url_part5 = "&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23cccccc&action=list_courses&section=01&Go=Go"
  store_url = paste(url_part1, url_part2, url_part3, url_part4, url_part5, sep = "")
  return(store_url)
}

x = rep(NA, n)
for (i in seq_along(x)){
  x[i] = make_store_url(sample.df[i,"department"], sample.df[i, "course"])
}

sample.df$numtexts[(n/2 + 1):n] = c(0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,1,
                                    0,1,0,0,0,1,0,0)
sample.df$newprice[(n/2 + 1):n] = c(0,253.50,0,0,0,117.50,0,0,0,0,52.50,0,0,0,0,0,48.00,0,251.75,
                                    0,92.75,0,0,0,200.00,0,0)
sample.df$usedprice[(n/2 +1):n] = c(0,190.25,0,0,0,88.25,0,0,0,0,39.50,0,0,0,0,0,36.00,0,189.00,
                                    0,69.75,0,0,0,150.00,0,0)

surveyData = sample.df

save(surveyData, file = "surveyData.Rdata")
