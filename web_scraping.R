library(readr)
library(rvest)
library(dplyr)
library(stringr)
# page_biol = read_html("http://biology.duke.edu/courses")
# table_biol = page_biol %>%
#   html_node("#block-views-courses-block > div > div > div > table") %>%
#   html_table() %>%
#   filter(str_detect(.[,"Course Notes"], "offered Spring 2018"))
make_store_url = function(department, number){
  url_part1 = "http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&this_category=1&term=SP18&store=320&step=5&qty=1000&listtype=begin&go=Go&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&department="
  url_part2 = department
  url_part3 = "&course="
  url_part4 = as.character(number)
  url_part5 = "&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23cccccc&action=list_courses&section=01&Go=Go"
  store_url = paste(url_part1, url_part2, url_part3, url_part4, url_part5, sep = "")
  return(store_url)
}
biol_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&this_category=1&term=SP18&store=320&step=3&listtype=begin&go=Go&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=BIOLOGY&Go=Go")
biol_courses = rep(NA, 200)
for (i in 2:200){
  biol_courses[i-1] = biol_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
biol_courses[is.na(biol_courses)] = ""

chem_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&go=Go&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=CHEM&Go=Go")
chem_courses = rep(NA, 200)
for (i in 2:200){
  chem_courses[i-1] = chem_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
chem_courses[is.na(chem_courses)] = ""

biochem_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=BIOCHEM&Go=Go")
biochem_courses = rep(NA, 200)
for (i in 2:200){
  biochem_courses[i-1] = biochem_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
biochem_courses[is.na(biochem_courses)] = ""

biostat_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=BIOSTAT&Go=Go")
biostat_courses = rep(NA, 200)
for (i in 2:200){
  biostat_courses[i-1] = biostat_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
biostat_courses[is.na(biostat_courses)] = ""

cell.molec.bio_courses = c("640", rep("", 199))

cellbio_courses = c("493", "503", "668", rep("", 197))

compsci_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=COMPSCI&Go=Go")
compsci_course = rep(NA, 200)
for (i in 2:200){
  compsci_courses[i-1] = compsci_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
compsci_courses[is.na(compsci_courses)] = ""

evanth_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=EVANTH&Go=Go")
evanth_courses = rep(NA, 200)
for (i in 2:200){
  evanth_courses[i-1] = evanth_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
evanth_courses[is.na(evanth_courses)] = ""

math_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=MATH&Go=Go")
math_courses = rep(NA, 200)
for (i in 2:200){
  math_courses[i-1] = math_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
math_courses[is.na(math_courses)] = ""

neurobio_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=NEUROBIO&Go=Go")
neurobio_courses = rep(NA, 200)
for (i in 2:200){
  neurobio_courses[i-1] = neurobio_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
neurobio_courses[is.na(neurobio_courses)] = ""

neurosci_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=NEUROSCI&Go=Go")
neurosci_courses = rep(NA, 200)
for (i in 2:200){
  neurosci_courses[i-1] = neurosci_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
neurosci_courses[is.na(neurosci_courses)] = ""

physics_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=PHYSICS&Go=Go")
physics_courses = rep(NA, 200)
for (i in 2:200){
  physics_courses[i-1] = physics_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
physics_courses[is.na(physics_courses)] = ""

psy_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=PSY&Go=Go")
psy_courses = rep(NA, 200)
for (i in 2:200){
  psy_courses[i-1] = psy_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
psy_courses[is.na(psy_courses)] = ""

sta_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=STA&Go=Go")
sta_courses = rep(NA, 200)
for (i in 2:200){
  sta_courses[i-1] = sta_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
sta_courses[is.na(sta_courses)] = ""

mgm_page = read_html("http://dukebooks.collegestoreonline.com/ePOS?wpd=1&width=100%25&term=SP18&store=320&step=3&listtype=begin&form=shared3%2Ftextbooks%2Fno_jscript%2Fmain.html&design=duke_textbooks&colspan=3&cellspacing=1&cellpadding=0&campus=MAIN&border=0&bgcolor=%23eeeeee&department=MGM&Go=Go")
mgm_courses = rep(NA, 200)
for (i in 2:200){
  mgm_courses[i-1] = mgm_page %>%
    html_node(paste0("#course > option:nth-child(",i,")")) %>%
    html_attr("value")
}
mgm_courses[is.na(mgm_courses)] = ""

classes.df = data.frame(biochem_courses,
                        biol_courses,
                        biostat_courses,
                        cell.molec.bio_courses,
                        chem_courses,
                        compsci_courses,
                        evanth_courses,
                        math_courses,
                        mgm_courses,
                        neurobio_courses,
                        neurosci_courses,
                        physics_courses,
                        psy_courses,
                        sta_courses)
save(classes.df, file = "classes.Rdata")