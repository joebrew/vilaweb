# cis <- vilaweb::cis
# cis <- haven::as_factor(cis)
# # Adjust names in new ceo
# for(j in 1:ncol(cis)){
#   message(j)
#   col_name <- names(cis)[j]
#   this_name <- attr(eval(parse(text = paste0('cis$`', col_name, '`'))), 'label')
#   names(cis)[j] <- this_name
# }

cis_list <- vilaweb::cis_list
