***** TRANSLINK PROJECT *****

library(twitteR)

dsTweetsDF=twListToDF(data)

translink = function(year, month, day, hour)
{
  start=c()
  stop=c()
  if(month < 10){
    month = paste(0, toString(month), sep = '')
  }else{
    month = toString(month)
  }
  
  if(day < 10){
    day = paste(0, toString(day), sep = '')
  }else{
    day = toString(day)
  }
  
  if(hour < 10){
    hour = paste(0, toString(hour), sep = '')
  }else{
    hour = toString(hour)
  }
  year = toString(year)
  date = paste(year, month, day,  sep = '-')
  date = paste(date, hour)
  for (i in 1:982) {
    if (grepl(date, dsTweetsDF$created[i])) {
      if (grepl("end|over", dsTweetsDF$text[i])) {
        print(dsTweetsDF$text[i])
        ans1=regexpr(".?[1-9]{0-9}{0,2}\\d+ ", dsTweetsDF$text[i])
        ans2=unique(unlist(regmatches(dsTweetsDF$text[i], ans1)))
        stop=c(stop,ans2)
      }else{
        ans3=regexpr(".?[1-9]{0-9}{0,2}\\d+ ", dsTweetsDF$text[i])
        print(dsTweetsDF$text[i])
        ans4=unique(unlist(regmatches(dsTweetsDF$text[i], ans3)))
        start=c(start,ans4)
      }
      
    }
    
  }
  return(list(start = start, stop = stop))
}
disruptions=translink(2020, 1, 26, 3)


disruptions$start
