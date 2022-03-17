#joins and pivots
#2022-Mar-15

library(tidyverse)
data1 = data.frame(ID=c(1,2)), X1=c("a1", "a2")
data2 = data.frame(ID=c(2,3)), X2=c("b1", "b2")

data1

left_join(data1, data2, by="ID")#join by ID
left_join(data1, data2, by=c("ID"=="ID")
          
data12= data1%>%
  left_join(data2, by="ID")

inner_table 
anti_join()
#%in%In
#check the nubber


survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle_n = c(2, 11, 8, 27),
                    chiton_n = c(1, 0, 0, 2),
                    mussel_n = c(0, 1, 1, 4))
lomg(survey)

wide= longley#we did the dios ecer
#