```{r setup}
library(knitr) # for changing workspace
library(stringr) # for using some regex functions
opts_chunk$set(root.dir = 'I:/R Data/141')
options(width = 110) 
load("vehicles.rda")
vehicle = vposts     # make a copy of the orignial data
body = vehicle$body
model_google = read.csv("model-google.csv")
```


```{r function}
value = function(regex,data)
{
  regex_fun = regex
  
  # It will store all the matched values in the posts return as a list
  result_fun= str_extract_all(data, ignore.case(regex_fun))
  
  # If there are multiple values in the list, get the first one out, if there is no value, give it NA
  value_fun = sapply(result_fun,function(x) {               
       if (length(x)>0) x[1]
       else NA
       })
  
  # how many posts contain the pattern 
  number = as.numeric(table(is.na(value_fun))[1])   
  
  # the posts that don't have that pattern 
  check = data[is.na(value_fun)]                             
  return(list(value_fun,number,check))                     
  
}
```


**Extract the price being asked for the vehicle from the body column, if it is present, and check if it agrees with the actual price in the price column.**

```{r}
# regular expression for price
price = "\\$?(\\d{0,3}[,.]?)*\\d{1,3}"           
price_value = value(price,body)                            

# how many posts have price in body
price_value[2]

# remove "$", "," or "." and then transform to numeric
price_value = as.numeric(gsub("\\$|,|.", "", price_value[[1]]))

# compare the price from "body" and from price column
table(price_value == vposts$price)                                
```

**Extract a Vehicle Identication Number (VIN) from the body, if it is present. We could use this to  both identify details of the car (year it was built, type and model of the car, safety features, body style, engine type, etc.) and also use it to get historical information about the particular car. Add the VIN, if available,to the data frame. How many postings include the VIN?**

```{r}
# regular expression for vin number
vin = "VIN[ :-]? ?\\d*\\w{1,7}\\d{1,3}\\w{1,5}\\d{4,8}"           
vin_num = value(vin,body)  

# how many posts have price in body
vin_num[2]
```

```{r}
# rough way to get the vin number
vin = "[A-HJ-NPR-Z0-9]{17}"                                     
vin_num1 = value(vin,body)  

# how many posts have price in body
vin_num1[2]

# look it the value 
 VIN = data.frame(vin_num1[[1]])
#View(VIN)
 vposts$vin = vin_num[[1]]
```

**Extract phone numbers from the body column, and again add these as a new column. How many posts include a phone number?**

```{r}
# regular expression for vin number
phone = "\\(?\\d{3}\\)?[ ]?[ -]?\\d{3}-? ?\\d{4}"               
phone_num = value(phone,body) 

# how many posts have price in body
phone_num[2]

```

**Extract email addresses from the body column, and again add these as a new column. How many posts include an email address?**

```{r}
# regular expression for vin number
email = "[[:alnum:]|[:punct:]]+@[[:alnum:]|[:punct:]]+?\\.(com|net|org|edu|gov){1}"     
email_add = value(email,body)

# how many posts have price in body
email_add[2]

# grep the posts have "email"
email = body[grepl("email", body, ignore.case = TRUE)]  

# how many posrs have email
length(email)

# look at some posts have "email"
email[1:3]
```

**Find the year in the description or body and compare it with the value in the year column.**

```{r}
# regular expression for year
year = " ?(19|20)\\d{2} ?"       
year_b = value(year,body)

# how many posrs have year
year_b[2]

# compare to the year column
year_num = as.numeric(year_b[[1]])
table(year_num == vposts$year) 
```

**Determine the model of the car, e.g., S60, Boxter, Cayman, 911, Jetta. This includes correcting mis-spelled or abbreviated model names. You may find the agrep() function useful. You should also use statistics, i.e., counts to see how often a word occurs in other posts and if such a spelling is reasonable, and whether this model name has been seen with that maker often. When doing these questions, you will very likely have to iterate by developing a regular expression, and seeing what results it gives you and adapting it. Furthermore, you will probably have to use two or more strategies when looing for a particular piece of information. This is expected; the data are not nice and regularly formatted.**

```{r}

# get the model from the "title" column
#title = "\\d{2,4} ([A-z]+[:punct:]?[A-z]+) ([A-z0-9]+)" 
#new_title  = value(title,vposts$title) 
#model = gsub(".*\\d{2,4} ([A-z]+) ([A-z0-9]+).*" , "\\2", new_title[[1]])
#model = casefold(model, upper = F)

# save it as csv (won't do it again, since I already saved it)
#vposts$model = model
#write.csv(vposts[,c(1,28)], file = "model.csv")

# read the sort data from google refine
#model_google = read.csv("model-google.csv")

# redefine model column
vposts$model = model_google[,3]

```