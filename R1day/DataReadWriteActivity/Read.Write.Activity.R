# Activity for Read-Write section

# 1. Locate the file 'fish_data.csv'. Set the working directory.

# 2. Use a function to read in the file. Assign this new data an object name. 
  dat = read.csv("fish_data.csv")

# 3. Look at the data. Maybe with the function 'head'
  head(dat)

# 4. Get the mean of all the columns with continuous values. Use the function 'mean'
#    for each column, separately.
  mean(dat$Weight)
  mean(dat$Length1)
  mean(dat$Height)
  mean(dat$Width)

# Two of these columns will give you problems. A different solution is needed
# for each.
  
# If you are getting an NA from R, perhaps look at the help R documentation for the
# function '?mean' and see if there is an argument you need to consider.

# 5. Now write the function again and you should see the value of the mean.
  mean(dat$Weight,na.rm = TRUE)
  
# 6. If you are getting a warning message, check out that column's values to see what type 
#    of values you've read in.
  dat$Width
  
# 7. Try changing the class. Did that work? Perhaps you got a warning message about "NAs introduced coercion".
#    Check out the value of the last element in this column of data. Change this value to what it should be.
  as.numeric(dat$Width)
  
  dat$Width[134] = 16
  
# 8. Now with this value fixed. Define this column as class numeric and save it back into your data object.
#    There should be no warnings now and no NA's  
  dat$Width = as.numeric(dat$Width)
  
# 9. Take the mean of this column now.
  mean(dat$Width)
  
# 10. Make the column 'Species' a factor. Make sure all the levels look right. If there is an issue, fix it.
#     Once the issue is fixed, check the levels again. You will need to drop the level that shouldn't be there
#     anymore. Consider code that is like this.... levels(dat$Species) = droplevels(dat$Species)
  dat$Species = factor(dat$Species)
  levels(dat$Species)
  
  dat$Species[116]="perch"
  levels(dat$Species)
  levels(dat$Species) = droplevels(dat$Species)
  
# 11. Now that your data is all nice and perfect. Write it out as a new csv file (with a new name, "cleaned.data"). 
#     Also save it as an R object. E.g., save(object, file="cleaned.dart")
  
  write.csv(dat, "cleaned.data.csv")
  save(dat, file="cleaned.data")  
  