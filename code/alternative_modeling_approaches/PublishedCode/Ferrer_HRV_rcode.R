#This is the code to run the oscillator model in R

##YOU NEED TO INSTALL THE NMLE PACKAGE.  You sould be able to do this from the packages and Data tab above.  Click package installer, find nmle, check the box next to it, and click install.


#First, put this code and the data on the desktop, and then change the working directory of R to the desktop.  You should be able to change the working directory in R by clicking the 'change working directory' under the 'Misc' Tab.  If you are using a PC it may be different since I am using the Mac version. 


########### Getting data into R #############

#This load the nlme package
library(nlme)

#Next you need to bring the data into R.  The next two commands will do this, and name the data with the appropriate columns.


data = read.csv('HRV_derivs.txt', header = FALSE)

names(data) = c('pid', 'task', 'x_m', 'dx_m', 'd2x_m', 'x_f', 'dx_f', 'd2x_f')




####### Obtaining the values currently in the tables ########

#each model will correspond to a different part of the table.  HRV_m_1 will represent the male model for task one.  HRV_m_2 the male model for task 2, and so on.

HRV_m_1 = lm(d2x_m ~x_m+dx_m+x_f+dx_f-1, data = data, task == 1)
HRV_m_2 = lm(d2x_m ~x_m+dx_m+x_f+dx_f-1, data = data, task == 2)
HRV_m_3 = lm(d2x_m ~x_m+dx_m+x_f+dx_f-1, data = data, task == 3)

HRV_f_1 = lm(d2x_f ~x_f+dx_f+x_m+dx_m-1, data = data, task == 1)
HRV_f_2 = lm(d2x_f ~x_f+dx_f+x_m+dx_m-1, data = data, task == 2)
HRV_f_3 = lm(d2x_f ~x_f+dx_f+x_m+dx_m-1, data = data, task == 3)

#To view the output of these models, use the summary() command.  For example, if you want to see the fits for males on task one, type in summary(HRV_m_1)



####### Fitting Data with Covariance Estimates ###########


# we need to multiply by a constant so that the model has enough covariation to get the random estimates.  We will call the new data set inc_data (increased data)

inc_data = data

inc_data[,3] = data$x_m*10000
inc_data[,4] = data$dx_m*10000
inc_data[,5] = data$d2x_m*10000
inc_data[,6] = data$x_f*10000
inc_data[,7] = data$dx_f*10000
inc_data[,8] = data$d2x_f*10000

#With this data set we can run the models and estimate the random components.  For these models, I will name each one HRV_rand_m_1, for them model for males for task one


HRV_rand_m_1 = lme(d2x_m ~ x_m+dx_m+x_f+dx_f-1, data = inc_data, ~-1+x_m+dx_m+x_f+dx_f|pid,correlation = NULL, weights = NULL, task == 1, method = c('REML'))

HRV_rand_m_2 = lme(d2x_m ~ x_m+dx_m+x_f+dx_f-1, data = inc_data, ~-1+x_m+dx_m+x_f+dx_f|pid,correlation = NULL, weights = NULL, task == 2, method = c('ML'))

HRV_rand_m_3 = lme(d2x_m ~ x_m+dx_m+x_f+dx_f-1, data = inc_data, ~-1+x_m+dx_m+x_f+dx_f|pid,correlation = NULL, weights = NULL, task == 3, method = c('ML'))

HRV_rand_f_1 = lme(d2x_f ~ x_f+dx_f+x_m+dx_m-1, data = inc_data, ~-1+x_f+dx_f+x_m+dx_m|pid,correlation = NULL, weights = NULL, task == 1, method = c('ML'))

HRV_rand_f_2 = lme(d2x_f ~ x_f+dx_f+x_m+dx_m-1, data = inc_data, ~-1+x_f+dx_f+x_m+dx_m|pid,correlation = NULL, weights = NULL, task == 2, method = c('ML'))

HRV_rand_f_3 = lme(d2x_f ~ x_f+dx_f+x_m+dx_m-1, data = inc_data, ~-1+x_f+dx_f+x_m+dx_m|pid,correlation = NULL, weights = NULL, task == 3, method = c('ML'))


#Once again, to get the fit estimates for each of these models simply use the summary() function.  For example, summary(HRV_rand_m_1) will give the fits for males HRV in task 1.
