## STart of Q1############

getwd()
setwd("C:/MICA/Term 4/AMMA 2017/data_2017")
emp<- read.csv("Emp.csv")
dept<-read.csv("Dept.csv",fill = T,header = T,sep=",")

# Merge Employee and Dept by DeptNo #
employee_dept_merged <- merge(emp,dept,by="DEPTNO",all = TRUE)
View(employee_dept_merged)
## End of Q1##########

## Start of Q2###############

# Average salary by location #
No.of.Employees_Loc <- c(rep(0,nrow(dept)))
salaryLoc <- c(rep(0,nrow(dept)))

for (i1 in 1:nrow(dept))
{
  for (i2 in 1:nrow(employee_dept_merged))
  {
    if (!is.na(employee_dept_merged$LOC[i2]))
    {  
      if (employee_dept_merged$LOC[i2]==dept$LOC[i1])
      {
        No.of.Employees_Loc[i1] <- No.of.Employees_Loc[i1] +1
        salaryLoc[i1] <- salaryLoc[i1] + employee_dept_merged$SAL[i2]
      }
    }    
  }  
}    

mean_salary <- c(rep(0,nrow(dept)))
for (i3 in 1:nrow(dept))
{
  mean_salary[i3] <- salaryLoc[i3]/No.of.Employees_Loc[i3]
}

## End of Q2##############

## STart of Q3#############

Employee_IDS <- unique(employee_dept_merged$ID)
countofemp <- rep(0,length(Employee_IDS))

for (i3 in 1:length(Employee_IDS))
{
  if (!(is.na(Employee_IDS[i3])))
  {
    for (i4 in 1:nrow(employee_dept_merged))
    {
      if (!(is.na(employee_dept_merged$MGR[i4])))
      {
        if (employee_dept_merged$MGR[i4]==Employee_IDS[i3])
        {
          countofemp[i3] <- countofemp[i3] + 1}}}}}

for (i5 in 1:length(Employee_IDS))
{
  if (countofemp[i5]==max(countofemp))
  {
    position <- i5
  }
} 

print("Manager with the highest number of employees")
print(employee_dept_merged$ENAME[position])

## End of Q3#############

## Start of Q4###########

emp<- read.csv("Emp.csv")
dept<-read.csv("Dept.csv",fill = T,header = T,sep=",")
employee_dept_merged <- merge(emp,dept,by="DEPTNO",all = TRUE)
View(employee_dept_merged)
hiked <- employee_dept_merged

for(i in 1:nrow(employee_dept_merged))
{
  if(!is.na(employee_dept_merged$JOB[i]))
  {
    if(employee_dept_merged$JOB[i]=="ANALYST")
    {
      hiked$SAL[i] = employee_dept_merged$SAL[i] + (0.15 *employee_dept_merged$SAL[i])
    }
  }
}

for(i in 1:nrow(employee_dept_merged))
{
  if(!is.na(employee_dept_merged$JOB[i]))
  {
    if(employee_dept_merged$JOB[i]=="CLERK")
    {
      hiked$SAL[i] = employee_dept_merged$SAL[i] + (0.125 *employee_dept_merged$SAL[i])
    }
  }
}

for(i in 1:nrow(employee_dept_merged))
{
  if(!is.na(employee_dept_merged$JOB[i]))
  {
    if(employee_dept_merged$JOB[i]=="MANAGER")
    {
      hiked$SAL[i] = employee_dept_merged$SAL[i] + (0.102 * employee_dept_merged$SAL[i])
    }
  }
}

for(i in 1:nrow(employee_dept_merged))
{
  if(!is.na(employee_dept_merged$JOB[i]))
  {
    if(employee_dept_merged$JOB[i]=="PRESIDENT")
    {
      hiked$SAL[i] = employee_dept_merged$SAL[i] + (0.057 * employee_dept_merged$SAL[i])
    }
  }
}

for(i in 1:nrow(employee_dept_merged))
{
  if(!is.na(employee_dept_merged$JOB[i]))
  {
    if(employee_dept_merged$JOB[i]=="SALESMAN")
    {
     hiked$SAL[i] = employee_dept_merged$SAL[i] + (0.133 * employee_dept_merged$SAL[i])
    }
  }
}
View(hiked)
