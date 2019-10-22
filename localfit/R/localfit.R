# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#给定数据，获取所有的对象列表
library(nlme)
name_list <- function(s_data,item_col){
  name<-s_data[item_col]
  a <- as.data.frame(table(name))
  names <- a$name
  return(names)
}

#给定原始数据和名称，获取对应名称的所有数据
get_name_data <- function(df,name,name_col,time_col){
  sub_data<-subset(df,df[,name_col] == name)
  sub_data_order <- sub_data[order(sub_data[,time_col]),]
  return(sub_data_order)
}

#给定一个数据序列，确定缺失值逻辑列表
bool_list <- function(df,fit_col){
  a <- df[,fit_col]#获取判断空值的列
  bool_na <- is.na(a)#空值列转化为bool逻辑值
  return(bool_na)
}

#由布尔值获取空值列表
nanum <- function(bool_na){
  nl <- list()#用于保存缺失值的行数
  for (vi in 1:length(bool_na)) {
    if (bool_na[vi]==TRUE){
      nl <- c(nl,list(vi))
    }
  }
  return(nl)
}

#由布尔值获取非空值列表
not_na_num <- function(bool_na){
  nl <- list()#用于保存非空值的行数
  for (vi in 1:length(bool_na)) {
    if (bool_na[vi]!=TRUE){
      nl <- c(nl,list(vi))
    }
  }
  return(nl)
}

#给定缺失值逻辑列表，缺失值位置，局部拟合个数，给出局部拟合的数据框
get_fit_df <- function(bool_na,nanum,fitnum,namedata){
  not_na <- length(bool_na)-sum(bool_na)
  fit_list <- list()#保存局部拟合数据的行数
  if (sum(bool_na)==0){#如果没有空值，直接跳出循环
    break
  }
  #判断非空值的数量是否大于局部取值数量
  else{
    if (fitnum>not_na){
      print("空值太多")
      for (i in 1:length(bool_na)){
        if(bool_na[i]==FALSE){
          fit_list <- c(fit_list,list(i))
        }
      }
      print(as.data.frame(fit_list))
    }
    else{
      #print("非空值足够")
      fit_f = nanum - 1#向前取值指针
      fit_b = nanum + 1#向后取值指针
      n <- length(bool_na)#用于判断取前取后
      while (length(fit_list) < fitnum){#当未达到需要的要求的数值时
        if (n%%2==0){
          if (fit_f<1){
              n = n +1
          }
          else{
            if(as.numeric(fit_f)>0 & bool_na[fit_f]!=TRUE){
             # print("向前取值")
              fit_list <- c(fit_list,list(fit_f))
             # print(as.data.frame(fit_list))
              n = n - 1
            }
            else{
              n = n + 1
            }
            fit_f = fit_f-1
          }
        }
        if (length(fit_list) == fitnum){
          break
        }#如果此处已经达到要求数量，提前停止循环
        if (n%%2==1){
          #print("向后取值")
          #print(fit_b)
          if(as.numeric(fit_b)<=length(bool_na) & bool_na[fit_b]==FALSE){
            fit_list <- c(fit_list,list(fit_b))
            #print(as.data.frame(fit_list))
            n = n - 1
          }
          else{
            n = n + 1
          }
          fit_b = fit_b + 1
        }
      }
    }

  }
  fit_list <- c(fit_list,list(nanum))
  fit_data_list<- as.data.frame(fit_list)
  fit_data <- data.frame()
  for (each in fit_data_list) {
    fit_data <- rbind(fit_data,namedata[each,])
  }
  return(fit_data[order(fit_data$Year),])
}

#返回拟合值列表
get_fit_id <- function(bool_na,nanum,fitnum){
  not_na <- length(bool_na)-sum(bool_na)
  fit_list <- list()#保存局部拟合数据的行数
  if (sum(bool_na)==0){#如果没有空值，直接跳出循环
    break
  }
  #判断非空值的数量是否大于局部取值数量
  else{
    if (fitnum>not_na){
      print("空值太多")
      for (i in 1:length(bool_na)){
        if(bool_na[i]==FALSE){
          fit_list <- c(fit_list,list(i))
        }
      }
      print(as.data.frame(fit_list))
    }
    else{
      #print("非空值足够")
      fit_f = nanum - 1#向前取值指针
      fit_b = nanum + 1#向后取值指针
      n <- length(bool_na)#用于判断取前取后
      while (length(fit_list) < fitnum){#当未达到需要的要求的数值时
        if (n%%2==0){
          if (fit_f<1){
            n = n +1
          }
          else{
            if(as.numeric(fit_f)>0 & bool_na[fit_f]!=TRUE){
              # print("向前取值")
              fit_list <- c(fit_list,list(fit_f))
              # print(as.data.frame(fit_list))
              n = n - 1
            }
            else{
              n = n + 1
            }
            fit_f = fit_f-1
          }
        }
        if (length(fit_list) == fitnum){
          break
        }#如果此处已经达到要求数量，提前停止循环
        if (n%%2==1){
          #print("向后取值")
          #print(fit_b)
          if(as.numeric(fit_b)<=length(bool_na) & bool_na[fit_b]==FALSE){
            fit_list <- c(fit_list,list(fit_b))
            #print(as.data.frame(fit_list))
            n = n - 1
          }
          else{
            n = n + 1
          }
          fit_b = fit_b + 1
        }
      }
    }

  }
  return(fit_list)
}

#向前取一个非空的数据
forward_id <- function(bool_na,nanum){
  #向前取一个值
  fit_f = nanum - 1
  fit_list <- list()
  while (length(fit_list) < 1) {
      if (fit_f<1){
          fit_list <- list()
          break
        }
      else{
        if(bool_na[fit_f]!=TRUE){
          # print("向前取值")
          fit_list <- c(fit_f)
          break
          # print(as.data.frame(fit_list))
          }
        }
      fit_f = fit_f-1
    }
  return(fit_list)
}

#向后取一个非空的数据
back_id <- function(bool_na,nanum){
  #向后取一个值
  fit_b = nanum + 1
  fit_list <- list()
  while (length(fit_list) < 1) {
    if (fit_b>length(bool_na)){
            fit_list <- list()
            break
          }
          else{
            if(bool_na[fit_b]!=TRUE){
              # print("向前取值")
              fit_list <- c(fit_list,list(fit_b))
              break
              # print(as.data.frame(fit_list))
            }
          }
          fit_b = fit_b + 1
        }

  return(fit_list)
}

#获取两个数的拟合
dub_fit_data <- function(bool_na,nanum,namedata){
  not_na <- length(bool_na)-sum(bool_na)
  fit_list <- list()#保存局部拟合数据的行数
  if (not_na==0){
    print("全为空值")
    fit_data <- data.frame(matrix(numeric(0),ncol=4))
  }
  else{
    if (not_na==1){
      not_na_id = 1
      for (each in 1:length(bool_na)) {
        if (bool_na[each] != TRUE) {
          fit_data <- namedata[each,]
        }
      }
    }
      else{
        #向前取一个值
        fw_id = forward_id(bool_na,nanum)
        #向后取一个值
        bk_id = back_id(bool_na,nanum)
        fit_list <- c(fw_id,bk_id)
        if (length(fit_list)==1){
          fit_list <- get_fit_id(bool_na=bool_na,nanum=nanum,fitnum=2)
        }
        fit_list <- c(fit_list,list(nanum))
        fit_data_list<- as.data.frame(fit_list)
        fit_data <- data.frame()
        for (each in fit_data_list) {
          fit_data <- rbind(fit_data,namedata[each,])
      }
    }
  }
  return(fit_data[order(fit_data[,2]),])
}

#获取拟合后的一条数据
dub_fit <- function(fit_data,all_data,nanum,fit_col,time_col){
  fit_time = all_data[,time_col][nanum]
  new_data = all_data[nanum,]
  if (nrow(fit_data)==0){
    last_result <- NA
  }
  else{
    if (nrow(fit_data)==1){
      #new_data = all_data[nanum,]
      last_result <- fit_data[1,fit_col]
    }
    else{
  year_max <- max(all_data[,time_col])
  year_min <- min(all_data[,time_col])
  x <- fit_data[,time_col]
  y <- fit_data[,fit_col]
  test_lm <- lm(y~x)
  lm_fit_time <- data.frame(x=fit_time)
  lm_result1 <- predict(test_lm,lm_fit_time)
  if (lm_result1 > 0){
    last_result <- lm_result1
  }
  else{#判断结果是否小于0，如果是
    fit_time2 = fit_time + 1
    lm_fit_time <- data.frame(x = fit_time2)
    lm_result2 <- predict(test_lm,lm_fit_time)
    if (lm_result1<lm_result2){#判断结果递增还是递减，如果递增
      lm_result = lm_result1
      repeat{
        fit_time = fit_time+1
        lm_fit_time <- data.frame(x=fit_time)
        lm_result <- predict(test_lm,lm_fit_time)
        last_result <- lm_result
        if(lm_result>0){
          break
        }
      }
    }
    else{
      if (lm_result1==lm_result2){
        last_result <- lm_result1
      }
      else{
      lm_result = lm_result1
      repeat{
        fit_time = fit_time-1
        lm_fit_time <- data.frame(x=fit_time)
        lm_result <- predict(test_lm,lm_fit_time)
        last_result <- lm_result

        if(lm_result>0){
          break
        }
      }
      }
    }
  }
    }
  }
  new_data[,fit_col] <- last_result
  return(new_data)
}

#全体数据拟合
dub_all_fit<- function(source_data,name_col,time_col,fit_col){
  result_data <- data.frame()
  namelist <- name_list(s_data= source_data,item_col=name_col)
  for (each in namelist) {
    each_data <- data.frame()
    print(each)
    name_data_all <- get_name_data(df=source_data,name = each,name_col=name_col,time_col=time_col)
    boollist <- bool_list(df=name_data_all,fit_col=fit_col)
    #print(boollist)
    na_list<-nanum(bool_na=boollist)
    not_na_list <- not_na_num(bool_na=boollist)
    if (length(na_list)==0){
      result_data <- rbind(result_data,name_data_all)
    }
    if (length(not_na_list)>0){
    for (not_na in not_na_list) {
      not_na_data <- name_data_all[not_na,]
      each_data <- rbind(not_na_data,each_data)
      }
    }
    if (length(na_list)>0){
    for ( na in na_list) {
      #print(num)
      fit_data <- dub_fit_data(bool_na = boollist,nanum = na,namedata = name_data_all)
      a <- fit_data
      new_one_data <- dub_fit(fit_data = fit_data,all_data = name_data_all,nanum = na,fit_col = fit_col,time_col = time_col)
      each_data <- rbind(new_one_data,each_data)
    }
    result_data <- rbind(result_data,each_data)
    }
  }
  return(result_data)
}





