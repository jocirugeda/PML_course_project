

#  Function to list chr columns of a data frame


traza_chr<-function(df){
        size_col<-length(df)
        
        for (i in 1:length(df))
        {
                tipo<-class(df[,i]);
                nombre<-names(df)[i];
                if (tipo=="character"){
                        print(paste(" nombre->",nombre));
                }
        }
        
}

factor_build<-function(df){
        res_df<-df
        # user_name
        l_names<-names(df)
        if (any("user_name" == l_names)){
        res_df$user_name<-factor(res_df$user_name)
        }
        # cvtd_timestamp
        if (any("cvtd_timestamp" == l_names)){
        res_df$cvtd_timestamp<-factor(res_df$cvtd_timestamp)
        }
        # new_window
        if (any("new_window" == l_names)){
        res_df$new_window<-factor(res_df$new_window)
        }
        # classe
        if (any("classe" == l_names)){
        res_df$classe<-factor(res_df$classe)        
        }
        res_df
}


force_to_numeric<-function(df){
        
        res_df<-df
        for (i in 1:length(res_df))
        {
                tipo<-class(res_df[,i]);
                
                if (tipo!="factor"){
                        res_df[,i]=as.numeric(res_df[,i])                
                }
        }  
        
        res_df
        
}


# convert is.na to 1/0 for aritmetic
flag_na<-function(x){
        if(is.na(x)){
                1
        }else{
                0
        }
}

# function to get proportion of NA for one column in the dataset
p_na<-function(df,index){
        size<-nrow(df)
        flag_vector<-sapply(df[,index],flag_na)
        num_na<-sum(flag_vector)        
        
        res<-num_na/size
        res
}

# DROP COLUMNS with upper ratio of NA than 1/2 values 
drop_BY_P_NA<-function(df_full){
        res_df<-df_full
        for (i in length(df_full):1)
        {
                p_value<-p_na(res_df,i)
                if (p_value>0.5){
                
                        res_df<-res_df[,-1*i]
                }
        }        
        res_df
}


process_initial<-function(df){        
        res_df<-factor_build(df)
        res_df<-force_chr_to_numeric(res_df)        
}


pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

