# German Credit: Feature Engineering
# BUSA3020 Advanced Analytics Techniques
# Student Name: Cyrus Kwan
# Student ID: 45200165
# Last Modified: 25/05/2021
# Accessible via: https://github.com/MQCyrusKwan/Assessment-4---Group-Report

uncode <- function(target_df, inter_df, list_attr){
    new_df <- target_df
    for(col_idx in 1:ncol(inter_df)){
        for(row_idx in 1:nrow(inter_df)){
            if(0 %in% inter_df[,col_idx]){
                new_df[row_idx,colnames(inter_df)[col_idx]] <- 
                    list_attr[[col_idx]][inter_df[row_idx, col_idx]+1]
            }else{
                new_df[row_idx,colnames(inter_df)[col_idx]] <- 
                    list_attr[[col_idx]][inter_df[row_idx, col_idx]]
            }
        }
    }
    return(new_df)
}