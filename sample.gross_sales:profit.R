


#sets data frame 
   actual_data<-data.frame(esceldamp)
    #delete top row (not needed bc colname replaces labels)
    collabel<-c("segment","country", "product", "discount_band", "units_sold", "manufactoring", "sale_price", "gross_sales","discount","sales","cogs", "profit","date","month_num","month_name","year")
    colnames(actual_data)<-collabel
    #delete leftmost column

country<- actual_data$country  
  
#sets graph  
    library(ggplot2)
      ggplot(data = actual_data) +
        geom_smooth(mapping = aes(x= gross_sales, y=profit))
      
    