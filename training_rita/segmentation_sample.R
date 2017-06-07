
#Nielsen Raw data
#Raw {itemcode,store,date,sales_unit,sales_value}

#calculate the sales market size for each item
test <- raw %>% group_by(itemcode) %>% summarise(vol = sum(sales_unit), val = sum(sales_value))
#calculate the average price for each item
mutate(test, price = val/vol)

#calculate acv for each store
raw %>% group_by(store) %>% summarise(vol = sum(sales_unit), val = sum(sales_value))

