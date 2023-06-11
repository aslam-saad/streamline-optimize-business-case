Streamlining Business Operations and Optimizing Efficiency: A
Transformational Business Case
================

## Introduction

In today’s competitive business landscape, organizations strive to
enhance operational efficiency and drive growth. This business case aims
to address these objectives by streamlining business operations through
advanced technology adoption and process optimization. We will leverage
a customer store dataset, analyze key metrics and models, and provide
actionable insights and recommendations for the business. By
implementing the proposed solutions, the company can improve customer
experience, increase revenue, and achieve long-term success.

## Business Case

A customer store facing various challenges in managing their business
operations effectively. They were eager to uncover growth opportunities
and optimize their processes. The store owners realized that they needed
a transformative approach to enhance customer satisfaction, increase
revenue, and improve overall business efficiency. Throught the following
chapters, I explore the journey we embarked on to help achieve these
goals.

``` r
# import libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(knitr)
```

``` r
# read the data 
data <- read_xlsx("store-b-data.xlsx")
```

## Chapter 1: Unveiling Discounts

The store owners were curious about the impact of discounts on their
business. They wanted to know the percentage of paid invoices that had
discounts applied and the total amount discounted. After analyzing the
customer store dataset, we discovered that discounts were applied to
4.17% of paid invoices, amounting to \$26,560. This insight highlighted
the significance of discount strategies in attracting customers and
boosting sales.

``` r
data %>%
  #create a boolean variable to indciate discount status
  mutate(has_discount = ifelse(discount_applied > 0, T, F))%>% 
            #% of the invoices with discounts applied
  summarise(has_discount = scales::percent(mean(has_discount, na.rm = T), accuracy=.01), 
            #the total amount of $ discounted
            discount_applied = scales::dollar(sum(discount_applied, na.rm = T)))
```

    ## # A tibble: 1 × 2
    ##   has_discount discount_applied
    ##   <chr>        <chr>           
    ## 1 4.17%        $26,560

## Chapter 2: Converting Monthly Customers to Annual Plans

To secure more revenue upfront, the store owners wanted to explore
opportunities to convert monthly customers into annual subscribers. We
delved into the dataset and found that 107 users subscribed to both
monthly and annual plans in the past year. This revelation indicated the
potential to encourage more monthly customers to upgrade to annual
plans, providing the store with stable and predictable revenue streams.

``` r
data %>%
  group_by(user_id, plan_duration)%>% #group by users and subscription plan
  summarise(value = as.logical(n()))%>% 
  spread(key=plan_duration, value=value) %>%
  filter(Monthly==T, Annual==T)%>% #if the user subscribed monthly and annualy
  ungroup()%>%
  summarise(users = n())
```

    ## # A tibble: 1 × 1
    ##   users
    ##   <int>
    ## 1   107

## Chapter 3: The Rise of Annual Subscriptions

Understanding the revenue composition is crucial for making informed
business decisions. We analyzed the store’s revenue in 2023 and compared
it to the previous year. In 2023, annual subscriptions accounted for
46.22% of the total revenue, a significant increase from 25.76% in 2022.
This exponential growth showcased the importance of promoting and
nurturing annual subscriptions to drive financial stability and business
expansion.

``` r
data %>%
  mutate(year = year(transaction_date))%>% #extract the year from the date
  group_by(year, plan_duration)%>%  #group by year and subscription plan 
  summarise(revenue = sum(amount_paid))%>% #calculate revenue
  #calculate the revenue share per year 
  mutate(revenue_share = scales::percent(revenue/sum(revenue), accuracy=.01))%>%
  filter(plan_duration == "Annual")%>%
  select(year, revenue_share)
```

    ## # A tibble: 2 × 2
    ## # Groups:   year [2]
    ##    year revenue_share
    ##   <dbl> <chr>        
    ## 1  2022 25.76%       
    ## 2  2023 46.22%

## Chapter 4: Unlocking Loyalty with Discounts

The store owners pondered offering discounted annual upgrades to their
most loyal monthly customers. To identify these loyal customers, we
identified 534 customers who had subscribed to the monthly plan for all
of the past 12 months. This loyal customer list provided the store
owners with an opportunity to offer exclusive discounts and rewards,
fostering customer loyalty and encouraging them to upgrade to annual
plans.

``` r
loyal_customers <- data %>%
  filter(plan_duration=="Monthly")%>% #filter monthly subscribers
  group_by(user_id)%>%
  summarise(subscriptions = n())%>%  #count the subscriptions for each user
  filter(subscriptions >= 12) #keep only customers that subscribed at least 12 months 

# save the customer list in a csv file
write.csv(loyal_customers, "Loyal Customers.csv")
```

## Chapter 5: Early Retention of Monthly Customers

Measuring customer retention is crucial for evaluating business
performance and identifying areas for improvement. We examined the
retention rate of new monthly customers in the following month, grouped
by the month of their first paid invoice. We observed consistently high
retention rates, with the exception of a slight dip in October 2022,
where the retention rate dropped to 92%. Overall, the retention rate
remained around 97%, indicating that the store was successful in
retaining its monthly customers.

``` r
retention <- data %>%
  filter(plan_duration=="Monthly")%>% #filter transactions with monthly subscription
  select(user_id, transaction_date)%>%
  group_by(user_id)%>% #group by each user
  arrange(user_id, transaction_date)%>% #arrange by transaction date per users
  filter(row_number() <= 2)%>% #look at the first two transactions
        #calculate the difference between the first and the second transaction (in months)
  mutate(diff = interval(transaction_date, lead(transaction_date)) %/% months(1), 
         #if the user has more then one transaction and the difference between 
         #the second and the first transaction is less than two months 
         #this will counted as retention 
         retention = ifelse(n()>1 & diff<2, T, F))%>%
  filter(!is.na(diff))%>% 
  mutate(month = month(transaction_date),
            year = year(transaction_date),
            date = format(transaction_date, "%b %Y"))%>%
  group_by(year, month, date)%>%
            #number of new customers with retention in the following month
  summarise(retention = sum(retention), 
            #number of new customers
            new_customers = n())%>%
  mutate(retention = scales::percent(retention/new_customers))%>%
  filter(!month %in% 3)%>%
  select(-month, -year) %>%
  select(date, retention, new_customers)

kable(retention)
```

| year | month | date     | retention | new_customers |
|-----:|------:|:---------|:----------|--------------:|
| 2022 |     4 | Apr 2022 | 97%       |          1147 |
| 2022 |     5 | May 2022 | 93%       |           107 |
| 2022 |     6 | Jun 2022 | 95%       |            96 |
| 2022 |     7 | Jul 2022 | 97%       |            75 |
| 2022 |     8 | Aug 2022 | 96%       |            81 |
| 2022 |     9 | Sep 2022 | 97%       |            70 |
| 2022 |    10 | Oct 2022 | 92%       |            65 |
| 2022 |    11 | Nov 2022 | 96%       |           132 |
| 2022 |    12 | Dec 2022 | 99%       |           153 |
| 2023 |     1 | Jan 2023 | 99%       |           130 |
| 2023 |     2 | Feb 2023 | 100%      |            88 |

## Chapter 6: Converting More Customers to Annual Subscriptions

To convert more customers to annual subscriptions, we recommended
focusing on offering attractive discount codes specifically for annual
plans. Our analysis revealed that more than 37% of annual subscribers
used discount codes, compared to only 10.4% of monthly subscribers. By
implementing targeted discount strategies, the store owners could
incentivize customers to choose annual subscriptions, thereby increasing
revenue and customer loyalty.

``` r
data %>%
  #create boolean variable - True means it has discount 
  mutate(has_discount = ifelse(discount_applied > 0, T, F))%>%  
  group_by(plan_duration, user_id)%>% #group by user and plan
  #true means the user used a discount at least once
  summarise(has_discount=as.logical(sum(has_discount)))%>% 
  #calculate the % of users who used discount per plan
  summarise(has_discount = scales::percent(mean(has_discount), accuracy=.01))
```

    ## # A tibble: 2 × 2
    ##   plan_duration has_discount
    ##   <chr>         <chr>       
    ## 1 Annual        37.19%      
    ## 2 Monthly       10.41%

Additionally, we suggested gathering more data on the demographics,
geography, interests, and behaviors of annual subscribers. This data
could be collected through surveys, social listening, and customer
feedback. By understanding the motivations and preferences of this
customer segment, the store owners could tailor their offerings and
marketing campaigns to better meet their needs.

``` r
data%>%
  filter(discount_applied > 0,
         plan_duration == "Annual")%>%
  #the percentage percentage for the total subscription price
  mutate(discount = scales::percent(discount_applied/subscription_price))%>%
  group_by(discount)%>%
  #calculate number of subscribers and revenue per discount %
  summarise(subscriptions=n(), 
            amount_paid = scales::dollar(round(sum(amount_paid), 0))) 
```

    ## # A tibble: 3 × 3
    ##   discount subscriptions amount_paid
    ##   <chr>            <int> <chr>      
    ## 1 20%                 30 $6,158     
    ## 2 25%                148 $28,283    
    ## 3 28%                147 $27,052

## Conclusion

Through this transformative journey, the store owners discovered the
potential for growth and optimization in their business operations. By
leveraging advanced technology, implementing discount strategies, and
analyzing key metrics, they successfully streamlined their operations
and improved customer satisfaction. The store witnessed a significant
increase in revenue from annual subscriptions, enhanced customer
retention, and unlocked opportunities to convert more customers onto
annual plans.

In this ever-evolving business landscape, continuous improvement is key.
By embracing data-driven insights and implementing the recommended
solutions, the store owners can stay ahead of the competition, foster
customer loyalty, and achieve sustainable long-term success.
