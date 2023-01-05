
# find a 2021 date with chroma customer and convert a data into fiscial year 
select * from fact_sales_monthly where customer_code = 90002002 and get_fiscal_quater(date) = "q1" 
order by date asc;

select * from fact_sales_monthly where customer_code = 90002002 and get_fiscal_year(date) = 2020 
order by date asc;

select 
	fs.date,fs.product_code,
    dp.product,dp.variant,
    fs.sold_quantity,fg.gross_price,
    round(fg.gross_price*fs.sold_quantity,2) as gross_price_total
from fact_sales_monthly  fs
join dim_product dp
using (product_code)
join fact_gross_price fg
on 
	fg.product_code = fs.product_code and 
    fg.fiscal_year = get_fiscal_year(fs.date)
where 
	customer_code = 90002002 and 
    get_fiscal_year(date) = 2021
order by date asc
limit 1000000;

/*
Generate a yearly report for Croma India where there are two columns

1. Fiscal Year
2. Total Gross Sales amount In that year from Croma

select sm.date, sum(gp.gross_price*sm.sold_quantity) as gross_price_total
from fact_sales_monthly sm
join fact_gross_price gp
on 
	gp.product_code = sm.product_code and 
    gp.fiscal_year = get_fiscal_year(sm.date)
where customer_code = 90002002
group by sm.date
order by sm.date ASC;

select
        get_fiscal_year(date) as fiscal_year,
        sum(round(sold_quantity*g.gross_price,2)) as yearly_sales
	from fact_sales_monthly s
	join fact_gross_price g
	on 
		g.fiscal_year=get_fiscal_year(s.date) and
		g.product_code=s.product_code
	whereget_monthly_gross_sales_for_customer
		customer_code=90002002
	group by fiscal_year
	order by fiscal_year;
    */
    
    
    -- india, 2021 --> gold
    -- srilanka, 2020 --> silver
    
    select c.market, sum(s.sold_quantity) as total_qty from fact_sales_monthly  s
    join dim_customer c
    using(customer_code)
    where get_fiscal_year(s.date) = 2021 and c.market = "india"
    group by c.market;
    
    
# top customer 

select 
	fs.date,fs.product_code,
    dp.product,dp.variant,
    fs.sold_quantity,fg.gross_price,
    round(fg.gross_price*fs.sold_quantity,2) as gross_price_total,
    pre.pre_invoice_discount_pct,
    round(fg.gross_price - pre.pre_invoice_discount_pct,2) as net_invoice_sales,
    round(post.discounts_pct + post.other_deductions_pct) as post_invoice_discount_pct
from fact_sales_monthly  fs
join dim_product dp
	using (product_code)
join fact_gross_price fg
	on fg.product_code = fs.product_code and 
    fg.fiscal_year = get_fiscal_year(fs.date)
join fact_pre_invoice_deductions pre
	on	pre.customer_code = fs.customer_code and
	pre.fiscal_year = get_fiscal_year(fs.date)
join fact_post_invoice_deductions post
	on post.customer_code = fs.customer_code and
    post.date = get_fiscal_year(fs.date)
where 
	fs.customer_code = 90002002 and 
    get_fiscal_year(fs.date) = 2021
order by date asc;

with cte1 as (select 
	fs.date,fs.product_code,
    dp.product,dp.variant,
    fs.sold_quantity,fg.gross_price,
    round(fg.gross_price*fs.sold_quantity,2) as gross_price_total,
    pre.pre_invoice_discount_pct
from fact_sales_monthly  fs
join dim_product dp
	using (product_code) 
join fact_gross_price fg
	on fg.product_code = fs.product_code and 
    fg.fiscal_year = fs.fiscal_year
join fact_pre_invoice_deductions pre
	on	pre.customer_code = fs.customer_code and
	pre.fiscal_year = fs.fiscal_year 
where 
    fs.fiscal_year  = 2021
limit 1000000)

select 
	*,(1 - pre_invoice_discount_pct)*gross_price_total as net_invoice_sales,
    (post.discounts_pct + post.other_deductions_pct) as post_invoice_discount_pct
from sales_preinv_discount s
join fact_post_invoice_deductions post
	on post.customer_code = s.customer_code and
	post.product_code = s.product_code and
    post.date = s.date;
    
select
		s.date,
		s.fiscal_year,
		s.customer_code,
		c.customer,
		c.market,
		s.product_code,
		p.product, p.variant,
		s.sold_quantity,
		g.gross_price as gross_price_per_item,
		round(s.sold_quantity*g.gross_price,2) as gross_price_total
	from fact_sales_monthly s
	join dim_product p
	on s.product_code=p.product_code
	join dim_customer c
	on s.customer_code=c.customer_code
	join fact_gross_price g
	on g.fiscal_year=s.fiscal_year
	and g.product_code=s.product_code;
    
select market, round(sum(net_sales)/1000000,2) as net_sales_mln
from net_sales 
where fiscal_year = 2021
group by market
order by net_sales desc
limit 5;

select c.customer, round(sum(net_sales)/1000000,2)  as net_sales_mln
from net_sales n
join dim_customer c
using(customer_code)
where fiscal_year = 2021
group by c.customer
order by net_sales_mln desc
limit 5;

# over
select *, 
	amount*100/sum(amount) over(partition by category) as pct
from random_tables.expenses 
order by category;

select *, 
	sum(amount) over(partition by category order by date) as total_expense_till_date
from random_tables.expenses 
order by category;

# net sales global market share %

with che1 as (select 
	c.customer, 
    round(sum(net_sales)/1000000,2)  as net_sales_mln
from net_sales n
join dim_customer c
	using(customer_code)
where fiscal_year = 2021
group by c.customer)

select 
	*,
	concat( round(net_sales_mln*100/sum(net_sales_mln) over(),2 ),"  %") as total_net_sales_percentage
from che1;
 
 # net sales global market share % per region
 
 with che1 as (select 
	c.customer,
    c.region,
    round(sum(net_sales)/1000000,2)  as net_sales_mln
from net_sales n
join dim_customer c
	using(customer_code)
where fiscal_year = 2021
group by c.customer, c.region)

select 
	*,
	 round(net_sales_mln*100/sum(net_sales_mln) over(partition by region),2 ) as total_net_sales_percentage
from che1
order by region, total_net_sales_percentage desc;

# window function row_number, rank , dense rank

with cte1 as (select *,
	row_number() over(partition by category order by amount desc) as rn,
	rank() over(partition by category order by amount desc) as rnk,
	dense_rank() over(partition by category order by amount desc) as drnk
from random_tables.expenses
order by category)

select * from cte1 where drnk<=2;

## top n product in each division by their quantity sold

with cte1 as (
		select
			c.market,
			c.region,
			round(sum(gross_price_total)/1000000,2) as gross_sales_mln
			from gross_sales s
			join dim_customer c
			on c.customer_code=s.customer_code
			where fiscal_year=2021
			group by market
			order by gross_sales_mln desc
		),
		cte2 as (
			select *,
			dense_rank() over(partition by region order by gross_sales_mln desc) as drnk
			from cte1
		)
	select * from cte2 where drnk <= 2;
		
# make new table fact_act_est with forcase and sales table

	create table fact_act_est
	(
        	select 
                    s.date as date,
                    s.fiscal_year as fiscal_year,
                    s.product_code as product_code,
                    s.customer_code as customer_code,
                    s.sold_quantity as sold_quantity,
                    f.forecast_quantity as forecast_quantity
        	from 
                    fact_sales_monthly s
        	left join fact_forecast_monthly f 
        	using (date, customer_code, product_code)
	)
	union
	(
        	select 
                    f.date as date,
                    f.fiscal_year as fiscal_year,
                    f.product_code as product_code,
                    f.customer_code as customer_code,
                    s.sold_quantity as sold_quantity,
                    f.forecast_quantity as forecast_quantity
        	from 
		    fact_forecast_monthly  f
        	left join fact_sales_monthly s 
        	using (date, customer_code, product_code)
	);

update fact_act_est set sold_quantity = 0 where sold_quantity is null;

update fact_act_est
	set forecast_quantity = 0
	where forecast_quantity is null;


# find forcasr accuracy group by customer (cupply chain report)
with forecast_err_table as (
             select
                  s.customer_code as customer_code,
                  c.customer as customer_name,
                  c.market as market,
                  sum(s.sold_quantity) as total_sold_qty,
                  sum(s.forecast_quantity) as total_forecast_qty,
                  sum(s.forecast_quantity-s.sold_quantity) as net_error,
                  round(sum(s.forecast_quantity-s.sold_quantity)*100/sum(s.forecast_quantity),1) as net_error_pct,
                  sum(abs(s.forecast_quantity-s.sold_quantity)) as abs_error,
                  round(sum(abs(s.forecast_quantity-sold_quantity))*100/sum(s.forecast_quantity),2) as abs_error_pct
             from fact_act_est s
             join dim_customer c
             on s.customer_code = c.customer_code
             where s.fiscal_year=2021
             group by customer_code
	)
	select 
            *,
            if (abs_error_pct > 100, 0, 100.0 - abs_error_pct) as forecast_accuracy
	from forecast_err_table
        order by forecast_accuracy desc;
        
        
# step 1: Get forecast accuracy of FY 2021 and store that in a temporary table
drop table if exists forecast_accuracy_2021;
create temporary table forecast_accuracy_2021
with forecast_err_table as (
        select
                s.customer_code as customer_code,
                c.customer as customer_name,
                c.market as market,
                sum(s.sold_quantity) as total_sold_qty,
                sum(s.forecast_quantity) as total_forecast_qty,
                sum(s.forecast_quantity-s.sold_quantity) as net_error,
                round(sum(s.forecast_quantity-s.sold_quantity)*100/sum(s.forecast_quantity),1) as net_error_pct,
                sum(abs(s.forecast_quantity-s.sold_quantity)) as abs_error,
                round(sum(abs(s.forecast_quantity-sold_quantity))*100/sum(s.forecast_quantity),2) as abs_error_pct
        from fact_act_est s
        join dim_customer c
        on s.customer_code = c.customer_code
        where s.fiscal_year=2021
        group by customer_code
)
select 
        *,
    if (abs_error_pct > 100, 0, 100.0 - abs_error_pct) as forecast_accuracy
from 
	forecast_err_table
order by forecast_accuracy desc;

# step 2: Get forecast accuracy of FY 2020 and store that also in a temporary table
drop table if exists forecast_accuracy_2020;
create temporary table forecast_accuracy_2020
with forecast_err_table as (
        select
                s.customer_code as customer_code,
                c.customer as customer_name,
                c.market as market,
                sum(s.sold_quantity) as total_sold_qty,
                sum(s.forecast_quantity) as total_forecast_qty,
                sum(s.forecast_quantity-s.sold_quantity) as net_error,
                round(sum(s.forecast_quantity-s.sold_quantity)*100/sum(s.forecast_quantity),1) as net_error_pct,
                sum(abs(s.forecast_quantity-s.sold_quantity)) as abs_error,
                round(sum(abs(s.forecast_quantity-sold_quantity))*100/sum(s.forecast_quantity),2) as abs_error_pct
        from fact_act_est s
        join dim_customer c
        on s.customer_code = c.customer_code
        where s.fiscal_year=2020
        group by customer_code
)
select 
        *,
    if (abs_error_pct > 100, 0, 100.0 - abs_error_pct) as forecast_accuracy
from 
	forecast_err_table
order by forecast_accuracy desc;

-- step 3: Join forecast accuracy tables for 2020 and 2021 using a customer_code
select 
	f_2020.customer_code,
	f_2020.customer_name,
	f_2020.market,
	f_2020.forecast_accuracy as forecast_acc_2020,
	f_2021.forecast_accuracy as forecast_acc_2021
from forecast_accuracy_2020 f_2020
join forecast_accuracy_2021 f_2021
on f_2020.customer_code = f_2021.customer_code 
where f_2021.forecast_accuracy < f_2020.forecast_accuracy
order by forecast_acc_2020 desc;

select count(*) from dim_customer
join fact_pre_invoice_deductions using (customer_code)

select count(*) from fact_pre_invoice_deductions