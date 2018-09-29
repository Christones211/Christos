--------------- Find Dates		
		select 		distinct
			datepart(ISO_WEEK,b.Inv_DateTime) as week,
			convert(date,b.Inv_DateTime) as date
			from Invoice b

--------------- Find Customers 

	SELECT  
			e.Cust_Point_UniqueId,e.Cust_Description,e.Cust_RPS_Name,l.Loc_Ael_1,
			datepart(ISO_WEEK,b.Inv_DateTime) as week,
			 datepart(Year,b.Inv_DateTime) ,

			sum(IIF(d.InvPur_Direction = b.inv_direction,a.InvRow_ItemQty,a.InvRow_ItemQty *(-1.0))) as Allqty, 
			sum(IIF(d.InvPur_Direction = b.inv_direction,a.InvRow_TotalPrice,a.InvRow_TotalPrice *(-1.0))) as Allrevenues
	FROM [DwStage].[dbo].[InvoiceRow] a
		inner join Invoice b on b.Inv_ID = a.InvRow_InvID and b.Inv_CustSourceid= a.InvRow_CustSourceid
		inner join Customer e on e.Cust_SourceMasterID=b.Inv_CustSourceid
		inner join InvoiceType c on c.InvTyp_ID= b.Inv_TypeID and c.InvTyp_CustSourceid =b.Inv_CustSourceid
		inner join InvoicePurpose d on d.InvPur_ID= b.Inv_InvPurID and d.InvPur_CustSourceId = b.Inv_CustSourceid	
		inner join  Location l on e.Cust_LocKey=l.Loc_Key
	 and d.InvPur_Code in (N'п',N'пи') 
	 and c.InvTyp_Code not in (N'пис-тил-ац',N'ап-пкг',N'ай-ап-пкг',N'EASYPAY',N'ай-EASYPAY',N'VIVA-еис-уп') 
     and e.Cust_RP_Scope ='STORE' and e.Cust_Description not like '%IQOS%'  and e.Cust_RP_PointAlias not like '%RL%' and e.Cust_RP_PointAlias not like '%ACT%' 
	 and e.Cust_ActivationDate <'2016-08-01' and e.Cust_ExpirationDate > GETDATE() 
	 group by   e.Cust_Point_UniqueId,e.Cust_Point_UniqueId,e.Cust_Description,e.Cust_RPS_Name,l.Loc_Ael_1,
	       datepart(Year,b.Inv_DateTime) ,
			datepart(ISO_WEEK,b.Inv_DateTime) 


--------------- Find top 20 Alcohol Brands 

	SELECT  top 20

		   BrandU.BrandID,
		   BrandU.BrandName,
			Manu.Man_Name,
				pu.Prod_CatID,
			sum(IIF(d.InvPur_Direction = b.inv_direction,a.InvRow_ItemQty,a.InvRow_ItemQty *(-1.0))) as Allqty, 
			sum(IIF(d.InvPur_Direction = b.inv_direction,a.InvRow_TotalPrice,a.InvRow_TotalPrice *(-1.0))) as Allrevenues
	FROM [DwStage].[dbo].[InvoiceRow] a
		inner join Invoice b on b.Inv_ID = a.InvRow_InvID and b.Inv_CustSourceid= a.InvRow_CustSourceid
		inner join Customer e on e.Cust_SourceMasterID=b.Inv_CustSourceid
		inner join InvoiceType c on c.InvTyp_ID= b.Inv_TypeID and c.InvTyp_CustSourceid =b.Inv_CustSourceid
		inner join InvoicePurpose d on d.InvPur_ID= b.Inv_InvPurID and d.InvPur_CustSourceId = b.Inv_CustSourceid	
		inner join  Location l on e.Cust_LocKey=l.Loc_Key
		inner join productUpd p on p.Prod_ID = a.InvRow_ProdId and p.Prod_CustSourceId = a.InvRow_CustSourceId
	    inner join ProductU pu on pu.Prod_Id = p.prod_prodUID
		inner join manufacturersu manu on manu.Man_ID=pu.ProdManID
	    inner join BrandU brANDU on brANDU.BrandID=pu.ProdBrandID
	 and d.InvPur_Code in (N'п',N'пи') 
	 and c.InvTyp_Code not in (N'пис-тил-ац',N'ап-пкг',N'ай-ап-пкг',N'EASYPAY',N'ай-EASYPAY',N'VIVA-еис-уп') 
     and e.Cust_RP_Scope ='STORE' and e.Cust_Description not like '%IQOS%'  and e.Cust_RP_PointAlias not like '%RL%' and e.Cust_RP_PointAlias not like '%ACT%' 
	 and e.Cust_ActivationDate <'2016-08-01' and e.Cust_ExpirationDate > GETDATE() and pu.Prod_CatID in (40)
	 group by  
		   BrandU.BrandID,
		   BrandU.BrandName,
			Manu.Man_Name,
			pu.Prod_CatID
			order by Allrevenues desc


select 			e.Cust_Point_UniqueId,e.Cust_Description,e.Cust_RPS_Name,l.Loc_Ael_1,
			datepart(ISO_WEEK,b.Inv_DateTime) as week,
			 datepart(Year,b.Inv_DateTime) as year,

		--   pu.Prod_Name,
		   BrandU.BrandID,
		   BrandU.BrandName,
			Manu.Man_Name,
				pu.Prod_CatID,
		--		pu.Prod_ID,
			sum(IIF(d.InvPur_Direction = b.inv_direction,a.InvRow_ItemQty,a.InvRow_ItemQty *(-1.0))) as Allqty, 
			sum(IIF(d.InvPur_Direction = b.inv_direction,a.InvRow_TotalPrice,a.InvRow_TotalPrice *(-1.0))) as Allrevenues into Chrisdb.dbo.BeersData
	FROM [DwStage].[dbo].[InvoiceRow] a
		inner join Invoice b on b.Inv_ID = a.InvRow_InvID and b.Inv_CustSourceid= a.InvRow_CustSourceid
		inner join Customer e on e.Cust_SourceMasterID=b.Inv_CustSourceid
		inner join InvoiceType c on c.InvTyp_ID= b.Inv_TypeID and c.InvTyp_CustSourceid =b.Inv_CustSourceid
		inner join InvoicePurpose d on d.InvPur_ID= b.Inv_InvPurID and d.InvPur_CustSourceId = b.Inv_CustSourceid	
		inner join  Location l on e.Cust_LocKey=l.Loc_Key
		inner join productUpd p on p.Prod_ID = a.InvRow_ProdId and p.Prod_CustSourceId = a.InvRow_CustSourceId
	    inner join ProductU pu on pu.Prod_Id = p.prod_prodUID
		inner join manufacturersu manu on manu.Man_ID=pu.ProdManID
		inner join BrandU brANDU on brANDU.BrandID=pu.ProdBrandID
	 and d.InvPur_Code in (N'п',N'пи') 
	 and c.InvTyp_Code not in (N'пис-тил-ац',N'ап-пкг',N'ай-ап-пкг',N'EASYPAY',N'ай-EASYPAY',N'VIVA-еис-уп') 
     and e.Cust_RP_Scope ='STORE' and e.Cust_Description not like '%IQOS%'  and e.Cust_RP_PointAlias not like '%RL%' and e.Cust_RP_PointAlias not like '%ACT%' 
	 and e.Cust_ActivationDate <'2016-08-01' and e.Cust_ExpirationDate > GETDATE() and pu.Prod_CatID in (40)
	 group by  	e.Cust_Point_UniqueId,e.Cust_Description,e.Cust_RPS_Name,l.Loc_Ael_1,
			datepart(ISO_WEEK,b.Inv_DateTime) ,
			 datepart(Year,b.Inv_DateTime) ,
		    	  Manu.Man_Name,
		   BrandU.BrandID,
		   BrandU.BrandName,
	        --	pu.Prod_Name,
				pu.Prod_CatID
				--pu.Prod_ID

select * from customer
select * from CategoryU 

select * from Chrisdb.dbo.BeersData

Create Table WeekYear
(
    Year  int not null ,
    Week datetime not null,
);



select * from productU where Prod_CatID in (40) 