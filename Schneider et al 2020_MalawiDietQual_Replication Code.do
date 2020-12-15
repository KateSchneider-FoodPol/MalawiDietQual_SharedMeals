/*
Kate Schneider
Replication code for:
	Schneider et al (2020). "HAssessing nutrient adequacy and diet quality 
	based on individual requirements and nutrient density where households 
	share their food."
		Journal of Nutrition.
Tables and figures Replication Code
Last modified: 16 Nov 2020
Contact: kate.schneider@tufts.edu
Created in Stata version 15.1
*/

// Working directory
global myfiles "C:\Users\kates\Box\PhD\5. Dissertation\06_Manuscripts\3. Thesis\Paper 1 - for publication\Replication Test"
cd "$myfiles"

// Methods //
	* Percent of women estimated to be breastfeeding
	* Sample statistic
	use MalawiIHPS_DietQual_PID, clear
	bys data_round: sum lactating if sex==1 & (age_rptd>=14 & age_rptd<=59)
	sum lactating if sex==1 & (age_rptd>=14 & age_rptd<=59)

	* Foods converted to kg units and matched to FCT 
	use MalawiIHPS_DietQual_FoodHHExp, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	tempvar included
		gen `included'=1 if qty_kg_total!=. & qty_kg_total!=0 & foodexcluded==0
		replace `included'=0 if qty_kg_total==. | qty_kg_total==0 | foodexcluded==0
	tab food_no `included'
		
	* Days eaten in household
	use MalawiIHPS_DietQual_PID, clear
	tab hh_b08
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	tab hh_b08 if !inlist(hh_b08,0,7)
	tab hh_b04 if !inlist(hh_b08,0,7) & hh_b08!=.
		
// TABLE 1 // "Summary Statistics"
	* Household Characteristics & Outcomes
	use MalawiIHPS_DietQual_HH, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)

	lab var reside "Rural (%)"
	lab var head_ed "Head education (years)"
	lab var spouse_ed "Spouse education (years)"

	global sumstats hhsize dependency_ratio reside head_ed spouse_ed pctfoodexp 
			
		eststo sum1: svy, subpop(if data_round==1): mean $sumstats
		eststo sum2: svy, subpop(if data_round==2): mean $sumstats
		eststo sum3: svy, subpop(if data_round==3): mean $sumstats
		eststo sum4: svy: mean $sumstats
			
	esttab sum1 sum2 sum3 sum4 using "Table 1.rtf", label replace cells("b(fmt(2)) se(par)") ///
		mlabels("2010" "2013" "2016/17" "Overall") title("Table 1. Population Summary Statistics") ///
		nonumbers noobs wide collabels("Mean" "(SE)") 

	lab var case_id "Households"
	eststo sum11: estpost sum case_id if data_round==1
	eststo sum12: estpost sum case_id if data_round==2
	eststo sum13: estpost sum case_id if data_round==3
	eststo sum14: estpost sum case_id
	esttab sum11 sum12 sum13 sum14 using "Table 1.rtf", label append ///
		cell(count) nonumbers wide noobs
	lab var case_id ""

	use MalawiIHPS_DietQual_PID, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	gen adult=1 if inlist(age_sex_grp,10,11,12,13,16,17,18,19,24,25)
	gen child=1 if inlist(age_sex_grp,1,2,3,4,5,6,7,8,9,14,15,23)
	egen totaladult=total(adult), by(case_id HHID y2_hhid y3_hhid data_round)
	egen totalchild=total(child), by(case_id HHID y2_hhid y3_hhid data_round)
	lab var totaladult "N Adults (>18)"
	lab var totalchild "N Children (<=18)"
	gen stuntedcombo=1 if stunted==1 | stunted==2 & stunted!=.
	replace stuntedcombo=0 if stunted==0 & stunted!=.
	lab var stuntedcombo "Under 5 stunting (%)"
	
	* Head occupation
	tab hh_e13_1a if hh_b04==1
	svy, subpop(if data_round==2 & daysate_conv!=0 & hh_b04==1): tab hh_e13_1a
	svy, subpop(if data_round==3 & daysate_conv!=0 & hh_b04==1): tab hh_e13_1a
	
		eststo sum5: svy, subpop(if data_round==1 & daysate_conv!=0): mean totaladult totalchild stuntedcombo 
		eststo sum6: svy, subpop(if data_round==2 & daysate_conv!=0): mean totaladult totalchild stuntedcombo 
		eststo sum7: svy, subpop(if data_round==3 & daysate_conv!=0): mean totaladult totalchild stuntedcombo 
		eststo sum8: svy, subpop(if daysate_conv!=0): mean totaladult totalchild stuntedcombo 
		
	esttab sum5 sum6 sum7 sum8 using "Table 1.rtf", label append ///
		cells("b(fmt(2) label(Mean)) se(par label((SE)))") ///
		nonumbers wide noobs
		
	lab var PID "Individuals"
	eststo sum15: estpost sum PID if data_round==1 & daysate_conv!=0 & age_sex_grp!=1
	eststo sum16: estpost sum PID if data_round==2 & daysate_conv!=0 & age_sex_grp!=1
	eststo sum17: estpost sum PID if data_round==3 & daysate_conv!=0 & age_sex_grp!=1
	eststo sum18: estpost sum PID if daysate_conv!=0 & age_sex_grp!=1
	esttab sum15 sum16 sum17 sum18 using "Table 1.rtf", label append cells(count) ///
		nonumbers wide noobs

	* Number of individuals eating no meals
	bys data_round: sum PID if daysate_conv==0 & age_sex_grp!=1
	lab var PID "Individuals consuming no meals"
	eststo sum19: estpost sum PID if data_round==1 & daysate_conv==0 & age_sex_grp!=1
	eststo sum20: estpost sum PID if data_round==2 & daysate_conv==0 & age_sex_grp!=1
	eststo sum21: estpost sum PID if data_round==3 & daysate_conv==0 & age_sex_grp!=1
	eststo sum22: estpost sum PID if daysate_conv==0 & age_sex_grp!=1
	esttab sum19 sum20 sum21 sum22 using "Table 1.rtf", label append cells(count) ///
		nonumbers wide noobs

	* Number of infants
	lab var PID "Infants"
	eststo sum23: estpost sum PID if data_round==1 &  age_sex_grp==1
	eststo sum24: estpost sum PID if data_round==2 &  age_sex_grp==1
	eststo sum25: estpost sum PID if data_round==3 &  age_sex_grp==1
	eststo sum26: estpost sum PID if  age_sex_grp==1
	esttab sum23 sum24 sum25 sum26 using "Table 1.rtf", label append cells(count) ///
		nonumbers wide noobs note("Population statistics, corrected using sampling weights." ///
		"Under 5 stunting defined as >2 standard deviations below median, combines moderate and severe")
	lab var PID ""

// FIGURE 1 // "Defining Needs for Nutrient Density by Age-Sex Group, Per Nutrient"
	use HHNeeds_IndivLevel, clear
	putexcel set "Figure 1", sheet(Density_DefiningNeeds, replace) modify
		putexcel A1="Household Sharing Scenario Nutrient Density", font(garamond, 14 bold)
		putexcel A2="", font(garamond, 11 bold)
		putexcel B3= "Age-Sex Group with Defining Need", font(garamond, 14 bold)
		putexcel B4:R4, overwritefmt merge hcenter
		putexcel B4="Frequency per Group, by nutrient", font(garamond, 12, white) bold hcenter fpattern(solid, gray) overwritefmt
		putexcel B5="Age-Sex Group", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel C5="Vit_A", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel D5="Vit_C", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel E5="Vit_E", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel F5="Thiamin", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel G5="Riboflavin", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel H5="Niacin", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel I5="Vit_B6", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel J5="Folate", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel K5="Vit_B12", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel L5="Calcium", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel M5="Copper", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel N5="Iron", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel O5="Magnesium", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel P5="Phosphorus", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel Q5="Selenium", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel R5="Zinc", font(garamond, 12, black) bold hcenter overwritefmt
		tab2 age_sex_grp nutr_no if ear_perkcal==max_ear_perkcal & ///
			!inlist(nutr_no,1,2,3,4,5,7,23) & !inlist(age_sex_grp,1,2,3), matcell(defining)
		local RowVar = "age_sex_grp"
		tabulate `RowVar' if !inlist(age_sex_grp,1,2,3), matcell(rowtotals)
		local RowCount = r(r)
		local RowValueLabel : value label `RowVar'
		levelsof `RowVar', local(RowLevels)
		forvalues row = 1/`RowCount' {
			local RowValueLabelNum = word("`RowLevels'", `row'+3)
			local CellContents : label `RowValueLabel' `RowValueLabelNum'
			local Cell ="B" + string(`row'+5)
			putexcel `Cell' = "`CellContents'", right font(garamond, 11, black)
		}
		putexcel C6=matrix(defining), right font(garamond, 11, black)
		* Tile chart created by conditional formatting

// FIGURE 2 // "Defining Upper Level Nutrient Density Tolerance by Age-Sex Group, Per Nutrient"
	use HHNeeds_IndivLevel, clear
	putexcel set "Figure 2", sheet(Density_DefiningTolerance, replace) modify
		putexcel A1="Household Sharing Scenario Nutrient Density", font(garamond, 14 bold)
		putexcel A2="", font(garamond, 11 bold)
		putexcel B3= "Age-Sex Group with Defining Need", font(garamond, 14 bold)
		putexcel B4:R4, overwritefmt merge hcenter
		putexcel B4="Frequency per Group, by nutrient", font(garamond, 12, white) bold hcenter fpattern(solid, gray) overwritefmt
		putexcel B5="Age-Sex Group", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel C5="Retinol", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel D5="Vit_C", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel E5="Vit_B6", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel F5="Calcium", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel G5="Copper", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel H5="Iron", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel I5="Phosphorus", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel J5="Selenium", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel K5="Zinc", font(garamond, 12, black) bold hcenter overwritefmt
		putexcel L5="Sodium", font(garamond, 12, black) bold hcenter overwritefmt
		tab2 age_sex_grp nutr_no if ul_perkcal==min_ul_perkcal & ///
			inlist(nutr_no,7,8,13,16,17,18,20,21,22,23) & !inlist(age_sex_grp,1,2,3), matcell(defining)
		local RowVar = "age_sex_grp"
		tabulate `RowVar' if !inlist(age_sex_grp,1,2,3), matcell(rowtotals)
		local RowCount = r(r)
		local RowValueLabel : value label `RowVar'
		levelsof `RowVar', local(RowLevels)
		forvalues row = 1/`RowCount' {
			local RowValueLabelNum = word("`RowLevels'", `row'+3)
			local CellContents : label `RowValueLabel' `RowValueLabelNum'
			local Cell ="B" + string(`row'+5)
			putexcel `Cell' = "`CellContents'", right font(garamond, 11, black)
		}
		putexcel C6=matrix(defining), right font(garamond, 11, black)
	
// TABLE 2 //
use MalawiIHPS_DietQual_Nut_PID, clear
	drop if nutr_no==1 // Placeholde for price
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	cap drop _*
	lab def resid 0 "urban" 1 "rural"
	lab val reside resid
	
	* Nutrient needs - per 1000 kcal
			gen iearperday=EAR_targeting/7
			replace iearperday=AMDRlow_targeting/7 if inlist(nutr_no,3,4,5)
			replace iearperday=(iearperday/kcal_perday)*1000
		gen earshareperday=EAR_sharing/7
			replace earshareperday=AMDRup_targeting/7 if inlist(nutr_no,3,4,5)
			replace earshareperday=(earshareperday/(KCAL_hh_perweek/7))*1000
	* % diff individual to shared
		gen eardiff=((earshareperday-iearperday)/iearperday)*100
		
	* UL/AMDR upper bounds for reference
		gen ulshareperday=UL_sharing/7
			replace ulshareperday=AMDRup_sharing/7 if inlist(nutr_no,3,4,5)
			replace ulshareperday=(ulshareperday/(KCAL_hh_perweek/7))*1000

		gen iulperday=ul
			replace iulperday=amdr_upper if inlist(nutr_no,3,4,5)
			replace iulperday=(iulperday/kcal_perday)*1000
	* % diff individual to shared
		gen uldiff=((ulshareperday-iulperday)/iulperday)*100

	foreach v in iearperday earshareperday eardiff ulshareperday iulperday uldiff {
		replace `v'=. if age_sex_grp==1
		}
		
		eststo a: svy, subpop(if !inlist(nutr_no,2,7,23) & age_sex_grp!=1): mean iearperday, over(nutr_no) 
		eststo b: svy, subpop(if !inlist(nutr_no,2,7,23) & age_sex_grp!=1): mean earshareperday, over(nutr_no) 
		eststo ab: svy, subpop(if !inlist(nutr_no,2,7,23) & age_sex_grp!=1): mean eardiff, over(nutr_no) 		
		eststo c: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): mean iulperday, over(nutr_no) 
		eststo d: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): mean ulshareperday, over(nutr_no) 
		eststo cd: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): mean uldiff, over(nutr_no) 
		esttab b a ab  using "Table 2", replace ///
			noobs label wide nostar nodepvars nogaps nonote rtf ///
			title("Table 2. Nutrient Needs and Upper Limits per 1,000 kcal") ///
			mtitles( "Shared" "Individual" "% Diff" "Individual" "Shared" "%Diff") cells("b(fmt(2)) se(par fmt(2))") ///
			collabels("mean" "se") mgroups("Panel A: Nutrient Needs", pattern(1 0 0)) ///
			note("Adjusted for survey weights." ///
			"* Macronutrient needs and limits defined by AMDR lower and upper bounds, respectively.")
		esttab d c  cd using "Table 2", append noobs label wide nostar nodepvars nogaps nonote rtf ///
			mtitles( "Sharing" "Individual" "% Diff" "Individual" "Shared" "%Diff") ///
			cells("b(fmt(2)) se(par fmt(2))") ///
			collabels("mean" "se") mgroups("Panel B: Limits", pattern(1 0 0)) ///
			note("Adjusted for survey weights." ///
			"* Macronutrient needs and limits defined by AMDR lower and upper bounds, respectively.")
			/* Nutrient units - provided in Nutrient Requirements spreadsheet:
			energy	kcal
			carbohydrate	g
			protein	g
			lipids	g
			vitA	mcg
			retinol	mcg
			vitC	mg
			vitE	mg
			thiamin	mg
			riboflavin	mg
			niacin	mg
			vitB6	mg
			folate	mcg
			vitB12	mcg
			calcium	mg
			copper	mg
			iron	mg
			magnesium	mg
			phosphorus	mg
			selenium	mcg
			zinc	mg
			sodium	mg
			*/
			* nutrients sorted on % difference highest to lowest, using excel

// Table 3 // Diet Quality	
use MalawiIHPS_DietQual_Nut_PID, clear
	drop if nutr_no==1 // Price
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	cap drop _*
	lab def resid 0 "urban" 1 "rural"
	lab val reside resid
	replace exceedshare_Eadjusted=exceedshare_Eadjusted*100
	replace hnar_share_Eadjusted_PIA=hnar_share_Eadjusted_PIA*100
	
		eststo a: svy, subpop(if !inlist(nutr_no,7,23) & age_sex_grp!=1): mean hnar_share_Eadjusted_PIA, over(nutr_no)
		eststo b: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): ///
			mean exceedshare_Eadjusted, over(nutr_no) 
		esttab a b using "Table 3", replace  cells("b(fmt(2)) se(par fmt(2))") ///
			noobs label wide nostar nodepvars nogaps nonote rtf ///
			title("Table 3. Prevalence of Poor Diet Quality") ///
			mtitles("Insufficient" "Excessive") collabels("Mean" "SE") ///
			note("Adjusted for survey weights.")

			
* Context for interpretation
	// Energy adequacy //
	use MalawiIHPS_DietQual_Nut_PID, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	sum hnar_share_PIA hnar_AME_PIA if nutr_no==2
	svy, subpop(if nutr_no==2): mean hnar_share_PIA hnar_AME_PIA

	// Percent of ASFs from purchases //
		use MalawiIHPS_DietQual_FoodHHExp, clear
		cap drop _*
		svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
		drop pct_buy
		foreach v of varlist qty_kg_* {
			replace `v'=0 if `v'==.
			}
		sum if qty_kg_total<(qty_kg_giftoth+qty_kg_ownprod+qty_kg_buy)  
		replace qty_kg_total=(qty_kg_giftoth+qty_kg_ownprod+qty_kg_buy) ///
			if qty_kg_total<(qty_kg_giftoth+qty_kg_ownprod+qty_kg_buy) 
		gen pct_qty_purchased=(qty_kg_buy/qty_kg_total)*100 
			lab var pct_qty_purchased "Percent food item consumption from purchases"
		gen pct_qty_ownprod=(qty_kg_ownprod/qty_kg_total)*100 
			lab var pct_qty_ownprod "Percent food item consumption from own production"
		gen pct_qty_gift=(qty_kg_ownprod/qty_kg_total)*100 
			lab var pct_qty_gift "Percent food item consumption from own production"

		tab food_no if foodexcluded==0, sum(pct_qty_purchased)
		tab food_no if foodexcluded==0, sum(pct_qty_ownprod)
		tab food_no if foodexcluded==0, sum(pct_qty_gift)
		
		gen pctcheck=(pct_qty_purchased+pct_qty_ownprod+pct_qty_gift)/100
		tab food_no, sum(pctcheck)

	// Sources of sodium //
		use MalawiIHPS_DietQual_FoodNUT_HHExp, clear
		svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)

		svy, subpop(if nutr_no==23 & tot_nutr_byfood_total!=0 & tot_nutr_byfood_total!=.): ///
			mean pcthhtotalnut_perfood, over(food_no)
		tempvar ateany
			gen `ateany'=1 if qty_kg_total!=0 & qty_kg_total!=.
			replace `ateany'=0 if qty_kg_total==0 | qty_kg_total==.
			svy, subpop(if nutr_no==23): mean `ateany', over(food_no)

// TABLE 4 // Impact of having different member types
use MalawiIHPS_DietQual_Nut_PID, clear
drop if nutr_no==1 // Price
svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
cap drop _*
lab def resid 0 "Urban" 1 "Rural"
lab val reside resid

* Collapse to HH level
gen childU8=1 if inlist(age_sex_grp,4,5,6,7)
	replace childU8=0 if childU8==.
gen adolescentgirl=1 if inlist(age_sex_grp,14,15)
	replace adolescentgirl=0 if adolescentgirl==.
gen wra=1 if inlist(age_sex_grp,16,17)
	replace wra=0 if wra==.
gen adultmale=1 if inlist(age_sex_grp,10,11)
	replace adultmale=0 if adultmale==.
replace lactating=0 if lactating==.
sort case_id HHID y2_hhid y3_hhid data_round id_code nutr_no
tabstat hnar_share_Eadjusted, by(nutr_no) stats(mean p50 min max)
tab nutr_no, sum(hnar_share_PIA)
tab nutr_no, sum(exceedshare_Eadjusted)
collapse (first) hnar_share_Eadjusted hnar_share hnar_share_PIA hnar_share_Eadjusted_PIA ///
	exceedshare exceedshare_Eadjusted wealth wealth_quintile reside pweight ea_id ///
	(max) lactating wra childU8 adolescentgirl adultmale, by(case_id HHID y2_hhid y3_hhid data_round nutr_no)
distinct case_id HHID y2_hhid y3_hhid data_round
unique case_id HHID y2_hhid y3_hhid data_round

* Single regression
replace hnar_share_Eadjusted_PIA=hnar_share_Eadjusted_PIA*100
replace exceedshare_Eadjusted=exceedshare_Eadjusted*100
lab var hnar_share_Eadjusted_PIA "% Population with Insufficient Nutrient Density in Diet"
lab var exceedshare_Eadjusted "% Population with Excessive Nutrient Density in Diet"
lab var reside "Rural"
lab var wealth_quintile "Wealth Quintile (asset index)"
lab var lactating "Breastfeeding woman present"
lab var wra "Woman of reproductive age (19-50) present"
lab var adultmale "Adult male (19-50) present"
lab var adolescentgirl "Adolescent girl (9-18)"
lab var childU8 "Child 3-8 present"

	* Lower bounds
	replace hnar_share_Eadjusted_PIA=hnar_share_Eadjusted_PIA*100
	eststo r3: svy: reg hnar_share_Eadjusted_PIA adolescentgirl wra lactating if nutr_no==3
	esttab r3 using "Table 4a", replace b(%9.2fc) se(%9.2fc) label nodepvars nogaps rtf collab("Mean (SE)") mlabel("3") 

	forval n=4/6 {
		eststo r`n': svy: reg hnar_share_Eadjusted_PIA adolescentgirl wra lactating if nutr_no==`n'
	esttab r`n' using "Table 4a", append b(%9.2fc) se(%9.2fc) label nodepvars nogaps rtf collab("Mean (SE)") mlabel("`n'")
	}
	forval n=8/22 {
		eststo r`n': svy: reg hnar_share_Eadjusted_PIA adolescentgirl wra lactating if nutr_no==`n'
		esttab r`n' using "Table 4a", append b(%9.2fc) se(%9.2fc) label nodepvars nogaps rtf collab("Mean (SE)") mlabel("`n'")
	}
	
	* Upper bounds
	replace exceedshare_Eadjusted=exceedshare_Eadjusted*100
	eststo r3: svy: reg exceedshare_Eadjusted childU8 adultmale if nutr_no==3
	esttab r3 using "Table 4b", replace b(%9.2fc) se(%9.2fc) label nodepvars nogaps rtf collab("Mean (SE)") mlabel("3") 

foreach n in 4 5 7 8 13 16 17 18 20 21 22 {
	eststo r`n': svy: reg exceedshare_Eadjusted childU8 adultmale  if nutr_no==`n'
	esttab r`n' using "Table 4b", append b(%9.2fc) se(%9.2fc) label nodepvars nogaps rtf collab("Mean (SE)") mlabel("`n'")
	}

	
// Fig 3 // Urban rural
use MalawiIHPS_DietQual_Nut_PID, clear
	
	drop if nutr_no==1 // Price
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	cap drop _*
	lab def resid 0 "Urban" 1 "Rural"
	lab val reside resid

	forval n=3/6 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA reside if nutr_no==`n'
	}
	forval n=8/22 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA reside if nutr_no==`n'
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein*" ///
           5 "Lipids***" /// 
           6 "Vitamin A" /// 
           7 "Retinol" ///
           8 "Vitamin C*" /// 
           9 "Vitamin E**" /// 
          10 "Thiamin" ///
          11 "Riboflavin*" /// 
          12 "Niacin" /// 
          13 "Vitamin B6" /// 
          14 "Folate" ///
          15 "Vitamin B12***" /// 
          16 "Calcium*" /// 
          17 "Copper***" /// 
          18 "Iron" /// 
          19 "Magnesium" /// 
          20 "Phosphorus" /// 
          21 "Selenium***" /// 
          22 "Zinc" /// 
          23 "Sodium", replace  
	lab val nutr_no nutr
	
replace hnar_share_Eadjusted_PIA=hnar_share_Eadjusted_PIA*100
graph hbar hnar_share_Eadjusted_PIA if !inlist(nutr_no,2,7,23) [pw=pweight], ///
	over(reside) over(nutr_no, label(labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") ///
	title("Insufficient for Lower Bound Requirements", size(small) margin(0 0 0 3)) ///
	name(inad, replace) graphregion(margin(zero)) legend(pos(5) ring(0))
graph export "Fig 3A.png", replace 


	foreach n in 3 4 5 7 8 16 17 18 20 21 22 23 {
	di as text "nutr_no=`n'"
	svy: logit exceedshare_Eadjusted reside if nutr_no==`n'
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein" ///
           5 "Lipids**" /// 
           7 "Retinol" ///
           8 "Vitamin C" /// 
          16 "Calcium*" /// 
          17 "Copper*" /// 
          18 "Iron" /// 
          20 "Phosphorus***" /// 
          21 "Selenium***" /// 
          22 "Zinc***", replace  
	lab val nutr_no nutr	
	
replace exceedshare_Eadjusted=exceedshare_Eadjusted*100	
graph hbar exceedshare_Eadjusted if inlist(nutr_no,3,4,5,7,8,16,17,18,20,21,22) [pw=pweight], ///
	over(reside) over(nutr_no, label(labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("") ///
	title("Exceeding Upper Bounds Limits", size(small) margin(0 0 0 3)) ///
	legend(pos(5) ring(0)) graphregion(margin(zero)) name(exc, replace)
graph export "Fig 3B.png", replace

// Fig 4 // by wealth quintile
use MalawiIHPS_DietQual_Nut_PID, clear
drop wealth wealth_quintile
* replace wealth vars with recreated
merge m:1 case_id HHID y2_hhid y3_hhid data_round ///
	using "C:\Users\kates\Box\PhD\5. Dissertation\05_Final datasets & Output\HH_wealth", ///
	keepusing(wealth wealth_quintile)
	drop if _merge==2 // 15 ate no meals
	drop _merge

	drop if nutr_no==1 // Price
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	cap drop _*
	lab def resid 0 "Urban" 1 "Rural"
	lab val reside resid

	forval n=3/6 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA i.wealth_quintile if nutr_no==`n'
	}
	forval n=8/22 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA i.wealth_quintile if nutr_no==`n'
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein***" ///
           5 "Lipids*" /// 
           6 "Vitamin A" /// 
           8 "Vitamin C*" /// 
           9 "Vitamin E***" /// 
          10 "Thiamin" ///
          11 "Riboflavin" /// 
          12 "Niacin" /// 
          13 "Vitamin B6" /// 
          14 "Folate" ///
          15 "Vitamin B12***" /// 
          16 "Calcium" /// 
          17 "Copper" /// 
          18 "Iron" /// 
          19 "Magnesium" /// 
          20 "Phosphorus" /// 
          21 "Selenium" /// 
          22 "Zinc", replace  
	lab val nutr_no nutr
	
replace hnar_share_Eadjusted_PIA=hnar_share_Eadjusted_PIA*100
graph hbar hnar_share_Eadjusted_PIA if !inlist(nutr_no,2,7,23) & inlist(wealth_quintile,1,5) [pw=pweight], ///
	over(wealth_quintile) over(nutr_no, label(labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") ///
	title("Insufficient for Lower Bound Requirements", size(small) margin(0 0 0 2)) ///
	legend(pos(5) ring(0)) graphregion(margin(zero))
* Notes: Population statistics corrected using survey weights
* Stars represent statistically significant difference between the wealthiest and poorest 
	*as tested in bivariate regression ***p<0.001 **0<0.01 *p<0.05
graph export "Fig 4A.png", replace	

	foreach n in 3 4 5 7 8 16 17 18 20 21 22 23 {
	di as text "nutr_no=`n'"
	svy: logit exceedshare_Eadjusted i.wealth_quintile if nutr_no==`n'
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein" ///
           5 "Lipids**" /// 
           7 "Retinol" ///
           8 "Vitamin C*" /// 
          16 "Calcium" /// 
          17 "Copper" /// 
          18 "Iron" /// 
          20 "Phosphorus" /// 
          21 "Selenium*" /// 
          22 "Zinc*", replace  
	lab val nutr_no nutr	

replace exceedshare_Eadjusted=exceedshare_Eadjusted*100
graph hbar exceedshare_Eadjusted if !inlist(nutr_no,2,6,8,9,10,11,12,13,14,15,19,23) ///
	& inlist(wealth_quintile,1,5) [pw=pweight], ///
	over(wealth_quintile) over(nutr_no, label(labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") ///
	title("Exceeding Upper Bounds Limits", size(small) margin(0 0 0 2)) ///
	legend(pos(5) ring(0)) graphregion(margin(zero))
* Notes: Population statistics corrected using survey weights
* Stars represent statistically significant difference between the wealthiest and poorest 
	*as tested in bivariate regression ***p<0.001 **0<0.01 *p<0.05
graph export "Fig 4B.png", replace	

	* RURAL ONLY
	forval n=3/6 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA i.wealth_quintile if nutr_no==`n' & reside==1
	}
	forval n=8/22 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA i.wealth_quintile if nutr_no==`n' & reside==1
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein***" ///
           5 "Lipids" /// 
           6 "Vitamin A" /// 
           7 "Retinol" ///
           8 "Vitamin C*" /// 
           9 "Vitamin E***" /// 
          10 "Thiamin" ///
          11 "Riboflavin" /// 
          12 "Niacin" /// 
          13 "Vitamin B6" /// 
          14 "Folate" ///
          15 "Vitamin B12**" /// 
          16 "Calcium" /// 
          17 "Copper" /// 
          18 "Iron" /// 
          19 "Magnesium" /// 
          20 "Phosphorus" /// 
          21 "Selenium" /// 
          22 "Zinc*" /// 
          23 "Sodium", replace  
	lab val nutr_no nutr

graph bar hnar_share_Eadjusted_PIA if !inlist(nutr_no,2,7,23) ///
	& inlist(wealth_quintile,1,5) & reside==1 [pw=pweight], ///
	over(wealth_quintile) over(nutr_no, label(angle(45) labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") title("Inadequate Nutrient Density, Rural households, by asset-based wealth quintiles") ///
	legend(pos(2) ring(0)) graphregion(margin(zero))
* Notes: Population statistics corrected using survey weights
* Stars represent statistically significant difference between the wealthiest and poorest 
	*as tested in bivariate regression ***p<0.001 **0<0.01 *p<0.05
graph export "Fig 4A_topbottom RURAL only.png", replace	

* By expenditure quintile instead of asset
xtile exp_quintile_urb1=totalexpenditure if reside==0 & data_round==1, nq(5)
xtile exp_quintile_urb2=totalexpenditure if reside==0 & data_round==2, nq(5)
xtile exp_quintile_urb3=totalexpenditure if reside==0 & data_round==3, nq(5)
xtile exp_quintile_rur1=totalexpenditure if reside==1 & data_round==1, nq(5)
xtile exp_quintile_rur2=totalexpenditure if reside==1 & data_round==2, nq(5)
xtile exp_quintile_rur3=totalexpenditure if reside==1 & data_round==3, nq(5)
gen exp_quintile=.
	replace exp_quintile=exp_quintile_urb1 if reside==0 & data_round==1
	replace exp_quintile=exp_quintile_urb2 if reside==0 & data_round==2
	replace exp_quintile=exp_quintile_urb3 if reside==0 & data_round==3
	replace exp_quintile=exp_quintile_rur1 if reside==1 & data_round==1
	replace exp_quintile=exp_quintile_rur2 if reside==1 & data_round==2
	replace exp_quintile=exp_quintile_rur3 if reside==1 & data_round==3
	lab val exp_quintile wealth
	
	forval n=3/6 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA i.exp_quintile if nutr_no==`n'
	}
	forval n=8/22 {
	di as text "nutr_no=`n'"
	svy: logit hnar_share_Eadjusted_PIA i.exp_quintile if nutr_no==`n'
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein***" ///
           5 "Lipids" /// 
           6 "Vitamin A" /// 
           7 "Retinol" ///
           8 "Vitamin C" /// 
           9 "Vitamin E***" /// 
          10 "Thiamin" ///
          11 "Riboflavin" /// 
          12 "Niacin***" /// 
          13 "Vitamin B6*" /// 
          14 "Folate*" ///
          15 "Vitamin B12*" /// 
          16 "Calcium" /// 
          17 "Copper" /// 
          18 "Iron" /// 
          19 "Magnesium" /// 
          20 "Phosphorus" /// 
          21 "Selenium" /// 
          22 "Zinc***" /// 
          23 "Sodium", replace  
	lab val nutr_no nutr
	
graph bar hnar_share_Eadjusted_PIA if !inlist(nutr_no,2,7,23) [pw=pweight], ///
	over(exp_quintile) over(nutr_no, label(angle(45) labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") title("Inadequate Nutrient Density") ///
	legend(pos(2) ring(0)) graphregion(margin(zero))
* Notes: Population statistics corrected using survey weights
* Stars represent statistically significant difference between the wealthiest and poorest 
	*as tested in bivariate regression ***p<0.001 **0<0.01 *p<0.05
graph export "Fig 4A_expquint.png", replace	

graph bar hnar_share_Eadjusted_PIA if !inlist(nutr_no,2,7,23) & ///
	inlist(exp_quintile,1,5) [pw=pweight], ///
	over(exp_quintile) over(nutr_no, label(angle(45) labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") title("Inadequate Nutrient Density, by expenditure quintiles") ///
	legend(pos(2) ring(0)) graphregion(margin(zero))
* Notes: Population statistics corrected using survey weights
* Stars represent statistically significant difference between the wealthiest and poorest 
	*as tested in bivariate regression ***p<0.001 **0<0.01 *p<0.05
graph export "Fig 4A_expquint topbottom only.png", replace	

// Supplementary materials // repeat fig 4 for excess
	foreach n in 3 4 5 7 8 13 16 17 18 20 21 22 23 {
	di as text "nutr_no=`n'"
	svy: logit exceedshare i.wealth_quintile if nutr_no==`n'
	}

	lab def nutr ///
           3 "Carbohydrate***" ///
           4 "Protein**" ///
           5 "Lipids***" /// 
           7 "Retinol**" ///
           8 "Vitamin C" /// 
          13 "Vitamin B6" /// 
          16 "Calcium***" /// 
          17 "Copper***" /// 
          18 "Iron***" /// 
          20 "Phosphorus**" /// 
          21 "Selenium**" /// 
          22 "Zinc*" /// 
          23 "Sodium**", replace  
	lab val nutr_no nutr	
	
replace exceedshare=exceedshare*100	
graph bar exceedshare if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) [pw=pweight], ///
	over(wealth_quintile) over(nutr_no, label(angle(45) labsize(small)) sort(1) descending) ///
	scheme(plotplain) asyvars ytitle("% Population") title("Excessive Nutrient Density") ///
	legend(pos(2) ring(0)) graphregion(margin(zero)) yscale(range(0(20)100)) ytick(#5) ///
	ylabel(#5, val)
* Notes: Population statistics corrected using survey weights
* Stars represent statistically significant difference between the wealthiest and poorest 
	*as tested in bivariate regression ***p<0.001 **0<0.01 *p<0.05
graph export "Fig 4B.png", replace	


***************SUPPLEMENTARY 
// Figure A
use MalawiIHPS_DietQual_Nut_PID, clear
drop if nutr_no==1 // Price
svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	cap drop _*
	lab def resid 0 "urban" 1 "rural"
	lab val reside resid
	drop if reside==0
	drop if data_round==1
	* Nutrient needs - per 1000 kcal
			gen iearperday=ear_perweek/7
			replace iearperday=amdr_lower_perweek/7 if inlist(nutr_no,3,4,5)
			gen ienergy1=iearperday if nutr_no==2
			egen ienergy=total(ienergy1), by(PID data_round)
			replace iearperday=(iearperday/ienergy)*1000
		gen earshareperday=EAR_sharing/7
			replace earshareperday=AMDRlow_sharing/7 if inlist(nutr_no,3,4,5)
			replace earshareperday=(earshareperday/(KCAL_hh_perweek/7))*1000
	* % diff individual to shared
		gen eardiff=((earshareperday-iearperday)/iearperday)*100
		
	* UL/AMDR upper bounds for reference
		gen ulshareperday=UL_sharing/7
			replace ulshareperday=AMDRup_sharing/7 if inlist(nutr_no,3,4,5)
			replace ulshareperday=(ulshareperday/(KCAL_hh_perweek/7))*1000

		gen iulperday=ul_perweek/7
			replace iulperday=amdr_upper_perweek/7 if inlist(nutr_no,3,4,5)
			replace iulperday=(iulperday/ienergy)*1000
	* % diff individual to shared
		gen uldiff=((ulshareperday-iulperday)/iulperday)*100
	
	collapse (mean) eardiff uldiff [pw=pweight], by(nutr_no)
	replace uldiff=0 if uldiff>0
	lab var eardiff "Lower bound"
	lab var uldiff "Upper bound"
	cap lab drop nutr
	lab def nutr 1 "Price" /// 
           2 "Energy " ///
           3 "Carbohydrate " ///
           4 "Protein" ///
           5 "Lipids" /// 
           6 "Vitamin A" /// 
           7 "Retinol" ///
           8 "Vitamin C" /// 
           9 "Vitamin E" /// 
          10 "Thiamin" ///
          11 "Riboflavin" /// 
          12 "Niacin" /// 
          13 "Vitamin B6" /// 
          14 "Folate" ///
          15 "Vitamin B12" /// 
          16 "Calcium" /// 
          17 "Copper" /// 
          18 "Iron" /// 
          19 "Magnesium" /// 
          20 "Phosphorus" /// 
          21 "Selenium" /// 
          22 "Zinc" /// 
          23 "Sodium"  
	lab val nutr_no nutr

tw dropline eardiff nutr_no if !inlist(nutr_no,7,23), msymbol(a) lcolor(black) msize(large) ///
		mcolor(black) yaxis(1) yscale(range(0(10)200) axis(1) extend) ///
		ylabel(#4, axis(1)) ytick(0(50)190, axis(1)) ///
		ytitle("% Increase in Lower Bound", size(small) margin(0 1 0 0) axis(1)) || ///
	dropline uldiff nutr_no if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23), ///
		msymbol(V) yaxis(2) lcolor(gs6) msize(med) mcolor(gs6) ///
		ytitle("% Decrease in Upper Bound", size(small) margin(1 0 0 0) axis(2) orientation(rvertical)) ///
		yscale(range(0(10)-200) extend axis(2)) ylabel(#4, axis(2)) xtitle("") ytick(0(50)-190, axis(2)) ///
		scheme(plotplain) xlabel(#21, valuelabel angle(vertical) labsize(small)) legend(off) ///
		yline(50, lcolor(gs9)) yline(-50, lcolor(gs9) axis(2))
	* Manually add the arrows to the axes
	* graph export "Supp Fig A.png", replace

	
// Supplementary Table A // Nutrient Needs & Requirements per Nutrient, Including Sharing expressed in per capita terms
	use MalawiIHPS_DietQual_Nut_PID, clear
	drop if nutr_no==1 // Price
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	* Nutrient needs for reference
		tempvar earameperday
			gen `earameperday'=ear_perweek/7
			replace `earameperday'=amdr_lower_perweek/7 if inlist(nutr_no,3,4,5)
		tempvar earshareperdaypercap
			gen `earshareperdaypercap'=(EAR_sharing/7)/hhsize
			replace `earshareperdaypercap'=(AMDRlow_sharing/7)/hhsize if inlist(nutr_no,3,4,5)
		tempvar earshareperday
			gen `earshareperday'=EAR_sharing/7
			replace `earshareperday'=AMDRlow_sharing/7 if inlist(nutr_no,3,4,5)

	* UL/AMDR upper bounds for reference
		tempvar ulameperday
			gen `ulameperday'=ul_perweek/7
			replace `ulameperday'=amdr_upper_perweek/7 if inlist(nutr_no,3,4,5)
		tempvar ulshareperdaypercap
			gen `ulshareperdaypercap'=(UL_sharing/7)/hhsize
			replace `ulshareperdaypercap'=(AMDRup_sharing/7)/hhsize if inlist(nutr_no,3,4,5)
		tempvar ulshareperday
			gen `ulshareperday'=UL_sharing/7
			replace `ulshareperday'=AMDRup_sharing/7 if inlist(nutr_no,3,4,5)

		eststo a: svy, subpop(if !inlist(nutr_no,7,23) & age_sex_grp!=1): mean `earameperday', over(nutr_no) 
		eststo b: svy, subpop(if !inlist(nutr_no,7,23) & age_sex_grp!=1): mean `earshareperdaypercap', over(nutr_no) 
		eststo c: svy, subpop(if !inlist(nutr_no,7,23) & age_sex_grp!=1): mean `earshareperday', over(nutr_no) 
		eststo d: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): mean `ulameperday', over(nutr_no) 
		eststo e: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): mean `ulshareperdaypercap', over(nutr_no) 
		eststo f: svy, subpop(if inlist(nutr_no,3,4,5,7,8,13,16,17,18,20,21,22,23) & age_sex_grp!=1): mean `ulshareperday', over(nutr_no) 
		esttab a b c d e f using "Supp Table A", replace ///
			noobs label wide nostar nodepvars nogaps nonote rtf ///
			title("Table A. Nutrient Needs and Limits, per nutrient") ///
			mtitles("Individual" "Shared Per Capita" "Shared Household Total" "Individual" "Shared Per Capita" "Shared Household Total") ///
			collabels("mean" "se") mgroups("Nutrient Needs" "Limits", pattern(1 0 0 1 0 0)) ///
			note("Adjusted for survey weights." ///
			"* Macronutrient needs and limits defined by AMDR lower and upper bounds, respectively.")

			/* Nutrient units - provided in Nutrient Requirements excel spreadsheet:
			energy	kcal
			carbohydrate	g
			protein	g
			lipids	g
			vitA	mcg
			retinol	mcg
			vitC	mg
			vitE	mg
			thiamin	mg
			riboflavin	mg
			niacin	mg
			vitB6	mg
			folate	mcg
			vitB12	mcg
			calcium	mg
			copper	mg
			iron	mg
			magnesium	mg
			phosphorus	mg
			selenium	mcg
			zinc	mg
			sodium	mg
		*/
	
	
// Supplementary Table B // Adequacy Ratios
	use MalawiIHPS_DietQual_Nut_PID, clear
	drop if nutr_no==1 // Price
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	cap drop _*
		// Comparing approaches by nutrient
		eststo a: svy, subpop(if !inlist(nutr_no,7,23) & age_sex_grp!=1): mean NAR_AME, over(nutr_no) 
		eststo b: svy, subpop(if !inlist(nutr_no,7,23) & age_sex_grp!=1): mean hnar_share, over(nutr_no)
		eststo c: svy, subpop(if !inlist(nutr_no,2,7,23) & age_sex_grp!=1): mean NAR_AME_Eadjusted, over(nutr_no) 
		eststo d: svy, subpop(if !inlist(nutr_no,2,7,23) & age_sex_grp!=1): mean hnar_share_Eadjusted, over(nutr_no)
		esttab b a d c using "Supp Table B", replace ///
			noobs label wide nostar nodepvars nogaps nonote rtf ///
			title("Table B-1. Mean Nutrient Adequacy Ratios by Nutrient & Estimation Method") ///
			mtitles("Shared" "AME" "Shared" "AME") ///
			collabels("Mean" "SE") mgroups("Reported Intakes" ///
			"Energy-Adjusted Intakes", pattern(1 0  1 0 )) ///
			note("Adjusted for survey weights.")

		// Percentiles for Energy:
		svy, subpop(if nutr_no==2 & age_sex_grp!=1): mean hnar_share
		svy, subpop(if nutr_no==2 & age_sex_grp!=1): mean hnar_target
		epctile hnar_share, svy subpop(if nutr_no==2 & age_sex_grp!=1) ///
			p(5 10 25 50 75 90 95)
		epctile hnar_target, svy subpop(if nutr_no==2 & age_sex_grp!=1) ///
			p(5 10 25 50 75 90 95)
			
	// Supplementary Table C // Food Group sources of nutrients
	use MalawiIHPS_DietQual_FoodNUT_HHExp, clear
	svyset ea_id [pweight=pweight], strata(reside) singleunit(centered)
	replace pcthhtotalnut_perfood=pcthhtotalnut_perfood*100
		forval n=3/23 {
		eststo m`n': svy, subpop(if nutr_no==`n'): mean pcthhtotalnut_perfood, ///
			over(food_group)
		}
		
	esttab m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 ///
		using "Supp Table C", replace noobs label cells(b(fmt(2)) se(fmt(2) par)) ///
		nostar nodepvars nogaps nonote rtf mtitles("Carbohydrate" ///
		"Protein" "Lipids" "Vitamin A" "Retinol" "Vitamin_C" ///
		"Vitamin_E" "Thiamin" "Riboflavin" "Niacin") ///
		title("Table B: Food group contributions to nutrient totals (percent total nutrient intake from the food group)")

	esttab m13 m14 m15 m16 m17 m18 m19 m20 m21 m22 m23 ///
		using "Supp Table C", append noobs label cells(b(fmt(2)) se(fmt(2) par)) ///
		nostar nodepvars nogaps nonote rtf mtitles("Vitamin B6" ///
		"Folate" "Vitamin_B12" "Calcium" "Copper" "Iron" ///
		"Magnesium" "Phosphorus" "Selenium" "Zinc" "Sodium") ///
		note("Adjusted for survey weights. Standard errors in parentheses.")
