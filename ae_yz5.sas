 /*====================================================================
| COMPANY           Bancova LLC
| PROJECT:          BANCOVA SAS TRAINING
| 
|
| PROGRAM:       	ae.yz.sas
| PROGRAMMER(S):    Yonghua Zhuang
| DATE:             07/21/2016
| PURPOSE:          Generate AE Table
|                        
| QC PROGRAMMER(S): 
| QC DATE:          
|
| INPUT FILE DIRECTORY(S): C:\bancova2016summer\data\raw      
| OUTPUT FILE DIRECTORY(S):C:\bancova2016summer\data\output       
| OUTPUT AND PRINT SPECIFICATIONS: Disposition.RTF    
|
|
| REVISION HISTORY
| DATE     BY        COMMENTS
|
|
=====================================================================*/
 

*----------------------------------------------*;
* options, directories, librefs etc.
*----------------------------------------------*;

** clean the log and output screen; 
dm 'log; clear; output; clear';

** log, output and procedure options; 
options center formchar="|____|||___+=|_/\<>*" missing = '.' nobyline nodate;


** define the location for the input and output; 
libname raw "C:\bancova2016summer\data\raw" access=readonly;
libname derived "C:\bancova2016summer\data\derived";
%let outdir=C:\bancova2016summer\data\output;


*****************************************;
*              Import Data              *;
*****************************************;
%macro import(xlsfile);
proc import out= work.&xlsfile 
            datafile= "C:\bancova2016summer\data\raw\&xlsfile..xls" 
            dbms=XLS replace;
run;

%mend import;
%import(adverseEvents); quit;
%import(demog_data); quit;

proc format;
	value treatmnt
	     1='Anticancer000'
		 2='Anticancer001'
		 3='Total';
	value severity
	     1='Mild'
		 2='Moderate'
		 3='Severe';
run;

*Select safety population with known subjid;
data dm;
	set demog_data;
	where ^missing(subjid);
	where safety=1;
run;


** Impute missing severity with most severe value (3);
data ae;
	set adverseevents (keep = subjid soct ptterm severity related);
	where ^missing(subjid) and ^missing(subjid) and ^missing(soct);
	if severity=. then severity=3;
run;


** Find duplicate and keep unique observations for dm; 
** Find duplicate and keep most severe observation for ae;
%macro dupfind (source=,var1=,var2=,lastvar=,dups=,uni=);
Proc sort data=&source;
		by &var1;
run;
data &uni &dups;
     set &source;
     by &var2;
     if not (first.&lastvar and last.&lastvar) then output &dups;
     if last.&lastvar then output &uni;
run;
%mend dupfind;
%dupfind(source=dm,var1=subjid, var2=subjid, lastvar=subjid, dups=demogdup, uni=dm1);
%dupfind(source=ae,var1=subjid soct ptterm severity, var2=subjid soct ptterm severity, lastvar=ptterm, dups=aedup, uni=ae1);

***merge dm and ae dataset;
data final1 missing;
   merge dm (keep=subjid treatmnt safety in=in1) ae (keep=subjid soct ptterm severity related in=in2);
   by subjid;
   if in1 and in2 then output final1;
   array check1{*} _NUMERIC_;
   do i1 = 1 to dim(check1);
      if missing(check1{i1}) then output missing;
   end;
   drop i1;
   array check2{*} _CHARACTER_;
   do i2 = 1 to dim(check2);
      if missing(check2{i2}) then output missing;
   end;
   format 	treatmnt treatmnt.
   			severity severity.;
   drop i2;
run ;
*** duplicate the data set for total column;

data final1;
	set final1;
	output;
	treatmnt=3;
	output;
run;

**Check related level;
proc freq data=final1;
	table related;
run; 

data relatedfinal1;
	set final1;
	where related="POSSIBLY RELATED";
run;
proc print data=final1;
run;

proc print data=relatedfinal1;
run;

data final1;
	set final1 (drop=related);
run;
	
data relatedfinal1;
	set relatedfinal1 (drop=related);
run;

* Compute safety population freq by treatmnt;
proc sql noprint;
	select count(distinct subjid) into :tt1-:tt3
	from final1
	group by treatmnt
	order by treatmnt;
	quit;
%put &tt1, &tt2, &tt3;

**macro to count ae population by treatment group;
%macro compute_aep(file=, table=, class=, where=, label= );
proc sql;
	create table &table as
	select &class, count(distinct subjid) as subt
	from (
		select *
		from &file
		where treatmnt in &where
		)
	group by &class;
	quit;

proc transpose data=&table out=&table (drop=_name_);
	var subt;
	id &class;
	idlabel &class;
run;

data &table;
	set &table;
	soctID=0; ptID=0; Vars=&label;
run;
%mend;


%compute_aep(table=aep1, file=final1, class=treatmnt, where=(1,2,3), label="Subjects with at Least One AE"); quit; *count by treatment group;
%compute_aep(table=aep2, file=relatedfinal1, class=severity, where=(1), label="Subjects with at Least One Drug Related AE"); quit; *count by severity level;
%compute_aep(table=aep3, file=relatedfinal1, class=severity, where=(2), label="Subjects with at Least One Drug Related AE"); quit;

proc print data=aep3;
run;

** Introduce soctID and ptID for data manipulation;
proc sort data=final1;
	by soct ptterm;
run;

proc sort data=relatedfinal1;
	by soct ptterm;
run;

data final2;
	set final1;
	by soct ptterm;
	if first.soct then soctID+100;
	if first.ptterm then ID+1;
	ptID=soctID+ID;
	drop ID;
run;

data relatedfinal2;
	set relatedfinal1;
	by soct ptterm;
	if first.soct then soctID+100;
	if first.ptterm then ID+1;
	ptID=soctID+ID;
	drop ID;
run;

**macro to count ae population soctID, ptID by treatment and severity level; 
%macro compute(output=, file=, IDs=, vars=, group=, tgroup=, by=);
proc sql;
	create table &output as
	select distinct &IDs, &vars, &group, count(distinct subjid)
	from(
		select distinct subjid, &IDs, &vars, &group
		from &file
		where treatmnt in (&tgroup.)
		)
		group by &group, &vars;
	quit;

proc transpose data=&output out=&output (drop=_name_);
	by &by;
	var _temg001;
	id &group;
	idlabel &group;
run;

%mend;

%compute(output=summary11, file=final2, IDs=%str(soctID,ptID), vars=%str(soct, ptterm), group=treatmnt, tgroup=%str(1,2,3), by= soctID ptID soct ptterm);
%compute(output=summary12, file=final2,IDs=soctID, vars=soct, group=treatmnt, tgroup=%str(1,2,3), by=soctID soct);
%compute(output=summary21, file=relatedfinal2,IDs=%str(soctID,ptID), vars=%str(soct, ptterm), group=severity, tgroup=1, by= soctID ptID soct ptterm);
%compute(output=summary22, file=relatedfinal2, IDs=soctID, vars=soct, group=severity, tgroup=1, by=soctID soct);
%compute(output=summary31, file=relatedfinal2, IDs=%str(soctID,ptID), vars=%str(soct, ptterm), group=severity, tgroup=2, by= soctID ptID soct ptterm);
%compute(output=summary32, file=relatedfinal2, IDs=soctID, vars=soct, group=severity, tgroup=2, by=soctID soct);

*Combind tables and caculate percentages;
%macro concate(output=, var1=, var2=, var3=, from1=, from2=, from3=, );
data &output;
	length Vars $70;
	set &from1
		&from2(rename=(ptterm=Vars))
		&from3(rename=(soct=Vars));
	drop soct;
run;

data &output (rename=(a1=&var1 a2=&var2 a3=&var3));
	set &output;
	length a1 a2 a3 $12;
	if ptID > 0 then Vars='   ' || Vars;
	%do i=1 %to 3;
		if &&&var&i='' then &&&var&i=0;
		a&i=compress(&&&var&i ||'('||put(100 * &&&var&i/&&&tt&i, 5.1)||')');
	%end;
	drop &var1 &var2 &var3 ;
run;

proc sort data=&output out=&output;
	by soctID ptID;
run;
%mend;

%concate(output=ae1, var1=Anticancer000, var2=Anticancer001, var3=Total,from1=aep1, from2=summary11, from3=summary12); quit;

*Combind tables and caculate percentages;
%macro concate1(output=, var1=, var2=, var3=, from1=, from2=, from3=, tt=);
data &output;
	length Vars $70;
	set &from1
		&from2(rename=(ptterm=Vars))
		&from3(rename=(soct=Vars));
	drop soct;
run;

data &output (rename=(a1=&var1 a2=&var2 a3=&var3));
	set &output;
	length a1 a2 a3 $12;
	if ptID > 0 then Vars='   ' || Vars;
	%do i=1 %to 3;
		if &&&var&i='' then &&&var&i=0;
		a&i=compress(&&&var&i ||'('||put(100 * &&&var&i/&&&tt, 5.1)||')');
	%end;
	drop &var1 &var2 &var3 ;
run;

proc sort data=&output out=&output;
	by soctID ptID;
run;
%mend;

%concate1(output=ae2, var1=Mild, var2=Moderate, var3=Severe,from1=aep2, from2=summary21, from3=summary22, tt=tt1); quit;
%concate1(output=ae3, var1=Mild, var2=Moderate, var3=Severe,from1=aep3, from2=summary31, from3=summary32, tt=tt2); quit;


*Insert Blank Rows between soct groups;
data ae11 (drop=soctID ptID); 
	do _n_=1 by 1 until(last.soctID);
		set ae1 end=last;
		by soctID notsorted;
		output;
	end;
	call missing(of _all_);
	if not last then output;
run;


*** create styles used for RTF-output ***;
proc template;
    DEFINE STYLE PANDA;
	PARENT= Styles.sasdocprinter;
	style fonts FROM FONTS /
		'titleFont' = ("courier new",12pt)
		'titleFont2'= ("courier new", 8pt)
		'headingFont' = ("times roman",10pt, bold)                            
        'docFont' = ("times roman",10pt);	
	style SystemFooter from systemfooter/
		font=fonts('titleFont2');
	replace body/
		bottommargin = 1in                                                
        topmargin = 1.5in                                                   
        rightmargin = 1in                                                 
        leftmargin = 1in; 	
	style TABLE from table/
		cellpadding=0
		cellspacing=0
		OUTPUTWIDTH=95%
		BORDERWIDTH=2PT;
	END;
run;


proc template;
    DEFINE STYLE PANDA1;
	PARENT= Styles.PANDA;

	style TABLE from table/
		cellpadding=0
		cellspacing=0
		rules=none
		BORDERTOPWIDTH=1PT
		;
	END;
run;


***** write table 1 to RTF using proc report*****;
ods listing close;
options nodate nonumber orientation=landscape missing='';
ods escapechar='^';
ods rtf file = "C:\bancova2016summer\data\derived\AE1.rtf" style=PANDA;
proc report data=ae11 nowindows missing headline headskip split='|';
	column vars Anticancer000 Anticancer001 Total;
	
	define vars/display 			style(header)=[just=l vjust=top asis=on]
									style(column)={just=l vjust=bottom asis=on cellwidth=30%}
									"Primary System Organ Class N(%) | Preferred Term";
	define Anticancer000/display 	style(header)=[just=c vjust=top asis=on]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Anticancer000 | N=&tt1.";
	define Anticancer001/display 	style(header)=[just=c vjust=top asis=on]
									style(column)=[just=c vjust=bottom asis=n cellwidth=8%]
									"Anticancer001 | N=&tt2.";
	define Total/display 			style(header)=[just=c vjust=top asis=on]
									style(column)=[just=c vjust=bottom asis=on cellwidth=6%]
									"Total | N=&tt3.";
	
	title1  h=8pt j=l "Bancova_Adverse" j=r "page^{pageof}";
	title2 "Table 3.1";
  	title3 "SUMMARY OF ADVERSE BY SYSTEM ORGAN CLASS AND PREFERRED TERM";
  	title4 "(SAFETY Population)";
	footnote1 "^R'\brdrb\brdrs\brdrw1";
	footnote2 "ae_yz.sas  submitted  &sysdate9. at  &systime by Yonghua Zhuang";
run ; 

%let text= %str(^S={just=l font=('courier new',8pt)}); 
	ods rtf text=' ';
	ods rtf text="&text Notes:";
	ods rtf text="&text A subject is counted only once within a prefered term and system organ class. Percents are based on the number of subjects in the safety population.";	

ods rtf close;
ods listing;

data ae2;
	set ae2;
	uniID=sum(soctID,ptID);
run;
proc sort data=ae2;
	by uniID;
run; 

data ae3;
	set ae3;
	uniID=sum(soctID,ptID);
run;
proc sort data=ae3;
	by uniID;
run; 

data ae4;
	merge	ae2
			ae3 (rename=(Mild=Mild1 Moderate=Moderate1 Severe=Severe1));
			by uniID;
	array nn{6} Mild Moderate Severe Mild1 Moderate1 Severe1;
	do i=1 to 6;
		if nn[i]='' then nn[i]= compress(0||'('||put(0, 4.1)||')');
	end;
	drop i;
run;

proc sort data=ae4;
	by soctID ptID;
run;

data ae5 (drop=soctID ptID uniID);
	do _n_=1 by 1 until(last.soctID);
		set ae4 end=last;
		by soctID notsorted;
		output;
	end;
	call missing(of _all_);
	if not last then output;
run;	


***** write table 2 to RTF using proc report*****;
ods listing close;
options nodate nonumber orientation=landscape missing='';
ods escapechar='^';
ods rtf file = "C:\bancova2016summer\data\derived\AE2.rtf" style=PANDA1;
proc report data=ae5 nowindows headline missing headskip split='|';
	column	(" ^S={borderbottomcolor=black borderbottomwidth=2} " vars)
			(" ^S={borderbottomcolor=black borderbottomwidth=2} Anticancer000 | N=&tt1." Mild Moderate Severe)
			(" ^S={borderbottomcolor=black borderbottomwidth=2} Anticancer001 | N=&tt2." Mild1 Moderate1 Severe1) ;

	define 	vars/display 			style(header)=[just=l vjust=top asis=on]
									style(column)={just=l vjust=bottom asis=on cellwidth=30%}
									" ";
	define Mild/display 			style(header)=[just=c vjust=top asis=on textdecoration=underline]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Mild";
	define Moderate/display 		style(header)=[just=c vjust=top asis=on textdecoration=underline]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Moderate";
	define Severe/display 			style(header)=[just=c vjust=top asis=on textdecoration=underline]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Severe";
	define Mild1/display 			style(header)=[just=c vjust=top asis=on textdecoration=underline]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Mild";
	define Moderate1/display 		style(header)=[just=c vjust=top asis=on textdecoration=underline]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Moderate";
	define Severe1/display 			style(header)=[just=c vjust=top asis=on textdecoration=underline]
									style(column)=[just=c vjust=bottom asis=on cellwidth=8%]
									"Severe";

	
	title1  h=8pt j=l "Bancova_Adverse" j=r "page^{pageof}";
	title2 "Table 3.2";
  	title3 "SUMMARY OF ADVERSE EVENTS BY SEVERITY";
  	title4 "(SAFETY Population)";
	footnote1 "^R'\brdrb\brdrs\brdrw1";
	footnote2 "ae_yz.sas  submitted  &sysdate9. at  &systime by Yonghua Zhuang";
run ; 

%let text= %str(^S={just=l font=('courier new',8pt)}); 
	ods rtf text=' ';
	ods rtf text="&text Notes:";
	ods rtf text="&text Subject is only counted once for a particular preferred term of a particular system organ class.  The most severe severity is chosen when there are multiple AEs with different severity for one subject.  If severity is missing, the conservative approach is taken – the severity imputed as “Severe”.";	

ods rtf close;
ods listing;

