/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 11:53:59 AM
PROJECT: WadlowJ_SAS_project_13JAN15
PROJECT PATH: P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\jwadlow\Assignments' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\jwadlow\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\jwadlow\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:01 AM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.FULLYR_2012_SUBSET);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.FULLYR_2012_SUBSET(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.FULLYR_2012_SUBSET
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.FULLYR_2012_SUBSET OUT=WORK.FULLYR_2012_SUBSET;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.FULLYR_2012_SUBSET
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: xray and mri variables filtered age   */
%LET _CLIENTTASKLABEL='xray and mri variables filtered age';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_Variable_Filter_Results);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_Variable_Filter_Results(label="MEPS_Variable_Filter_Results") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.MARRY12X, 
          t1.EDRECODE
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS Program Code   */
%LET _CLIENTTASKLABEL='SAS Program Code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';
%LET _SASPROGRAMFILE='\\Client\P$\QAC\qac200\students\jwadlow\Assignments\SAS Program Code\SAS Program Code.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 10:53:37 AM
   By task: Data Set Attributes1

   Input Data: Local:MYDATA.MEPS_VARIABLE_FILTER_RESULTS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForMEPS_VARIABLE_FIL);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MEPS_VARIABLE_FILTER_RESULTS;

RUN;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 Adults Getting MRIs vs X-Rays Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 Adults Getting MRIs vs X-Rays Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:02 AM
   By task: One-Way Frequencies for 2012 Adults Getting MRIs vs X-Rays Subset

   Input Data: Local:MYDATA.MEPS_VARIABLE_FILTER_RESULTS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_VARIABLE_FILTER_RESULTS
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SEX, T.REGION12, T.WAGEP12X, T.VETSP12X, T.USBORN42, T.UNEMP12X, T.TTLP12X, T.TRSTP12X, T.STRKDX, T.SSIP12X, T.SSECP12X, T.SFFLAG42, T.SAQWT12F, T.SAQELIG, T.SALEP12X, T.REFDP12X, T.RACETHX, T.PUBP12X, T.PHQ242, T.PENSP12X
		     , T.PCS42, T.OTHRP12X, T.OHRTDX, T.MIDX, T.MIAGED, T.MCS42, T.K6SUM42, T.IRASP12X, T.IPDIS12, T.INTRP12X, T.HIBPDX, T.ERTOT12, T.ENGSPK42, T.EMPST53, T.EMPST42, T.EMPST31, T.EMPHDX, T.EDUYRDEG, T.DVTOT12, T.DVGEN12, T.DNUNAB42
		     , T.DIVDP12X, T.DIABDX, T.CHOLDX, T.CHLDP12X, T.CHDDX, T.CANCERDX, T.BUSNP12X, T.BMINDX53, T.ASTHDX, T.ARTHDX, T.ANGIDX, T.ALIMP12X, T.AGE12X, T.ADWRTH42, T.ADTLHW42, T.ADSPRF42, T.ADSPEC42, T.ADSOCA42, T.ADSMOK42, T.ADSAD42
		     , T.ADRTWW42, T.ADRTCR42, T.ADRISK42, T.ADREST42, T.ADRESP42, T.ADPWLM42, T.ADPRX42, T.ADPRTM42, T.ADPALS42, T.ADPAIN42, T.ADOVER42, T.ADNSMK42, T.ADNRGY42, T.ADNERV42, T.ADNDCR42, T.ADMWLM42, T.ADMALS42, T.ADLIST42
		     , T.ADLANG42, T.ADINTR42, T.ADINST42, T.ADINSB42, T.ADINSA42, T.ADILWW42, T.ADILCR42, T.ADHOPE42, T.ADHECR42, T.ADHDADDX, T.ADGENH42, T.ADFHLP42, T.ADFFRM42, T.ADEZUN42, T.ADEXPL42, T.ADEGMC42, T.ADEFRT42, T.ADDRBP42, T.ADDPRS42
		     , T.ADDOWN42, T.ADDAYA42, T.ADCMPY42, T.ADCMPM42, T.ADCMPD42, T.ADCLIM42, T.ADCAPE42, T.ADAPPT42
	FROM MYDATA.MEPS_VARIABLE_FILTER_RESULTS(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for analysis of MRI vs X-Rays for 2012 Adults";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES WAGEP12X / MISSPRINT  SCORES=TABLE;
	TABLES VETSP12X / MISSPRINT  SCORES=TABLE;
	TABLES USBORN42 / MISSPRINT  SCORES=TABLE;
	TABLES UNEMP12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES TRSTP12X / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES SSIP12X / MISSPRINT  SCORES=TABLE;
	TABLES SSECP12X / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES SALEP12X / MISSPRINT  SCORES=TABLE;
	TABLES REFDP12X / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES PUBP12X / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES PENSP12X / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES OTHRP12X / MISSPRINT  SCORES=TABLE;
	TABLES OHRTDX / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES MIAGED / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES IRASP12X / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES INTRP12X / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES ENGSPK42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPHDX / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES DVTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES DVGEN12 / MISSPRINT  SCORES=TABLE;
	TABLES DNUNAB42 / MISSPRINT  SCORES=TABLE;
	TABLES DIVDP12X / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES CHOLDX / MISSPRINT  SCORES=TABLE;
	TABLES CHLDP12X / MISSPRINT  SCORES=TABLE;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES BUSNP12X / MISSPRINT  SCORES=TABLE;
	TABLES BMINDX53 / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES ALIMP12X / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHDADDX / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_MEPS_VARIABLE_FILTER_R);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_MEPS_VARIABLE_FILTER_R(label="QUERY_FOR_MEPS_VARIABLE_FILTER_R") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          /* HEALTH_IN_GENERAL_ADGENH42 */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health Rated From 1 to 5" AS HEALTH_IN_GENERAL_ADGENH42, 
          /* HEALTH_LIMITS_ADDAYA42 */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="Modified Activities Rated 1 to 3" AS HEALTH_LIMITS_ADDAYA42, 
          /* LIMITED_CLIMBING_ADCLIM42 */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="Climbing stairs limited rated 1 to 3" AS LIMITED_CLIMBING_ADCLIM42, 
          /* ACCOMPLISH_LESS_PAIN_ADPALS42 */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="Pain hinders accomplishment" AS ACCOMPLISH_LESS_PAIN_ADPALS42, 
          /* WORK_LIMITED_ADPWLM42 */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="Physical problems limit work rated 1 to 5" AS WORK_LIMITED_ADPWLM42, 
          /* ACCOMPLISH_LESS_MENTAL_ADMALS42 */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="Accomplish less because of mental problems rated 1 to 5" AS ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          /* WORK_LIMIT_MENTAL_ADMWLM42 */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="Work limited by mental problems rated 1 to 5" AS WORK_LIMIT_MENTAL_ADMWLM42, 
          /* WORK_LIMIT_PAIN_ADPAIN42 */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="Work limited by pain rated 1 to 5" AS WORK_LIMIT_PAIN_ADPAIN42, 
          /* FELT_CALM_ADCAPE42 */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="Felt calm and peaceful rated 1 to 5" AS FELT_CALM_ADCAPE42, 
          /* HAD_ENERGY_ADNRGY42 */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="Had energy rated 1 to 5" AS HAD_ENERGY_ADNRGY42, 
          /* FELT_DOWN_ADDOWN42 */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="Felt down rated 1 to 5" AS FELT_DOWN_ADDOWN42, 
          /* HEALTH_STOPPED_SOCIAL_ADSOCA42 */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="Health stopped social life rated 1 to 5" AS HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          /* EMPLOY_MARCH1_EMPST31 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment status on March 1" AS EMPLOY_MARCH1_EMPST31, 
          /* EMPLOY_APRIL2_EMPST42 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="Employment status on April 2" AS EMPLOY_APRIL2_EMPST42, 
          /* EMPLOY_MAY3_EMPST53 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="Employment status on May 3" AS EMPLOY_MAY3_EMPST53, 
          /* VISITS_TO_MED_ADAPPT42 */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="Visits to medical office for care" AS VISITS_TO_MED_ADAPPT42, 
          /* DEPRESSED_ADDPRS42 */
            (CASE 
               WHEN -1 = t1.ADDPRS42 THEN .
               WHEN -7 = t1.ADDPRS42 THEN .
               WHEN -8 = t1.ADDPRS42 THEN .
               WHEN -9 = t1.ADDPRS42 THEN .
               ELSE t1.ADDPRS42
            END) LABEL="Felt Depressed in 2 weeks" AS DEPRESSED_ADDPRS42, 
          /* DR_CHK_BLOOD_PRESS_ADDPBP42 */
            (CASE 
               WHEN -1 = t1.ADDRBP42 THEN .
               WHEN -8 = t1.ADDRBP42 THEN .
               WHEN -9 = t1.ADDRBP42 THEN .
               ELSE t1.ADDRBP42
            END) LABEL="Doctor checked blood pressure" AS DR_CHK_BLOOD_PRESS_ADDPBP42, 
          /* EVERYTHING_EFFORT_ADEFRT42 */
            (CASE 
               WHEN -1 = t1.ADEFRT42 THEN .
               WHEN -7 = t1.ADEFRT42 THEN .
               WHEN -8 = t1.ADEFRT42 THEN .
               WHEN -9 = t1.ADEFRT42 THEN .
               ELSE t1.ADEFRT42
            END) LABEL="Everything an effort in last 30 days" AS EVERYTHING_EFFORT_ADEFRT42, 
          /* EASY_CARE_ADEGMC42 */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting medical care" AS EASY_CARE_ADEGMC42, 
          /* DOC_EXPLAINED_ADEXPL42 */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doctor Explained so Understood" AS DOC_EXPLAINED_ADEXPL42, 
          /* DOC_INSTRUCTION_ADEXUN42 */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="Doctor gave easy to understand instructions" AS DOC_INSTRUCTION_ADEXUN42, 
          /* FORMS_ADFFRM42 */
            (CASE 
               WHEN -1 = t1.ADFFRM42 THEN .
               WHEN -8 = t1.ADFFRM42 THEN .
               WHEN -9 = t1.ADFFRM42 THEN .
               ELSE t1.ADFFRM42
            END) LABEL="Had to fill out and sign forms" AS FORMS_ADFFRM42, 
          /* HELP_FORMS_ADFHLP42 */
            (CASE 
               WHEN -1 = t1.ADFHLP42 THEN .
               WHEN -8 = t1.ADFHLP42 THEN .
               WHEN -9 = t1.ADFHLP42 THEN .
               ELSE t1.ADFHLP42
            END) LABEL="Got help filling forms" AS HELP_FORMS_ADFHLP42, 
          /* RATED_CARE_ADHECR42 */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="Health care rated 1 to 10" AS RATED_CARE_ADHECR42, 
          /* HEART_ATTACK_MIDX */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="Heart attack diagnosis" AS HEART_ATTACK_MIDX, 
          /* ADDADHD_ADHDADDX */
            (CASE 
               WHEN -1 = t1.ADHDADDX THEN .
               WHEN -8 = t1.ADHDADDX THEN .
               WHEN -9 = t1.ADHDADDX THEN .
               ELSE t1.ADHDADDX
            END) LABEL="ADD or ADHD Diagnosis" AS ADDADHD_ADHDADDX, 
          /* STROKE_STRKDX */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="Stroke Diagnosis" AS STROKE_STRKDX, 
          /* OTHER_HEART_OHRTDX */
            (CASE 
               WHEN -7 = t1.OHRTDX THEN .
               WHEN -8 = t1.OHRTDX THEN .
               WHEN -9 = t1.OHRTDX THEN .
               ELSE t1.OHRTDX
            END) LABEL="Other heart diagnosis" AS OTHER_HEART_OHRTDX, 
          /* AGE_HEART_DIAG_MIAGED */
            (CASE 
               WHEN -1 = t1.MIAGED THEN .
               WHEN -7 = t1.MIAGED THEN .
               WHEN -8 = t1.MIAGED THEN .
               ELSE t1.MIAGED
            END) LABEL="Age at heart diagnosis" AS AGE_HEART_DIAG_MIAGED, 
          /* BLOOD_PRESS_HIBPDX */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="High blood pressure diagnosis" AS BLOOD_PRESS_HIBPDX, 
          /* EMPHYSEMA_DIAG_EMPHDX */
            (CASE 
               WHEN -7 = t1.EMPHDX THEN .
               WHEN -8 = t1.EMPHDX THEN .
               WHEN -9 = t1.EMPHDX THEN .
               ELSE t1.EMPHDX
            END) LABEL="Emphysema diagnosis" AS EMPHYSEMA_DIAG_EMPHDX, 
          /* MARRIED_RECODE_MARRY12X */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Married Recode" AS MARRIED_RECODE_MARRY12X, 
          /* EDUCATION_LEVEL_RECODE_EDURECODE */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education Level Recoded" AS EDUCATION_LEVEL_RECODE_EDURECODE
      FROM MYDATA.MEPS_VARIABLE_FILTER_RESULTS t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Code SF-12 Variables   */
%LET _CLIENTTASKLABEL='Reverse Code SF-12 Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED(label="QUERY_MEPS_VAR_FILTER_REVERSED") AS 
   SELECT t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          /* ADPAIN_RECODED_REVERSED */
            (6-t1.WORK_LIMIT_PAIN_ADPAIN42) LABEL="ADPAIN Recoded and Reversed" AS ADPAIN_RECODED_REVERSED, 
          /* ADCAPE42_RECODED_REVERSED */
            (6-t1.FELT_CALM_ADCAPE42) LABEL="ADCAPE42 Recoded and Reversed" AS ADCAPE42_RECODED_REVERSED, 
          /* ADNRGY42_RECODED_REVERSED */
            (6-t1.HAD_ENERGY_ADNRGY42) LABEL="ADNRGY42 Recoded and Reversed" AS ADNRGY42_RECODED_REVERSED, 
          /* ADGENH42_RECODED_REVERSED */
            (6-t1.HEALTH_IN_GENERAL_ADGENH42) LABEL="ADGENH42 Recoded and Reversed" AS ADGENH42_RECODED_REVERSED, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH42 AS HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA42 AS HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM42 AS LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42 AS ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM42 AS WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42 AS ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42 AS WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN42 AS WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE42 AS FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY42 AS HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN42 AS FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42 AS HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE
      FROM MYDATA.QUERY_FOR_MEPS_VARIABLE_FILTER_R t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis of Reversed Recodes   */
%LET _CLIENTTASKLABEL='Table Analysis of Reversed Recodes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:02 AM
   By task: Table Analysis of Reversed Recodes

   Input Data: Local:MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_IN_GENERAL_ADGENH42, T.FELT_CALM_ADCAPE42, T.HAD_ENERGY_ADNRGY42, T.ADGENH42_RECODED_REVERSED, T.ADNRGY42_RECODED_REVERSED, T.ADCAPE42_RECODED_REVERSED, T.WORK_LIMIT_PAIN_ADPAIN42, T.ADPAIN_RECODED_REVERSED
	FROM MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN_RECODED_REVERSED * WORK_LIMIT_PAIN_ADPAIN42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42_RECODED_REVERSED * FELT_CALM_ADCAPE42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADGENH42_RECODED_REVERSED * HEALTH_IN_GENERAL_ADGENH42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42_RECODED_REVERSED * HAD_ENERGY_ADNRGY42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MEPS Variables Sum   */
%LET _CLIENTTASKLABEL='MEPS Variables Sum';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM(label="QUERY_FOR_MEPS_VAR_FILTER_SUM") AS 
   SELECT t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          /* SUM_SF12_VARIABLES */
            
            (SUM(t1.ADGENH42_RECODED_REVERSED,t1.ADNRGY42_RECODED_REVERSED,t1.ADCAPE42_RECODED_REVERSED,t1.ADPAIN_RECODED_REVERSED,t1.HEALTH_STOPPED_SOCIAL_ADSOCA42,t1.FELT_DOWN_ADDOWN42,t1.WORK_LIMIT_MENTAL_ADMWLM42,t1.ACCOMPLISH_LESS_MENTAL_ADMALS42,t1.WORK_LIMITED_ADPWLM42,t1.ACCOMPLISH_LESS_PAIN_ADPALS42,t1.LIMITED_CLIMBING_ADCLIM42,t1.HEALTH_LIMITS_ADDAYA42)) 
            LABEL="Sum of SF-12 Variables" AS SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH42 AS HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA42 AS HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM42 AS LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42 AS ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM42 AS WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42 AS ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42 AS WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN42 AS WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE42 AS FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY42 AS HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN42 AS FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42 AS HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED AS ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED AS ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED AS ADNRGY42_RECODED_REVERSED1, 
          t1.ADGENH42_RECODED_REVERSED AS ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH421 AS HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA421 AS HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM421 AS LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421 AS ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM421 AS WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421 AS ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421 AS WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN421 AS WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE421 AS FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY421 AS HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN421 AS FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421 AS HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE
      FROM MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Check Aggregate Variables Coding   */
%LET _CLIENTTASKLABEL='Check Aggregate Variables Coding';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:03 AM
   By task: Check Aggregate Variables Coding

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_LIMITS_ADDAYA42, T.LIMITED_CLIMBING_ADCLIM42, T.ACCOMPLISH_LESS_PAIN_ADPALS42, T.WORK_LIMITED_ADPWLM42, T.ACCOMPLISH_LESS_MENTAL_ADMALS42, T.WORK_LIMIT_MENTAL_ADMWLM42, T.FELT_DOWN_ADDOWN42
		     , T.HEALTH_STOPPED_SOCIAL_ADSOCA42, T.ADPAIN_RECODED_REVERSED, T.ADCAPE42_RECODED_REVERSED, T.ADNRGY42_RECODED_REVERSED, T.ADGENH42_RECODED_REVERSED, T.SUM_SF12_VARIABLES
	FROM MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variables Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR HEALTH_LIMITS_ADDAYA42 LIMITED_CLIMBING_ADCLIM42 ACCOMPLISH_LESS_PAIN_ADPALS42 WORK_LIMITED_ADPWLM42 ACCOMPLISH_LESS_MENTAL_ADMALS42 WORK_LIMIT_MENTAL_ADMWLM42 FELT_DOWN_ADDOWN42 HEALTH_STOPPED_SOCIAL_ADSOCA42 ADPAIN_RECODED_REVERSED
	  ADCAPE42_RECODED_REVERSED ADNRGY42_RECODED_REVERSED ADGENH42_RECODED_REVERSED SUM_SF12_VARIABLES;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for SF-12 Variables   */
%LET _CLIENTTASKLABEL='Summary Statistics for SF-12 Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:03 AM
   By task: Summary Statistics for SF-12 Variables

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12_VARIABLES
	FROM MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics for SF-12 Variable Scores";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE NONOBS 	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_SF12_VARIABLES;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of SF-12 Variables   */
%LET _CLIENTTASKLABEL='Distribution Analysis of SF-12 Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:04 AM
   By task: Distribution Analysis of SF-12 Variables

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_SF12_VARIABLES
	FROM MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SF-12 Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_SF12_VARIABLES;
	HISTOGRAM   SUM_SF12_VARIABLES / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Health Score Categorical Filter   */
%LET _CLIENTTASKLABEL='Health Score Categorical Filter';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_VAR_FILTER_SUM);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_VAR_FILTER_SUM AS 
   SELECT /* HEALTH_SCORE_CATEGORICAL */
            (CASE  
               WHEN t1.SUM_SF12_VARIABLES >=2 and t1.SUM_SF12_VARIABLES <41
               THEN 1
               WHEN t1.SUM_SF12_VARIABLES >=41 and t1.SUM_SF12_VARIABLES <48
               THEN 2
               WHEN t1.SUM_SF12_VARIABLES >=48 and t1.SUM_SF12_VARIABLES < 52
               THEN 3
               ELSE 4
            END) LABEL="Health Scores Categorized" AS HEALTH_SCORE_CATEGORICAL, 
          t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED1, 
          t1.ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE
      FROM MYDATA.QUERY_FOR_MEPS_VAR_FILTER_SUM t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:05 AM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_MEPS_VAR_FILTER_SUM
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_VAR_FILTER_SUM
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HEALTH_SCORE_CATEGORICAL, T.SUM_SF12_VARIABLES
	FROM WORK.QUERY_FOR_MEPS_VAR_FILTER_SUM as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis of Health Scores vs Categorized Health Scores";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_SF12_VARIABLES * HEALTH_SCORE_CATEGORICAL /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: In Full Year   */
%LET _CLIENTTASKLABEL='In Full Year';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.INFULLYR);

PROC SQL;
   CREATE TABLE MYDATA.INFULLYR(label="INFULLYR") AS 
   SELECT t1.HEALTH_SCORE_CATEGORICAL, 
          t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED1, 
          t1.ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          /* INFULLYR */
            (1) AS INFULLYR
      FROM WORK.QUERY_FOR_MEPS_VAR_FILTER_SUM t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for Marriage   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Marriage';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:05 AM
   By task: One-Way Frequencies for Marriage

   Input Data: Local:MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MARRIED_RECODE_MARRY12X
	FROM MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MARRIED_RECODE_MARRY12X /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Marriage Categorical   */
%LET _CLIENTTASKLABEL='Marriage Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_MEPS_VAR_FILTER_);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_MEPS_VAR_FILTER_ AS 
   SELECT t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          /* MARRY12X_CATEGORICAL */
            (CASE  
               WHEN t1.MARRIED_RECODE_MARRY12X = 1
               THEN 1
               WHEN t1.MARRIED_RECODE_MARRY12X = 5
               THEN 2
               ELSE 3
            END) LABEL="Married Categorical" AS MARRY12X_CATEGORICAL
      FROM MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Married vs Categorical   */
%LET _CLIENTTASKLABEL='Married vs Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:06 AM
   By task: Married vs Categorical

   Input Data: Local:WORK.QUERY_FOR_QUERY_MEPS_VAR_FILTER_
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_MEPS_VAR_FILTER_
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRIED_RECODE_MARRY12X, T.MARRY12X_CATEGORICAL
	FROM WORK.QUERY_FOR_QUERY_MEPS_VAR_FILTER_ as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis of Married vs Married Categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRIED_RECODE_MARRY12X * MARRY12X_CATEGORICAL /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies for Education   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for Education';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:06 AM
   By task: One-Way Frequencies for Education

   Input Data: Local:MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.EDUCATION_LEVEL_RECODE_EDURECODE
	FROM MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES EDUCATION_LEVEL_RECODE_EDURECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Education Categorical   */
%LET _CLIENTTASKLABEL='Education Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_QUERY_MEPS_VAR_FI_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_QUERY_MEPS_VAR_FI_0000 AS 
   SELECT t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          /* EDUCATION_CATEGORICAL */
            (CASE  
               WHEN t1.EDUCATION_LEVEL_RECODE_EDURECODE <=12
               THEN 1
               WHEN t1.EDUCATION_LEVEL_RECODE_EDURECODE =13
               THEN 2
               WHEN t1.EDUCATION_LEVEL_RECODE_EDURECODE =14
               THEN 3
               WHEN t1.EDUCATION_LEVEL_RECODE_EDURECODE =15
               THEN 3
               ELSE 4
            END) LABEL="Education Categorical" AS EDUCATION_CATEGORICAL
      FROM MYDATA.QUERY_MEPS_VAR_FILTER_REVERSED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Education vs Categorical   */
%LET _CLIENTTASKLABEL='Education vs Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:06 AM
   By task: Education vs Categorical

   Input Data: Local:WORK.QUERY_FOR_QUERY_MEPS_VAR_FI_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_QUERY_MEPS_VAR_FI_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCATION_LEVEL_RECODE_EDURECODE, T.EDUCATION_CATEGORICAL
	FROM WORK.QUERY_FOR_QUERY_MEPS_VAR_FI_0000 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis for Education Level vs Categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUCATION_LEVEL_RECODE_EDURECODE * EDUCATION_CATEGORICAL /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Diagnosis Query Builder for Recode   */
%LET _CLIENTTASKLABEL='Diagnosis Query Builder for Recode';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.DIAGNOSIS_QUERY_BUILDER);

PROC SQL;
   CREATE TABLE MYDATA.DIAGNOSIS_QUERY_BUILDER(label="DIAGNOSIS_QUERY_BUILDER") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          /* ANGIDX_RECODE */
            (CASE 
               WHEN 2 = t1.ANGIDX THEN 0
               WHEN -7 = t1.ANGIDX THEN .
               WHEN -8 = t1.ANGIDX THEN .
               WHEN -9 = t1.ANGIDX THEN .
               ELSE t1.ANGIDX
            END) LABEL="ANGINA DIAGNOSIS RECODE" AS ANGIDX_RECODE, 
          /* ARTHDX_RECODE */
            (CASE 
               WHEN 2 = t1.ARTHDX THEN 0
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="ARTHRITIS DIAGNOSIS RECODE" AS ARTHDX_RECODE, 
          /* ASTHDX_RECODE */
            (CASE 
               WHEN 2 = t1.ASTHDX THEN 0
               WHEN -7 = t1.ASTHDX THEN .
               WHEN -8 = t1.ASTHDX THEN .
               WHEN -9 = t1.ASTHDX THEN .
               ELSE t1.ASTHDX
            END) LABEL="ASTHMA DIAGNOSIS RECODE" AS ASTHDX_RECODE, 
          /* DIABDX_RECODE */
            (CASE 
               WHEN 2 = t1.DIABDX THEN 0
               WHEN -7 = t1.DIABDX THEN .
               WHEN -8 = t1.DIABDX THEN .
               WHEN -9 = t1.DIABDX THEN .
               ELSE t1.DIABDX
            END) LABEL="DIABETES DIAGNOSIS RECODE" AS DIABDX_RECODE, 
          /* HIBPDX_RECODE */
            (CASE 
               WHEN 2 = t1.HIBPDX THEN 0
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="HIGH BLOOD PRESSURE RECODE" AS HIBPDX_RECODE
      FROM MYDATA.MEPS_VARIABLE_FILTER_RESULTS t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Diagnosis Recode Analysis   */
%LET _CLIENTTASKLABEL='Diagnosis Recode Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:07 AM
   By task: Diagnosis Recode Analysis

   Input Data: Local:MYDATA.DIAGNOSIS_QUERY_BUILDER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.DIAGNOSIS_QUERY_BUILDER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.HIBPDX, T.DIABDX, T.ASTHDX, T.ARTHDX, T.ANGIDX, T.ANGIDX_RECODE, T.ARTHDX_RECODE, T.ASTHDX_RECODE, T.DIABDX_RECODE, T.HIBPDX_RECODE
	FROM MYDATA.DIAGNOSIS_QUERY_BUILDER(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis of Diagnosis vs Recodes";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ANGIDX_RECODE * ANGIDX /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ARTHDX_RECODE * ARTHDX /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ASTHDX_RECODE * ASTHDX /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES DIABDX_RECODE * DIABDX /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HIBPDX_RECODE * HIBPDX /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Diagnosis Sum   */
%LET _CLIENTTASKLABEL='Diagnosis Sum';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.ANGIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.ASTHDX_RECODE, 
          t1.DIABDX_RECODE, 
          t1.HIBPDX_RECODE, 
          /* SUM_DIAGNOSIS */
            (SUM(t1.ANGIDX_RECODE,t1.ARTHDX_RECODE,t1.ASTHDX_RECODE,t1.DIABDX_RECODE,t1.HIBPDX_RECODE)) LABEL=
            "Total Diagnosis" AS SUM_DIAGNOSIS
      FROM MYDATA.DIAGNOSIS_QUERY_BUILDER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis of Diagnosis   */
%LET _CLIENTTASKLABEL='Table Analysis of Diagnosis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:07 AM
   By task: Table Analysis of Diagnosis

   Input Data: Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ANGIDX_RECODE, T.ARTHDX_RECODE, T.ASTHDX_RECODE, T.DIABDX_RECODE, T.HIBPDX_RECODE, T.SUM_DIAGNOSIS
	FROM WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variables Diagnosis Sum";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ANGIDX_RECODE ARTHDX_RECODE ASTHDX_RECODE DIABDX_RECODE HIBPDX_RECODE SUM_DIAGNOSIS;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics of Diagnosis   */
%LET _CLIENTTASKLABEL='Summary Statistics of Diagnosis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:07 AM
   By task: Summary Statistics of Diagnosis

   Input Data: Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_DIAGNOSIS
	FROM WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_DIAGNOSIS;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR SUM_DIAGNOSIS;

			HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of Total Diagnosis   */
%LET _CLIENTTASKLABEL='Distribution Analysis of Total Diagnosis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:08 AM
   By task: Distribution Analysis of Total Diagnosis

   Input Data: Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_DIAGNOSIS
	FROM WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Total diagnosis";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_DIAGNOSIS;
	HISTOGRAM   SUM_DIAGNOSIS / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:08 AM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.SUM_DIAGNOSIS
	FROM WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES SUM_DIAGNOSIS /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Diagnosis Categorical   */
%LET _CLIENTTASKLABEL='Diagnosis Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_DIAGNOSIS_QUERY_B_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_DIAGNOSIS_QUERY_B_0000 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.MARRY12X, 
          t1.EDRECODE, 
          t1.ANGIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.ASTHDX_RECODE, 
          t1.DIABDX_RECODE, 
          t1.HIBPDX_RECODE, 
          t1.SUM_DIAGNOSIS, 
          /* DIAGNOSIS_CATEGORICAL */
            (CASE  
               WHEN t1.SUM_DIAGNOSIS =0
               THEN 0
               WHEN t1.SUM_DIAGNOSIS =1
               THEN 1
               WHEN t1.SUM_DIAGNOSIS =2
               THEN 2
               ELSE 3
            END) LABEL="CATEGORICAL DIAGNOSIS" AS DIAGNOSIS_CATEGORICAL
      FROM WORK.QUERY_FOR_DIAGNOSIS_QUERY_BUILDE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:09 AM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_B_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_DIAGNOSIS_QUERY_B_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DIAGNOSIS_CATEGORICAL, T.SUM_DIAGNOSIS
	FROM WORK.QUERY_FOR_DIAGNOSIS_QUERY_B_0000 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis of Diagnosis vs Categorical Diagnosis";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_DIAGNOSIS * DIAGNOSIS_CATEGORICAL /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: In ER   */
LIBNAME EC100015 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='In ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.INER);

PROC SQL;
   CREATE TABLE MYDATA.INER(label="INER") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* INER */
            (1) AS INER
      FROM EC100015.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Full Join   */
%LET _CLIENTTASKLABEL='Full Join';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.FULL_JOIN);

PROC SQL;
   CREATE TABLE MYDATA.FULL_JOIN(label="FULL_JOIN") AS 
   SELECT t1.HEALTH_SCORE_CATEGORICAL, 
          t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED1, 
          t1.ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          t1.INFULLYR
      FROM MYDATA.INFULLYR t1
           FULL JOIN MYDATA.INER t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t2.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:09 AM
   By task: List Data

   Input Data: Local:MYDATA.FULL_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.SUBSTempTableSubsetInputTable);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.FULL_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER
	FROM MYDATA.FULL_JOIN as T
;
QUIT;

DATA WORK.SUBSTempTableSubsetInputTable / VIEW=WORK.SUBSTempTableSubsetInputTable;
	SET WORK.SORTTempTableSorted;
	IF MOD(_N_, 100) = 0 THEN
		OUTPUT;
RUN;

TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SUBSTempTableSubsetInputTable
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID DUPERSID1 INER;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		WORK.SUBSTempTableSubsetInputTable);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes for Full Join   */
%LET _CLIENTTASKLABEL='Data Set Attributes for Full Join';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:09 AM
   By task: Data Set Attributes for Full Join

   Input Data: Local:MYDATA.FULL_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForFULL_JOIN);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.FULL_JOIN OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForFULL_JOIN(LABEL="Contents Details for FULL_JOIN");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForFULL_JOIN
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='FULL_JOIN';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForFULL_JOIN OUT=WORK.CONTContentsForFULL_JOIN;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForFULL_JOIN
		WHERE memname='FULL_JOIN';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_FULL_JOIN);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_FULL_JOIN AS 
   SELECT t1.HEALTH_SCORE_CATEGORICAL, 
          t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED1, 
          t1.ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          t1.INFULLYR, 
          /* MRI_RECODE */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="MRI Recode" AS MRI_RECODE, 
          /* XRAYS_RECODE */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="Xrays Recode" AS XRAYS_RECODE
      FROM MYDATA.FULL_JOIN t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies of ER for MRI   */
%LET _CLIENTTASKLABEL='One-Way Frequencies of ER for MRI';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:09 AM
   By task: One-Way Frequencies of ER for MRI

   Input Data: Local:WORK.QUERY_FOR_FULL_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_FULL_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MRI_RECODE
	FROM WORK.QUERY_FOR_FULL_JOIN as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies of ER for Xrays   */
%LET _CLIENTTASKLABEL='One-Way Frequencies of ER for Xrays';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:10 AM
   By task: One-Way Frequencies of ER for Xrays

   Input Data: Local:WORK.QUERY_FOR_FULL_JOIN
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_FULL_JOIN
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_RECODE
	FROM WORK.QUERY_FOR_FULL_JOIN as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYS_RECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: DUPERSID_COUNT   */
%LET _CLIENTTASKLABEL='DUPERSID_COUNT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.DUPERSID_COUNT);

PROC SQL;
   CREATE TABLE MYDATA.DUPERSID_COUNT(label="DUPERSID_COUNT") AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.QUERY_FOR_FULL_JOIN t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: FULL_JOIN_DUPERSID   */
%LET _CLIENTTASKLABEL='FULL_JOIN_DUPERSID';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.FULL_JOIN_DUPERSID);

PROC SQL;
   CREATE TABLE MYDATA.FULL_JOIN_DUPERSID(label="FULL_JOIN_DUPERSID") AS 
   SELECT t1.HEALTH_SCORE_CATEGORICAL, 
          t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED1, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID, 
          t1.ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          t1.INFULLYR, 
          t1.MRI_RECODE, 
          t1.XRAYS_RECODE
      FROM WORK.QUERY_FOR_FULL_JOIN t1
           INNER JOIN MYDATA.DUPERSID_COUNT t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Frequency Table Count of DUPERSID   */
%LET _CLIENTTASKLABEL='Frequency Table Count of DUPERSID';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:10 AM
   By task: Frequency Table Count of DUPERSID

   Input Data: Local:MYDATA.FULL_JOIN_DUPERSID
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.FULL_JOIN_DUPERSID
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM MYDATA.FULL_JOIN_DUPERSID as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of Count of DUPERSID   */
%LET _CLIENTTASKLABEL='Distribution Analysis of Count of DUPERSID';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:11 AM
   By task: Distribution Analysis of Count of DUPERSID

   Input Data: Local:MYDATA.FULL_JOIN_DUPERSID
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.FULL_JOIN_DUPERSID
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM MYDATA.FULL_JOIN_DUPERSID as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: DUPERSID Categorical   */
%LET _CLIENTTASKLABEL='DUPERSID Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.DUPERSID_ER_CATEGORICAL);

PROC SQL;
   CREATE TABLE MYDATA.DUPERSID_ER_CATEGORICAL(label="DUPERSID_ER_CATEGORICAL") AS 
   SELECT /* DUPERSID_CATEGORICAL */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID =1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID =2
               THEN 2
               WHEN t1.COUNT_of_DUPERSID =3
               THEN 3
               ELSE 4
            END) LABEL="Individual Visits to ER Categorical" AS DUPERSID_CATEGORICAL, 
          t1.HEALTH_SCORE_CATEGORICAL, 
          t1.HEALTH_IN_GENERAL_ADGENH42, 
          t1.HEALTH_LIMITS_ADDAYA42, 
          t1.LIMITED_CLIMBING_ADCLIM42, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS42, 
          t1.WORK_LIMITED_ADPWLM42, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS42, 
          t1.WORK_LIMIT_MENTAL_ADMWLM42, 
          t1.WORK_LIMIT_PAIN_ADPAIN42, 
          t1.FELT_CALM_ADCAPE42, 
          t1.HAD_ENERGY_ADNRGY42, 
          t1.FELT_DOWN_ADDOWN42, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA42, 
          t1.ADPAIN_RECODED_REVERSED, 
          t1.ADCAPE42_RECODED_REVERSED, 
          t1.ADNRGY42_RECODED_REVERSED, 
          t1.ADGENH42_RECODED_REVERSED, 
          t1.SUM_SF12_VARIABLES, 
          t1.HEALTH_IN_GENERAL_ADGENH421, 
          t1.HEALTH_LIMITS_ADDAYA421, 
          t1.LIMITED_CLIMBING_ADCLIM421, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS421, 
          t1.WORK_LIMITED_ADPWLM421, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS421, 
          t1.WORK_LIMIT_MENTAL_ADMWLM421, 
          t1.WORK_LIMIT_PAIN_ADPAIN421, 
          t1.FELT_CALM_ADCAPE421, 
          t1.HAD_ENERGY_ADNRGY421, 
          t1.FELT_DOWN_ADDOWN421, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA421, 
          t1.ADPAIN_RECODED_REVERSED1, 
          t1.ADCAPE42_RECODED_REVERSED1, 
          t1.ADNRGY42_RECODED_REVERSED1, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          t1.ADGENH42_RECODED_REVERSED1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.EDUYRDEG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.PHQ242, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SFFLAG42, 
          t1.CANCERDX, 
          t1.BMINDX53, 
          t1.USBORN42, 
          t1.ENGSPK42, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.DIABDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DNUNAB42, 
          t1.EMPHDX, 
          t1.HIBPDX, 
          t1.MIAGED, 
          t1.OHRTDX, 
          t1.STRKDX, 
          t1.ADHDADDX, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.IPDIS12, 
          t1.MIDX, 
          t1.ALIMP12X, 
          t1.BUSNP12X, 
          t1.CHLDP12X, 
          t1.DIVDP12X, 
          t1.INTRP12X, 
          t1.IRASP12X, 
          t1.OTHRP12X, 
          t1.PENSP12X, 
          t1.PUBP12X, 
          t1.REFDP12X, 
          t1.SALEP12X, 
          t1.SSECP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.TRSTP12X, 
          t1.UNEMP12X, 
          t1.VETSP12X, 
          t1.WAGEP12X, 
          t1.SEX, 
          t1.HEALTH_IN_GENERAL_ADGENH4211, 
          t1.HEALTH_LIMITS_ADDAYA4211, 
          t1.LIMITED_CLIMBING_ADCLIM4211, 
          t1.ACCOMPLISH_LESS_PAIN_ADPALS4211, 
          t1.WORK_LIMITED_ADPWLM4211, 
          t1.ACCOMPLISH_LESS_MENTAL_ADMALS422, 
          t1.WORK_LIMIT_MENTAL_ADMWLM4211, 
          t1.WORK_LIMIT_PAIN_ADPAIN4211, 
          t1.FELT_CALM_ADCAPE4211, 
          t1.HAD_ENERGY_ADNRGY4211, 
          t1.FELT_DOWN_ADDOWN4211, 
          t1.HEALTH_STOPPED_SOCIAL_ADSOCA4211, 
          t1.EMPLOY_MARCH1_EMPST31, 
          t1.EMPLOY_APRIL2_EMPST42, 
          t1.EMPLOY_MAY3_EMPST53, 
          t1.VISITS_TO_MED_ADAPPT42, 
          t1.DEPRESSED_ADDPRS42, 
          t1.DR_CHK_BLOOD_PRESS_ADDPBP42, 
          t1.EVERYTHING_EFFORT_ADEFRT42, 
          t1.EASY_CARE_ADEGMC42, 
          t1.DOC_EXPLAINED_ADEXPL42, 
          t1.DOC_INSTRUCTION_ADEXUN42, 
          t1.FORMS_ADFFRM42, 
          t1.HELP_FORMS_ADFHLP42, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.RATED_CARE_ADHECR42, 
          t1.HEART_ATTACK_MIDX, 
          t1.ADDADHD_ADHDADDX, 
          t1.STROKE_STRKDX, 
          t1.OTHER_HEART_OHRTDX, 
          t1.AGE_HEART_DIAG_MIAGED, 
          t1.BLOOD_PRESS_HIBPDX, 
          t1.EMPHYSEMA_DIAG_EMPHDX, 
          t1.MARRIED_RECODE_MARRY12X, 
          t1.EDUCATION_LEVEL_RECODE_EDURECODE, 
          t1.INFULLYR, 
          t1.MRI_RECODE, 
          t1.XRAYS_RECODE
      FROM MYDATA.FULL_JOIN_DUPERSID t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Comparison for DUPERSID vs CATEGORICAL   */
%LET _CLIENTTASKLABEL='Table Comparison for DUPERSID vs CATEGORICAL';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:11 AM
   By task: Table Comparison for DUPERSID vs CATEGORICAL

   Input Data: Local:MYDATA.DUPERSID_ER_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.DUPERSID_ER_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.DUPERSID_CATEGORICAL
	FROM MYDATA.DUPERSID_ER_CATEGORICAL as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis of DUPERSID vs Categorical DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Joshua Wadlow";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID * DUPERSID_CATEGORICAL /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Frequencies of DUPERSID Categorical   */
%LET _CLIENTTASKLABEL='Frequencies of DUPERSID Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:12 AM
   By task: Frequencies of DUPERSID Categorical

   Input Data: Local:MYDATA.DUPERSID_ER_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.DUPERSID_ER_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DUPERSID_CATEGORICAL
	FROM MYDATA.DUPERSID_ER_CATEGORICAL as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DUPERSID_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jwadlow\Assignments\WadlowJ_SAS_project_13JAN15.egp';
%LET _CLIENTPROJECTNAME='WadlowJ_SAS_project_13JAN15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 11:53:12 AM
   By task: Distribution Analysis

   Input Data: Local:MYDATA.DUPERSID_ER_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.DUPERSID_ER_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID_CATEGORICAL
	FROM MYDATA.DUPERSID_ER_CATEGORICAL as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: DUPERSID_CATEGORICAL";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR DUPERSID_CATEGORICAL;
	HISTOGRAM   DUPERSID_CATEGORICAL / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
