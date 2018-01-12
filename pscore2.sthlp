{smcl}
{* *! version 1.2.0  02jun2011}{...}
{findalias asfradohelp}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "[R] help" "help help"}{...}
{viewerjumpto "Syntax" "examplehelpfile##syntax"}{...}
{viewerjumpto "Description" "examplehelpfile##description"}{...}
{viewerjumpto "Options" "examplehelpfile##options"}{...}
{viewerjumpto "Remarks" "examplehelpfile##remarks"}{...}
{viewerjumpto "Examples" "examplehelpfile##examples"}{...}
{title:Title}

{phang}
{bf:pscore2} {hline 2} A greedy search algorithm for balanced covariate strata  


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:pscore2}
[{it:externalname}]
{varlist}
{ifin}
{weight}{cmd:, blockid(}{it:string}{cmd:) pscore(}{it:string}{cmd:)}
[{opt ksm:irnov }{opt var:iance }{opt tenf:orce }{opt wi:lk }{opt med:ian }{opt supp:lied }{opt logit }{opt com:sup }{cmd:level(}{it:real 0.2}{cmd:)}
{opt re:scale }{cmd:by(}{it:real 0.5}{cmd:) }{opt rev:ert }{opt det:ail }{opt sum:mary }{opt qui:etly}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Compulsory}

   {cmd: blockid(}{it:newvar1}{cmd:)} 	   Variable name for the strata identifier created by {bf:pscore2}. 
   {cmd: pscore(}{it:newvar2}{cmd:)}  	   Variable name for the balancing score function as according to {it:newvar1}. 

{syntab:Statistical testing}

    {opt default  }          Compute two-sample t-test with equal variances for continuous and and rank-sum test for discrete variables.
    {opt ksm:irnov } 		 Compute two-sample Kolmogorov-Smirnov equality-of-distributions test.
    {opt var:iance } 		 Compute two-sample mean-comparison and variance-comparison test.
    {opt tenf:orce }             Compute two-sample mean-comparison test with equal variances for all conditioning variables.
    {opt wi:lk }                 Compute rank-sum test.
    {opt med:ian }               Compute nonparametric equality-of-medians test.


{syntab:Balancing score function}

    {opt supp:lied }		 Externally supplied balancing score function specified by {it:externalname}.
    {opt logit }		Estimate the balancing score function by logistic regression (default is probit).
    {opt com:sup }               Restrict analysis to the common support of treated and untreated observations. 
                          (Consider this for speeding-up computations.)


{syntab:Algorithm}

    {cmd:level(}{it:real}{cmd:)} 	   Specifiy the level for the cutoff-type-I error for the statistical test. If nothing 
                          is specified the default of 0.2 is used. s choice is subject to the user. 
    {opt re:scale }       	 Use rescaling by a constant scale factor to update the testing interval. If the  
                          {cmd:by( )} option is not specified a default value of 1/2 is used.                                    
    {cmd:by(}{it:real}{cmd:) }		   Has to be specified togehter with {opt re:scale}. Update testing interval
                          by a constant scale factor element of (0,1).  
    {opt rev:ert }	  	 Revert the direction of the grid search to search into the direction of the maximum.
    
{syntab:Display and summary options}

    {opt det:ail }		 Noisily display the estimation output of an internally estimated logit or probit model
 			  or display a detailed summary ouput of an externally supplied balancing score. 
    {opt sum:mary }	  	 If specified additional results are returned in e( ). If quitely is not specified 
                          the estimated p-values for each variable in varlist are displayed as estimation results.
    {opt qui:etly}        	 Has to be specified together with {opt sum:mary}. If specified additional results are only 
                          returned in e( ) but are not displayed in the estimation output.

{marker syntax}{...}
{title:Saved results}

   {cmd: pscore2} saves the following in e():
    
    Scalars
    
    e(converged)          1 if {it:newvar1} and {it:newvar2} are nonempty, 0 otherwise.
    e(shareN)             The share of observations satisfying covariate balance and classified 
                          by the levels of {it:newvar1} relative to all observations.
    e(classifiedN)        The number of observations satisfying covariate balance and classified 
                          by the levels of {it:newvar1}.  
    e(strata)             The number of estimated strata.

    Matrices

    e(tot) 		  The total numbers of treated and untreated observations calssified 
                          by the levels of {it:newvar1}.
    e(strataN)		  The strata-specific numbers of treated and untreated observations.
    e(scstrata)           The strata bounds in terms of the balancing score function.

    If {opt sum:mary} is specified
    
    e(fitness)            The minimum estimated p-values for each stratum.
    e(pvals)              The full set of estimated p-values for each variable in {varlist}
                          and each stratum.  


{synoptline}

