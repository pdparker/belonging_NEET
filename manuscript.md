---
title             : "School Belonging Predicts whether an Emerging Adult will be Not in Education, Employment, or Training (NEET) after School"
shorttitle        : "School Belonging and NEET"
wordcount         : "6427"
keywords          : "school belonging; NEET; emerging adults; longitudinal"
abstract: |
  Youth with low school belonging or who are Not (being) in Employment, Education, or Training (NEET) are at the margins of critical social institutions. Yet little research has considered whether school belonging is a risk factor for later NEET. Using two longitudinal cohorts from Australia (*N* = 14,082; 51% Boys), we explore this relationship. Controlling for a range of individual and school level covariates, we find that school belonging at age 15 is a consistent and practically significant predictor of NEET status at ages 16-20. We found that this relationship is not the product of school belonging lowering the chances of students graduating high-school. Rather, school belonging had a unique impact beyond graduation. Given the costs of NEET, school belonging is of significant policy concern.
csl               : "apa7.csl"
documentclass     : "apa7"
output:
  papaja::apa6_pdf:
    includes:
      after_body: "appendix.tex"
    pandoc_args: ['--metadata-file=code/common-blinded.yaml']
    keep_md: yes
bibliography: code/references.bib
header-includes:
  - \usepackage{rotating}
  - \DeclareDelayedFloatFlavor{sidewaysfigure}{figure}
  - \hypersetup{colorlinks = false,pdfborder={1 1 1}}
  - \note{\clearpage}
---



The transition from compulsory schooling to further education or employment is one of the most critical, complex, and increasingly challenging of all developmental transitions [@dietrich2012]. Outcomes of this period have lifelong implications and thus transition success is crucial [@zarrett2006]. A transition of particular concern is those youth who do not transition into further employment, education, or training (NEET) after compulsory school. Youth who are NEET are at far greater lifetime risk of social exclusion with significant individual, social, and economic costs [@bynner2002]. As such, NEET remains a central concern for policy [@europeancommission.jointresearchcentre.2015; @woodhouse_2021]. Although considerable research has noted the institutional, social status, and academic achievement patterns of NEET youth [@bynner2002; @europeancommission.jointresearchcentre.2015], little research [c.f. @muir2015young] has considered the role of school belonging experiences. In this paper, we suggest that school belonging may be a critical precursor to NEET status.

Beyond academic success, school may provide one of the most critical pathways through which young people can fulfill their basic psychological need to belong [@osterman2000; @Baumeister2021]. A student's sense of school belonging has been defined as their sense of affiliation with their school and how accepted, respected, included, and supported they feel by others within their school environment [@goodenow1993]. Educational researchers and practitioners have recognized the critical importance of school belonging in predicting a range of essential educational outcomes [@allen2016; @korpershoek2019]. School belonging has also been found to be an important predictor of mental health and emotional wellbeing [@arslan2018; @parr2020; @zhang2018]. There is also evidence that school belonging is associated with the future aspirations of students post-schooling [@irvin2011] and may be one of the processes behind school drop-out [@sánchez2005; @slaten2016; @pisa].

A critical question remains: does school belonging---as one way in which youth are placed at the margins of critical social institutions---predict whether youth will remain on the margins of society as they leave school. A critically vulnerable group of youth are those who are NEET. And understanding NEET as part of wider social exclusion is needed [@thompson2011].

## School Belonging

A student's sense of school belonging draws heavily on the social connections they build at school including relationships with peers, teachers, and parents [@schoolb2017]. As such, social and emotional competencies are an important aspect of a student's sense of belonging at school [@allen2017]. Belonging is so important that it has been described as a basic psychological need [@Baumeister2021; @baumeister1995; @Leary2021] and a fundamental priority of schooling [@allen2017; @allen2017a; @allen2018]. Students who do not feel like they belong express feelings of alienation, isolation, and disaffection [@allen2017,p.14]. A lack of school belonging has been found to predict social exclusion [@arslan2020] and social exclusion at school may create a pattern of exclusion throughout life. However, it is important to note that school belonging is not limited to social relationships and can include the sense of belonging a student has to the educational institution itself and the complex interactions of the socio-ecology within a school system (e.g., such as policies, practices, school vision and mission) [@allen2016a; @rethinki2018].

There are many mechanisms by which school belonging might lead to non-participation in education, employment, and training in adulthood. Low school belonging during schooling is associated with increased emotional distress, physical violence (both as a perpetrator and a victim), increased prescription drug and other drug misuse, and STI diagnoses in adulthood [@steiner2019]: factors which have been found to reduce employment [@hammer1997]. Poor psychological health outcomes identified in adolescence, resulting in low school belonging, could lead young people to be marginalized in society [@allen2018; @sapiro2019]. Low school belonging is also associated with low academic motivation and academic stress, and patterns of educational dissatisfaction that may continue beyond compulsory secondary schooling [@allen2018; @abdollahi2020]. We outline some broader theories for the connection below.

## NEET

NEET is a well-studied but controversial concept in the social sciences. Its critics note that, as a group, NEET youth are heterogeneous and that many youths drift into and out of this category over time [@yates2006; @serracant2013; @holte2017]. However, NEET status remains a critical predictor of life-long social exclusion and a focal point of interventions aimed at increasing the life chances and lifelong attainment of young people [@bynner2012]. Put simply, although NEET categorization may miss important nuances in interventions for specific individuals, it remains a critical concept for social policy [e.g. @woodhouse_2021]. This is not only because of the individual concerns to health, security, wellbeing, and lifetime attainment but also to society in terms of lost productivity, welfare payments, and associations with crime [@bynner2012; @europeancommission.jointresearchcentre.2015]. In Australia, the estimated cost of NEET to the economy is \$16 billion annually [@woodhouse_2021]. At an individual level, even though NEET can be a transitory state, an instance of being NEET tends to increase long-term negative occupational outcomes and particularly unstable and tenuous employment over the life span [@ralston2016].

The link between negative school experiences and NEET has an established research literature [@muir2015young]. For the most part, this literature has focused on the outcomes of social exclusion (e.g., rejection and ostracism as well as educational outcomes like disengagement and absenteeism) or on narrow aspects of exclusion like bullying [@muir2015young]. Some qualitative research has noted that young people who are either NEET or at risk of being NEET report that their sense of exclusion from school involved both peers and teachers and a general sense that they did not belong [@muir2015young]. Yet large-scale research exploring both school belonging and NEET is absent despite the fact that these measures have strong theoretical links.

From a social science perspective, belonging and NEET can be seen as an outworking of social identity and its formation via theories such as identity capital [@bynner2002] and the politics of belonging [@halse2018theories]. Identity capital is the tangible (connections, ways of speaking, memberships of a group, and ways of dressing) and intangible (self-belief, self-regulatory skills, and other so-called '21st-Century skills') resources that provide access to opportunities [@côté1996]. Identity capital is what an individual invests in to develop a stable sense of self. The purpose of this identity investment is to help one navigate complex, constantly in flux, social systems and the links between them in order to smooth the path of their life course [@côté1996]. The politics of belonging is a framework for examining the ways in which social positions and identities are differentially valued and contested and the ways in which community boundaries are set in determining who can and cannot belong [@yuval-davis2006]. Put simply, identity capital is that which individuals invest in to develop their identity and the politics of belonging represents the contested ways in which both macro- and micro-contexts value those identities.

Low school belonging could thus be indicative of low identity capital, because school belonging may result from a sense of identity that facilitates establishing oneself within the school community. Low identity capital could likewise lead youth to be at risk of being NEET because a youths identity that was forced to competed in the 'identity market' of school is likewise forced to compete in the identity markets associated with the labor market and tertiary education institutions [@bynner2002; @côté1996].

The connection between school belonging and NEET could also be understood from the perspective of the politics of belonging [@halse2018theories]. In particular, different social positions (e.g., gender, ethnicity, and social class) and self or communally adopted identities are differentially valued by social institutions and this can lead to patterns of exclusion [@yuval-davis2006]. Some youth face both implicit (e.g., school personnel stereotypes about race, class, and gender) and explicit (e.g., race, class, and gender-based exclusionary policies) barriers to full membership in social institutes, including work and school [@macdonald2005; @yates2010; @brown1995]. Thus, school belonging and NEET are linked because they represent a pattern of being on the margins of critical social institutions. From this perspective, school belonging represents a primarily psychological outcome of social exclusion that is a precursor to wider institutional exclusion in adulthood (e.g., from the labor market). Thus, knowing that school belonging is a risk factor may help identify youth in need of increased support.

From the perspective of identity capital and identity politics, low school belonging may predict being NEET due to a range of mechanisms, both subtle and overt, that are related to contested identities. One overt mechanism is that a lack of belonging may result in a failure to graduate from high-school because a young person's experience of not belonging causes them to disengage from school: youth who do not feel like they belong are more likely to be absent, to engage in truant behavior, and to leave school early without a qualification [@sánchez2005; @korpershoek2019; @pisa]. All of these make entering further education, training, or employment more difficult. Thus, we seek to explore the role of high-school completion in explaining the link between school belonging and NEET status. We do this by exploring whether low school belonging predicts being NEET after controlling for high-school graduation. If (and only if) there is a notable drop in the predictive power of belonging after controlling for school completion, we will calculate the expected indirect effect of school completion as a mechanism accounting for the relationship between school belonging and NEET status.

Even outside high-school graduation there are reasons to believe that belonging is related to NEET. Youth who do not feel like they belong at school may graduate with only a weak connection to school that is only reinforced by parental pressure and government compulsory enrollment. This weak connection to school may lead to a weak connection to other social institutions where parents and government regulation exert a smaller influence and where individuals are required to compete for places in the labor market or further education. Taken together, there are good reasons to expect that school belonging and NEET are linked. However, it is also possible that these factors are linked via third variables such as low achievement, SES, or living in a regional community. For this reason, it is critical that our research controls for these factors.

## Critical Controls

In the current research, we use school belonging at age 15 to predict whether a youth will be NEET during ages 16 to 20. Although we use longitudinal data, this is unlikely to provide good controls for all sources of confounding. As such, we identified a number of baseline demographic, academic, and school context variables that have been shown in the literature to predict NEET status. We control for academic achievement, school context (school average achievement and socioeconomic status), cohort (participants aged 15 in 2003 vs 2015), gender, socioeconomic status (SES), place, and ethnicity.

*Academic achievement* appears to be the most predictive factor identified in research [@bynner2002]. Research also shows that *school context* is predictive [@europeancommission.jointresearchcentre.2015] of NEET with youth in poorer achieving and/or low SES schools more likely to be NEET. Labor market conditions and, in particular, youth unemployment is also a critical predictor [@europeancommission.jointresearchcentre.2015] and as such we compare a *cohort* of youth who experienced relatively low levels of youth unemployment (a cohort starting in 2003) to a cohort with moderately high levels of youth unemployment (a cohort starting in 2015). We also explored the moderation of results by cohort. In Australia---the context for the current study---the mandatory age for leaving school was increased to 17 years of age from 2008-2010 [@parker2019]. Internationally, raising the school leaving ages is a common policy change aimed at increasing high-school completion [@markussen2010]. Before 2008 some jurisdictions had school-leaving ages as low as 15. The explicit aim of such policies is typically to reduce rates of dropout and increase young people's chances of gaining access to full-time employment, training, or education post high-school [@markussen2010]. Thus, we pay particular attention to whether results are consistent across cohorts, not because we can disentangle the effect of labor market conditions or policy changes but because consistency across cohorts would speak to the generalizability of the associations detected. *Gender*, *SES*, *place* (urban versus rural), and *ethnicity* have all been shown to have relationships with being NEET [@europeancommission.jointresearchcentre.2015]. We will also take an exploratory perspective on the degree to which belonging is related to NEET differs for boys and girls, by SES, place, ethnicity, and for youth of different achievement levels.

## Current Research

Based on the available literature and theory, we advance the following research hypotheses:

-   *Hypothesis 1* Low school belonging at age 15 will predict NEET status at any time between ages 16 to 20 controlling for background demographics, academic achievement, and school characteristics.

-   *Hypothesis 2 (Exploratory hypothesis)* The relationship between school belonging and NEET status varies as a function of academic achievement, SES/parental social class, ethnicity, rural status, and/or gender.

-   *Hypothesis 3 (Exploratory hypothesis)* The relationship between school belong and NEET status generalizes across cohort (noting the potential role of changes in labor market conditions or changes to school-leaving age policy).

-   *Hypothesis 4* The relationship between school belonging and NEET status is at least partially explained by high-school graduation.

# Methods

## Participants

There was a total of 14,082 participants (48.52% female) across the 2003 and 2015 LSAY cohorts. Sample statistics can be found (broken down by whether the participant was *ever* NEET at any stage across the years of interest) in Table \@ref(tab:tab-descript).

Data were from two cohorts (2003 and 2015) of the Longitudinal Study of Australian Youth (LSAY). The LSAY cohort databases are a longitudinal extension of the Programme for International Student Assessment (PISA) which follows PISA participants yearly for 10 years. LSAY is the longitudinal extension of the Australian component of the Programme for International Student Assessment (PISA) samples in 2003 and 2015. PISA represents the first wave of the LSAY cohorts. Australian participants of PISA were given the option of signing up to LSAY voluntarily after completing the PISA tests and questionnaire. Unsurprisingly, many chose not to and this account for the smaller LSAY sample size from the PISA sample size. We defined the sample of interest as all those PISA participants that agreed to take part in LSAY and participated in the first LSAY wave of data collection. This represented about 50% of the PISA sample. We ran sensitivity analysis with the full PISA sample with missing values imputed and results were very similar. A data dictionary and full information on LSAY data collection methods can be found at the [LSAY website](http://lsay.edu.au/).

(ref:descriptives) Descriptive Statistics.

\begin{table}

\caption{(\#tab:tab-descript)(ref:descriptives)}
\centering
\begin{tabular}[t]{lccc}
\toprule
Characteristic & NEET, N = 1,323 & not NEET, N = 7,670 & p-value\\
\midrule
Girl &  &  & 0.4\\
\hspace{1em}Boy & 678 (51\%) & 4,026 (52\%) & \\
\hspace{1em}Girl & 645 (49\%) & 3,644 (48\%) & \\
Indigenous &  &  & <0.001\\
\hspace{1em}Indigenous & 134 (10\%) & 345 (4.5\%) & \\
\addlinespace
\hspace{1em}non-Indigenous & 1,189 (90\%) & 7,325 (96\%) & \\
Immigrant Background &  &  & 0.067\\
\hspace{1em}Immigrant & 300 (23\%) & 1,590 (21\%) & \\
\hspace{1em}non-Immigrnat & 990 (77\%) & 5,982 (79\%) & \\
\hspace{1em}Unknown & 33 & 98 & \\
\addlinespace
Urban &  &  & 0.2\\
\hspace{1em}Provincial & 557 (42\%) & 3,087 (40\%) & \\
\hspace{1em}Urban & 766 (58\%) & 4,583 (60\%) & \\
SES & 0.26 (-0.44, 0.84) & 0.47 (-0.11, 0.99) & <0.001\\
\hspace{1em}Unknown & 10 & 29 & \\
\addlinespace
Achievement & -0.06 (-0.88, 0.62) & 0.28 (-0.39, 0.87) & <0.001\\
School Avg Achievement & -0.06 (-0.41, 0.33) & 0.06 (-0.26, 0.39) & <0.001\\
School Avg. SES & 0.19 (-0.13, 0.57) & 0.38 (0.03, 0.65) & <0.001\\
\hspace{1em}Unknown & 0 & 1 & \\
\bottomrule
\multicolumn{4}{l}{\rule{0pt}{1em}\textsuperscript{1} n (\%); Median (IQR)}\\
\multicolumn{4}{l}{\rule{0pt}{1em}\textsuperscript{2} Pearson's Chi-squared test; Wilcoxon rank sum test}\\
\multicolumn{4}{l}{\textsuperscript{a} This table is based on the unimputed data. The data summarises scores for}\\
\multicolumn{4}{l}{participants who were NEET at any stage versus those who were never NEET.}\\
\end{tabular}
\end{table}

Participants did not have a fixed NEET state. Most participants were never NEET. Of those that were NEET most moved in and out of this status across the four years of interest (see Figure \@ref(fig:alluvial)).

(ref:alluvial-cap) NEET status changes across four years.

![(\#fig:alluvial)(ref:alluvial-cap)](manuscript_files/figure-latex/alluvial-1.pdf) 

## Measures

### School Belonging

School belonging was measured using the PISA scale of belonging at time wave 1. We used the survey organizers' composite score of belonging. The reliability for the scale was acceptable in both cohorts (Cohort 2003 $\alpha$ = .85 95% CI [.84 .85]; Cohort 2015 $\alpha$ = .85 95% CI [.84, .85]). The 6-item scale was measured with a 4-point Likert response with poles of 'strongly agree' and 'strongly disagree'. Example items include ("I feel like I belong" and "I feel awkward and out of place"). Parallel analysis by cohort suggested that a single component was sufficient to account for the variance in the items. The composite score the survey organizers created was valanced so that high scores equaled a stronger sense of school belonging. There were small differences in the instructions to participants and minor item wording differences for the 2003 and 2015 cohort (see Appendix A). These differences were so small we do not expect them to have any influence on the results. Nevertheless, we always a) control for cohort and b) explore whether cohort moderates the relationships of interest in this analysis.

### NEET Status

We defined NEET status as those youth that indicated at the time of testing they were a) not studying for any sort of tertiary qualification, b) were no longer in high-school, and c) were not in the labor market. This was measured in waves 2-5 using the derived variables from LSAY that ensured that NEET was measured consistently across waves.

### High-School Graduation

High-school graduation was measured using an LSAY derived variable coded as 0 if participants had not graduated from high-school and 1 if they had graduated from high-school. This was measured in waves 2-5.

### Individual Covariates {#individual-covariates}

*Achievement* was represented by taking the first principal component of the PISA math, reading, and science tests. The 2003 cohort had five plausible values for each achievement test. We took a principal component related to each set of plausible values and assigned each one to one of the imputed data sets (see [Analysis] section). The 2015 cohort had 10 plausible values per achievement test. We randomly selected five plausible value sets from this 10, took the first principal component for each set and assigned each one to one of the imputed datasets. *Socioeconomic status* (SES) was assessed using the PISA Economic, Social, and Cultural Status (ESCS) scale. The ESCS is an index of parents' years of schooling, parental occupation, and home and educational resources. *Place* was defined according to the Australia Bureau of Statistics assignment of the school postcodes to geographic categories the participant was enrolled in at the first time wave. These categories were then simplified to an urban/provincial binary. *Gender* was measured using participant self-report (for this reason we use the term gender rather than sex). *Ethnicity* was measured by participant self-report and had three categories: 1) Australian Indigenous, 2) local-born non-Indigenous, 3) first-generation immigrant (i.e., born outside Australia).

In the protocol we stated that we would run sensitivity analysis with social class (based on the parent with the highest social class job). Social class was represented by transforming the H-ISEI [Highest-International Socio-Economic Index of occupational status @ganzeboom1992] into Erikson-Goldthorpe-Portocarero codes with class classifications of Salariat, Intermediate, and Working. Results were almost identical using social class or socioeconomic status so we retain the latter results here (see the Appendix B for results from the main model using social class).

### School Level Covariates

School context was defined by school average achievement and school average SES. Both of these were formed by taking school aggregated means of the individual-level SES and achievement variables (see section [Individual Covariates](#individual-covariates)).

## Analysis

Data cleaning, manipulation, and plotting were conducted in R [@rcoreteam2020]. Due to the complexity of the models fit to the data, we ran all the multilevel models in Julia [@bezanson2017], a scientific programming language designed to provide fast computing times. To predict belonging we fit the following model:

$$
y_{belonging} \sim N(\alpha_{i[j]} + X_i\beta, \sigma^2_y), ~ for~i= 1,...,n
$$

$$
\alpha_j \sim N(U_j\gamma, \sigma^2_\alpha),~for~j = 1,...k
$$

Where $i$ is the individual participant in school $j$ and $X$ is a matrix of individual-level predictors (e.g., gender, SES, achievement). $U$ is a matrix of school-level predictors (i.e., school average achievement and SES.

To predict NEET status we fit a three-level logistic regression model with observations from age 16 to 20 nested within participants who were themselves nested within schools at age 15. This model was fit as <!--# Rewrite equation -->:

$$
ln(\frac{p}{1 - p}) \sim N(\alpha_{i[j[k]]} + X_i\beta,\sigma^2_y),~for~i = 1,...,n
$$

$$
\alpha_{j[k]} \sim N(\mu_{j[k]}+W_j\gamma_1, \sigma^2_{\alpha j[k]}),~for~j = 1,...m
$$

$$
\alpha_k \sim N(U_j\gamma_2, \sigma^2_{\alpha}),~for~j = 1,...k
$$

Where $p$ is the probability of being NEET and $X$ and $U$ remain the same as above with the exception that $X$ now also includes school belonging. $W$ includes only a single predictor for time wave. We ran the primary NEET models with both Bayes multilevel models and Generalized Estimating Equations (GEE) and the results were similar. As noted in [Deviations from Protocol] we chose to retain the maximum likelihood models to reduce the computational complexity for the several models we ran.

Attrition for the in-scope sample was fairly minimal. One option for accounting for the PISA attrition issue was to define the sample as those participants who agreed to participate in LSAY. However, we chose to retain the full sample and use multiple imputation to account for attrition in order to be maximally representative of the population. Given we had a mix of continuous and categorical variables we used a decision tree based missing data model via the MICE package [@buuren2011]. Five imputations were extracted with one plausible value for achievement assigned to each imputation. Given that attrition was only relevant for outcome variables that were modeled in long form and thus provide a natural form of full information modelling[^1] and missing data were 5% or less for the predictors, five imputations were deemed sufficient.

[^1]: One observation per participant per row of data (i.e., long form data) provides a form of full information analysis because as long as a participant has an observed outcome for at least one wave they are included in the model. This is particularly the case in the current research where the focus is on between-person comparisons rather than within-person causal systems.

## Deviations from Protocol

A redacted version of the protocol for this paper can be found in supplementary materials (the editorial team have access to the original date stamp protocol). There were several deviations from this protocol in this paper:

1.  *NEET Definition*. We received advice from the survey organizers on how to calculate NEET status from derived variables in LSAY when developing the protocol. However, on receiving the data we discovered that this definition (focused on tertiary enrollment and employment status) would classify youth still in high-school as NEET. Thus, our new definition of NEET made sure that participants still enrolled in high-school were not classified as NEET.
2.  *Maximum Likelihood Estimation*. We had originally planned to use Bayesian multilevel models (with weakly informative priors). However, running these models took 45 hours and resulted in effective sample sizes that were questionably small. To have full confidence in the results would require significantly increasing the number of iterations run and thus significantly increasing the required computing time. We thus decided to refit the models using maximum likelihood within Julia (given Julia's speed benefits). The results from both models were essentially identical. As were sensitivity models run using Generalized Estimating Equations (see Appendix C for results).
3.  *NEET Status across four-years*. A brand new release of LSAY 2015 that occurred after we submitted our protocol allowed us to add an additional wave of data to our analysis. This allowed us to capture youth aged up to 20 years of age.

# Results

## Predictors of Belonging (Exploratory Analysis)

(ref:belong-result) Predictors of School Belonging.


\begin{table}[tbp]

\begin{center}
\begin{threeparttable}

\caption{\label{tab:tab-belong}(ref:belong-result)}

\begin{tabular}{llll}
\toprule
Parameter & \multicolumn{1}{c}{Beta} & \multicolumn{1}{c}{-95\% CI} & \multicolumn{1}{c}{+95\% CI}\\
\midrule
Intercept & -0.10 & -0.14 & -0.06\\
Achievement (SD Units) & 0.00 & -0.02 & 0.02\\
Urban & 0.00 & -0.02 & 0.02\\
SES (SD Units) & 0.10 & 0.08 & 0.13\\
Gender & 0.12 & 0.09 & 0.15\\
Urban & 0.09 & 0.05 & 0.13\\
Indigenous & -0.02 & -0.08 & 0.05\\
Immigrant & 0.04 & 0.00 & 0.08\\
School Average SES (SD Units) & -0.01 & -0.07 & 0.05\\
School Average Achievement (Sd Units) & 0.04 & -0.02 & 0.09\\
Cohort (2015) & -0.20 & -0.24 & -0.16\\
\bottomrule
\end{tabular}

\end{threeparttable}
\end{center}

\end{table}

We first tested whether our demographic and academic achievement variables predicted feelings of school belonging. The results can be found in Table \@ref(tab:tab-belong). Girls, urban youth, Immigrants, and high SES youth had higher levels of belonging. Academic achievement was also a significant predictor but the effect size was not practically significant. Indeed, the demographic and achievement predictors all had relatively weak effect sizes.

We also tested whether these individual-level estimates differed by cohort. Comparing this multi-group model to a model that just controlled for cohort indicated that the latter was a significantly better fitting model (F (6, 69039.669) = 2.584, p = 0.017)[^2]. There were two significant differences by cohort for the influence of gender on belonging and for the influence of achievement on belonging. Exploring the marginal means for these significant interactions showed trivial differences (see Figure \@ref(fig:fig-belong)).

[^2]: Note the F-test here is a multiple imputation version of a chi-square log-likelihood ratio test [@meng1992].

(ref:belong-interaction) Cohort Differences in Predicting School Belonging.

![(\#fig:fig-belong)(ref:belong-interaction)](manuscript_files/figure-latex/fig-belong-1.pdf) 

## Predicting NEET Status

(ref:neet-result) Model Predicting NEET Status.


\begin{table}[tbp]

\begin{center}
\begin{threeparttable}

\caption{\label{tab:tab-neet}(ref:neet-result)}

\begin{tabular}{llll}
\toprule
Parameter & \multicolumn{1}{c}{Odds Ratio} & \multicolumn{1}{c}{-95\% CI} & \multicolumn{1}{c}{+95\% CI}\\
\midrule
Intercept & 0.02 & 0.02 & 0.03\\
School Belonging (SD Units) & 0.81 & 0.77 & 0.86\\
Time Wave (1-Year Units) & 1.22 & 1.17 & 1.28\\
Achievement (SD Units) & 0.72 & 0.68 & 0.77\\
Urban & 0.72 & 0.68 & 0.77\\
SES (SD Units) & 0.86 & 0.80 & 0.92\\
Gender & 1.20 & 1.09 & 1.31\\
Urban & 1.02 & 0.91 & 1.14\\
Indigenous & 1.78 & 1.52 & 2.07\\
Immigrant & 1.11 & 0.98 & 1.26\\
School Average SES (SD Units) & 0.69 & 0.58 & 0.82\\
School Average Achievement (Sd Units) & 1.20 & 1.03 & 1.40\\
Cohort (2015) & 1.35 & 1.21 & 1.52\\
Random Intercept: Individual & 1.18 &  & \\
Random Intercept: School & 0.15 &  & \\
\bottomrule
\addlinespace
\end{tabular}

\begin{tablenotes}[para]
\normalsize{\textit{Note.} Random intercepts are not in odds-ratio units.}
\end{tablenotes}

\end{threeparttable}
\end{center}

\end{table}

We next explored whether school belonging predicted NEET status controlling for a range of demographics, school context, and academic achievement predictors. Results can be found in Table \@ref(tab:tab-neet) with conditional means for belonging and other notable predictors in Figure \@ref(fig:fig-neet). It is first worth noting that the participants in the 2015 LSAY cohort were 1.5 times more likely to be NEET at some stage from ages 16-20. This is consistent with the lower youth unemployment rates from 2004-2007 than in 2016-2019 (ABS Cat. No. 6291.0.55.001; see Appendix G). High-levels of achievement and SES, and being female were all associated with a lower likelihood of being NEET, while Indigenous participants were almost two times more likely to be NEET than non-Indigenous participants. School context had large but unexpected effects. High-school average SES was a protective factor against NEET while high-school average achievement (controlling for individual achievement) appears to increase the chances of being NEET. This was almost entirely due to the multicollinearity between these two predictors: r = 0.72 [0.712,0.728]. This multicollinearity would be troubling if school context was a major focus. However, as proxies for school context, the inclusion of both school average SES and achievement allows for more precise estimates of the influence of school belonging on NEET status [@bollinger2015]. In the Appendix D and E we show that estimates for both school SES and achievement predicted lower likelihood of being NEET; though only school average SES was significant.

Notwithstanding a number of strong predictors of NEET status, our results show that school belonging at age 15 had a significant and strong protective influence. Participants who felt like they belonged at school at age 15 were less likely to be NEET from ages 16 to 20.

## Moderation of NEET Belonging Association

We explored whether the effect of belonging on NEET varied as a factor of achievement, gender, SES, place, Indigenous and immigrant status. There was no evidence that the results differed as a function of these variable (F (6, 5.67) = 0.22, p = 0.957). In addition, there was no evidence that the association between belonging and NEET varied as a function of cohort.

## High-school Graduation as Mechanism

Finally, we wanted to know whether school belonging had a unique effect on NEET status, or whether its effect was purely a mechanism of a lower likelihood to graduate high-school. In a final model we thus predicted NEET status as above but also controlled for high-school graduation (see Table \@ref(tab:tab-grad)). While high-school graduation was indeed a strong predictor of NEET status, school belonging was still a significant predictor. Indeed, it was not even a notably weaker predictor in this model. The coefficient for school belonging barely changed when introducing high-school graduation as a covariate and thus any indirect effects via mediation analysis would not be of practical significance and thus we did not estimate them. Figure \@ref(fig:fig-compare) shows that those that graduated high-school almost never became NEET. Yet, among those who did not graduate, belonging remained a clearly practically significant predictor of NEET status. Predictors of high-school graduation can be found in Appendix F.

(ref:neet-grad) Model Predicting NEET Status (Controlling for High-School Graduation).


\begin{table}[tbp]

\begin{center}
\begin{threeparttable}

\caption{\label{tab:tab-grad}(ref:neet-grad)}

\begin{tabular}{llll}
\toprule
Parameter & \multicolumn{1}{c}{Odds Ratio} & \multicolumn{1}{c}{-95\% CI} & \multicolumn{1}{c}{+95\% CI}\\
\midrule
Intercept & 0.03 & 0.02 & 0.03\\
High-School Graduate & 6.91 & 5.92 & 8.07\\
Time Wave (1-Year Units) & 0.74 & 0.69 & 0.80\\
Achievement (SD Units) & 0.64 & 0.60 & 0.69\\
Urban & 0.64 & 0.60 & 0.69\\
SES (SD Units) & 0.84 & 0.78 & 0.91\\
Gender & 1.18 & 1.06 & 1.30\\
Urban & 0.93 & 0.82 & 1.06\\
Indigenous & 1.76 & 1.47 & 2.09\\
Immigrant & 1.04 & 0.90 & 1.19\\
School Belonging (SD Units) & 0.80 & 0.75 & 0.84\\
Cohort (2015) & 1.16 & 1.02 & 1.32\\
School Average SES (SD Units) & 0.72 & 0.59 & 0.88\\
School Average Achievement (Sd Units) & 1.18 & 1.00 & 1.40\\
Random Intercept: Individual & 1.95 &  & \\
Random Intercept: School & 0.21 &  & \\
\bottomrule
\addlinespace
\end{tabular}

\begin{tablenotes}[para]
\normalsize{\textit{Note.} Random intercepts are not in odds-ratio units.}
\end{tablenotes}

\end{threeparttable}
\end{center}

\end{table}

(ref:neet-cap) Marginal Effects for Predicting NEET Status.

\begin{sidewaysfigure}
\includegraphics{manuscript_files/figure-latex/fig-neet-1} \caption{(ref:neet-cap)}(\#fig:fig-neet)
\end{sidewaysfigure}

(ref:belong-comparison) Marginal Effects for Predicting NEET Status.

\begin{sidewaysfigure}
\includegraphics{manuscript_files/figure-latex/fig-compare-1} \caption{(ref:belong-comparison)}(\#fig:fig-compare)
\end{sidewaysfigure}

# Discussion

We set out to test if school belonging was a risk factor for NEET status using two longitudinal cohorts of Australian youth. As anticipated, we found that school belonging was a practically and statistically significant predictor of NEET status and that this was the case despite controlling for a range of school and individual-level covariates. Results did not differ by cohort or across a range of social locations (gender, SES, ethnicity, and place). Further, high-school graduation did not appear to explain the link between school belonging and NEET. The marginal effect from a model that controlled for high-school graduation showed that amongst non-graduates, belonging was an important predictor of NEET status (almost all youth who graduated high-school did not experience NEET). The predicted probability of being NEET for a youth one standard deviations below the mean on school belonging was over one percentage points higher than a youth one standard deviations above the mean. For those who did not graduate high-school the predicted probability difference was approximately 2.5 percentage points (see Figure \@ref(fig:fig-compare)). In our study protocol (see supplementary materials), we suggested that an effect size of one percentage point would be considered meaningful in the context of NEET proportions in the population of interest.

Research has consistently shown that not graduating high-school is a significant risk factor for a range of outcomes including lifetime earnings, welfare requirements, health, and employability [@hollands2014]. Our results hint that high levels of school belonging among non-graduates could help mitigate these concerns; though clearly more research in this area is needed. At the very least, our results suggest that school personnel and, at a national level, policymakers, should monitor school belonging as one of a range of indicators of NEET risk.

## Predictors of School Belonging

Academic achievement was a significant predictor of school belonging in the current research but the practical significance of this association was trivial. Achievement has long been considered to have a reciprocal relationship with belonging, yet this likely varies by nation depending on the degree to which high achievement is socially valued [@oecd_2019]. The lack of relationship between achievement and belonging in this study---as also found in PISA reports on Australia [@oecd_2019]---may imply that, in Australia, a young person's sense of belonging is not contingent on their academic prowess. This may generalise to other Anglophone countries. Meta-analyses where most studies were from English speaking Western countries found only a small association between achievement and belonging [@korpershoek2019, [@allen2016]]. As we note below, academic achievement was a strong predictor of school graduation.

Demographic predictors were much more notable predictors of school belonging. Youth who were from urban contexts, girls, immigrants, and high SES youth all had small, but meaningful positive relationships with belonging. This is consistent with previous research [@europeancommission.jointresearchcentre.2015] and with previous work with PISA reports [@oecd_2019] that forms the first wave of the LSAY data. Our research showed that Indigenous status was a positive predictor of school belonging. This is important in countering deficit-orientated language about Indigenous youth and their connection to education [@craven2016]. Indeed, a focus on what drives Indigenous students to stay at school and feel a sense of belonging may have more applied impact towards addressing Indigenous educational disadvantage than a focus on reasons for non-attendance or early school drop out. While this is a positive story, we acknowledge that this result was present while controlling for other covariates including SES. Part of the disadvantage that indigenous students face in education is conveyed through a history of marginalization that has resulted in Indigenous Australians having lower, on average, SES. In total (i.e., when not controlling for other variables) Indigenous students have lower levels of school belonging [@debortoli_2018]. As such, these results should be interpreted with care, but should serve as an incentive to consider the role that belonging might play in helping Indigenous students flourish in school.

## School Belonging and NEET

Low school belonging was a consistent and reliable predictor of NEET status. Furthermore the marginal effect sizes were of practical significance. School belonging had a smaller association with NEET than academic achievement had with NEET. But belonging had a stronger relationship to NEET than did the traditional predictor of SES. Marginal effects suggested that school belonging was particularly important among those that did not complete high-school---largely because few youth that completed high-school became NEET[^3]. Indeed, for youth who did not complete high-school, those who strongly felt like they belonged (+2 SD above the mean) were five percentage points less likely to be NEET than those who strongly felt like they did not belong at school (-2 SD below the mean). Given that about 10% of Australian youth aged 15-24 are NEET, costing the economy over \$16 billion a year [@woodhouse_2021], this is a notable effect. Strikingly, high-school graduation was not a practically meaningful mechanism explaining the link between belonging and NEET. Thus other mechanisms such as broad experience of perception of social exclusion and other educational, mental health, and social costs of belonging [@steiner2019; @hammer1997; @allen2018; @abdollahi2020; @hayes2015] should be considered as key mechanisms in future research.

[^3]: Although this appears to imply moderation, this is not moderation. The reason for this is that logistic regression is linear in the predictors but not in the predicted probabilities. Hence marginal effect of belonging differ for high-school graduates and non-graduates.

Our research shows that school belonging may be a significant predictor of post-compulsory education social exclusion, and the lifetime of potential costs that NEET status has for mental health, earnings, and other attainment outcomes [@ralston2016]. As we noted in the introduction, school belonging may be a risk factor because it may signal that a particular youth lacks the identity capital required to navigate the complexity and fluidity of social institutions across the lifespan [@bynner2002]. This may, in part, be due to the politics of belonging where some identities are placed at the margins of society making it harder for such individuals to securely access social institutes [@macdonald2005; @yates2010; @brown1995].

The concern of our results from a policy perspective is that it raises the possibility of 'The Matthew Effect'. In simple terms, the Matthew effect suggests that advantage begets advantage while disadvantage begets disadvantage [@kerckhoff_1999; @ralston2016; @hillmert2011]. It recognizes that social inequality is affected by accumulated dis/advantage [@hillmert2011]. The link in our research between low school belonging and NEET, and empirical research linking NEET to a lifetime of disadvantage [@ralston2016], can be viewed as the accumulation of disadvantage by those raised on the fringes of society. Social science has often focused on marginalization along traditional axes of disadvantage: gender, social class, place, and ethnicity. In particular, a great deal of focus has been on social class [@kerckhoff1997]. Yet a focus on belonging encourages research and practice to also consider other aspects of identity---such as whether the identity is endorsed by the self and/or others as authentic [@taylor_1992]---that place youth on the margins. Our research encourages such a focus given that school belonging was relatively weakly predicted by traditional axes of disadvantage and that school belonging predicted NEET status controlling for such social locations. The role of identity, the politics of belonging, and the authentic self are meaningful avenues for future research on school belonging.

## Other Predictors of NEET

Outside of school belonging, predictors of NEET status followed established relationships. Youth who were low achieving, Indigenous, girls, and low SES were particularly at risk. School context also mattered with analysis in the Appendix D and E showing that attending a rich school had notable protective benefits after controlling for individual-level variables (although school average achievement had little influence). This may suggest a form of social closure in which the scarce opportunities available for youth who struggle in school go to those with connections (e.g., those who attended prestigious private schools) leaving those with few connections without a safety net [@gugushvili2017]. The juxtaposition between school average SES and school average achievement is of particular interest and further study devoted to this topic is needed.

## Limitations

Although the current paper has notable strengths (e.g., multiple cohorts of longitudinal data), there are limitations that readers should consider when interpreting its results. We noted in the discussion that belonging forces a spotlight not just on disadvantage due to social location but also on disadvantage related to other forms of self or group identity. Interestingly, the role of social locations in predicting school belonging appeared to be relatively weak. Yet, in our current research, we were not able to do full justice to an exploration of the role of social locations. As Yuval-Davis [-@yuval-davis2006, p.200] notes, social locations "even in their most stable format, are virtually never constructed along one power axis of difference, although official statistics--as well as identity politics--often tend to construct them in this way. This is why the intersectional approach to social locations is so crucially important." To capture a true intersectional perspective we would have needed to estimate very complex higher-order interactions which we simply did not have the power to address. Indeed, it may be questioned whether a truly intersectional approach can even be addressed in any quantitative research, thus highlighting the need for continual and integrated qualitative research. Nevertheless, future research may be able to take a more truly intersectional perspective by looking at the multiplicative rather than additive influences of social location variables (e.g., gender, social class, place, and ethnicity).

It should also be noted that we only explored outcomes up to the age of 20 as this was the only data we had access to. Traditionally, NEET research is concerned with the full period of age 15-24 years of age. Of particular interest, youth who remain or become NEET at age 24 will likely have considerably worse lifetime outcomes than those who are NEET at age 20. In addition, the loss in sample size going from the Australian PISA sample to the LSAY sample is a concern. Yet it is encouraging that results were largely similar when the sample was defined as the PISA sample or the LSAY sample.

Finally, while we aimed to control for a range of covariates at the individual and school level, there are likely other variables that we did not control for and that could have biased the results. As such, the results of the current research are most safely interpreted *comparatively* (i.e., what is the likely NEET status of a youth at age 16-20 who is high on school belonging at age 15 compared to a similar youth who is low on belonging) rather than *causally* (i.e., what is the causal effect of school belonging at age 15 on NEET status at age 16-20).

## Conclusion

School belonging is a form of marginalization from *within* a critical social institution. NEET status represents exclusion *from* entry into many major social institutions. If we can intervene while youth are still in a major social institution we may be able to help avoid a lifetime of marginalization and social exclusion. From this perspective, measures of school belonging present an important metric that teachers, schools, and policymakers can monitor to identify students that may be at risk and thus implement interventions that may help break a pattern of social exclusion.

\newpage

# References

```{=tex}
\begingroup
\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}
```
\endgroup
