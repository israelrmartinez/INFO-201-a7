# a7-starter-repo

Starter repo for assignment 7. You should have **one repository** for
your team -- make sure to add all team members as **collaborators** to
the project.

# Overview

Almost all data-driven research projects involve multiple team members
working on the same code-base. In fact, they often involve people
working on the same documents at the same time, and running the same
code on different computers. 

The purpose of this assignment is to give you opportunity
to practice using these collaboration tools. Something will likely go
wrong, which is really the point. Following this assignment, you'll be
prepared to dive into your final project alongside your other team
members.


# Analyzing US Election Data

You have probably seen US election visualization, both at state level

![](https://upload.wikimedia.org/wikipedia/commons/4/44/ElectoralCollege2012.svg)

and at county level

![](https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/2016_Nationwide_US_presidential_county_map_shaded_by_vote_share.svg/1280px-2016_Nationwide_US_presidential_county_map_shaded_by_vote_share.svg.png)

Your task here is to analyze the few past elections and come up with
your own visualizations and other analyses.  The data is downloaded
from
[Tony McGovern's repo on GitHub](https://github.com/tonmcg/US_County_Level_Election_Results_08-16)
and merged with 
[census fips codes](https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt).

The dataset contains the following variables:

* _fips\_code_: county
  [fips code](https://en.wikipedia.org/wiki/FIPS_county_code)
* _county_: county name
* _total\_2008_: total votes cast in this county at 2008 presidential
  elections 
* _dem\_2008_: total votes for the democratic presidential candidate
* _gop\_2008_: total votes for the republican presidential candidate
* _oth\_2008_: total votes for other candidates
* similar entries for 2012 and 2016 elections
* _state_: two letter state code

Note: the data for Alaska are missing.

It is your task to pick interesting patterns and trends in the data,
and decide how to present it in a suitable way, we provide only basic
guidelines (see also for a few ideas below).  Replicating existing
work is fine (but you have to write code yourself!)
Feel free to add additional data, for instance you may try to
correlate election results, population and economic outcomes. 

Your final product is a report that must include:

* An introductory paragraph, telling what this report is about and
  where this data is coming from.
* A paragraph of summary information, citing at least 3 values
  calculated from the data.
* Two charts or tables (3 if you have 5 group members) that display information
  from the data (what you present is up to you).
* the final _index.html_ file should be uploaded to github.io through
  _gh-pages_ branch.

Unlike other assignments, you have to keep your code organized in multiple
different files. This helps keep your project more modular and
clear. You'll create four different files for this project:

* An index.Rmd file that renders your report
* A file with functions that calculate summary information to be included in your report
* A file with a function that returns your first chart or table
* A file with another function that returns another chart or table

You should save your .R files in a `R/` directory. There's more
information below about how these files interact.


# Instructions

## Set Up The Repo

As with other group assignments, one person (and only one person) will
begin by clicking the link that creates the GitHub repo.  The whole
group must work on this single repo. 


## Create GitHub Issues

In order to keep track of who needs to do what, GitHub has created the
issues tab. Typically, GitHub Issues are used by teams to assign
tasks and track progress on different parts of the project. You can
easily create and assign issues on GitHub's issues tab. For this
assignment, you should assign each of the components below to a
different group member, and keep track of your progress by closing the
issues once you've completed a task. You should close issues from the
command line when you make your commit messages.


## Work with Branches

Each team member has to create at least one branch, make edits there,
push the branch to github,
and merge it back to the master branch.  Everyone also has to make at
least one edit directly to the master branch.  It is your task to
agree on what to do with branches and what to do by directly
committing to the master.  (Normally larger changes are done in
branches and minor modifications directly to the master).  Everyone
also has to solve a conflict, either by mergin own branch or when
commiting into master branch.


## Report Components


As described above, you'll be creating at least four different files. Because
the purpose of this assignment is to practice collaboration, each
section should be completed by a different person. We'll be checking the commit
history to ensure that each section was pushed by a different
account. Here is additional information on each section:



### _index.rmd_ File

_index.rmd_ is the main product of this exercise.  It should be
self-contained, i.e. those who read it should understand what is it
about and how the data is processed (but not the details).  It should
also be addressed to general audience, those who are interested in
elections but not in computer code.  Hence do not show any code or messages in
the report.  Imagine you are working as a consultant providing reports
for a politcal party&mdash;your audience wants to learn about election results but not about
the underlying R code.

The index file should contain the following parts:

1. Initialize R: load libraries, you may want to load data here, set
   _knitr_ options, etc.  Ensure that the results will not be visible
   in the final report.
1. A brief introduction where you tell what you are doing.
2. A paragraph that contains the summary information, created by the
   summary-producing R script.  Include the numbers either inline or
   as separate paragraphs as appropriate.
3. Your first chart or table.  It should be created by the first
   analysis script; in the _index.rmd_ file you just print or plot it.
   
   It should be followed (or preceeded) by a paragraph that explains
   what the table shows, outlines
   the methods and summarizes the results.
4. Your second chart/table, processed and explained in the same way. 
5. If you are working in a group of 5, you have to produce one more
   chart/table. 

At the end, you put the knitted _index.html_ in the _gh-pages_ branch
so it will be visible on _github.io_.  However, the gh-pages branch
should contain _only_ index.html and no other files!  (You still retain
all your files in the master branch.)


### Summary information script

The first file you should save in your `R/` directory should
contain a function that returns
a list of information about it.  It is a good idea to pass the data as an
argument to this function.  For example:

```r
## A function that takes in a dataset and returns a list of info about it:
describeData <- function(dataset) {
   ret <- list()
   ret$length <- nrow(dataset)  # number of counties we have data for
   ## do some more interesting stuff here
   ## add more components to 'ret',
   ## ...
   return(ret)
}
```

We don't expect you to write a generalizable function that detects
rows/columns and asks questions of your dataset. Instead, think of
this like a simple analysis you would do on this particular dataset,
just wrapped in a function. This will allow you to pass your dataset
into this function from your index.Rmd file.


### Analysis File(s)

In your other .R files you should write functions that
create visualizations/tables of that
data and return these to _index.rmd_.  In this way you can include these into the _index.rmd_ file by
just calling the function at the appropriate place.  This keeps your
markdown much more readable.  You have to create a separate R script
for each analysis.

#### A few ideas:

You are free to come up with any kind of relevant analyses but here I
list a few ideas.

I recommend you to experiment with maps.  The data is at county level,
you can get the US county boundaries by `map_data("county")` using
_ggplot_ library.

You may also consider to connect election data with macro-level data,
such as population or income.  There are many sources where you can
get US geographic data at county level (and even more at state level), you may consider
dataset `county` in the _openintro_ package.

You may also analyze the vote swings over elections, and visualize how
the
counties (or states) have become more democratic or republican.  Can
you show that this is related to, say, population size? 

In terms of tables, you may show party preferences in, say, 10 largest
US metro areas, or in otherwise interesting cities.

### Submission

As with the previous assignment, you should add and commit your
changes using git, and push your assignment to GitHub. You will submit
the URL of your repository as your assignment. Only one person will
need to submit, because this is configured as a group project.


## Expectations

At this point in the quarter, we expect you to be following the best
practices we've incorporated into the class. This means:

* Proper use of libraries such as _dplyr_ for data manipulation
* Structuring your code so that if the data changes, you can easily update your entire report
* Leveraging markdown syntax to provide structure to your report
* Clearly commenting and properly organizing your code
* Writing functions to encapsulate chunks of code that you use more than once
* Avoiding variables that are unnecessary for your analysis/report
* Creating appropriate labels for your visualizations


## Grading

As you see, grading is heavily tilted toward git usage.

* Proper use of git and GitHub: **25pt**
    * created and assigned issues **3**
	* closed issues though commit messages **3**
	* created branches **2**
	* branches uploaded to github **2**
	* merged branches **5**
	* solved conflicts **4**
	* informative commit messages **3**
	* only _index.html_ on gh-pages **3**
* Proper use of rmarkdown **6pt**
    * markdown text appropriately structured, including title, author
      names, section headers **4**
	* code chunks not overly long, most code in separate files **2**
* Components of the report **7pt**
    * introduction **1**
	* summary text (values seamlessly integrated in text) **2**
	* first (and following charts/tables) **2**
* code **12pt**
    * code in appropriately named files in a folder named _R_ **2**
    * appropriate functions that input the dataset and output charts
      and tables **4**	
    * function return structures are appropriate (a list or something
      similar for summary, graphs/tables for the other functions) **4**
	* charts, tables appropriately labeled, colored, etc.
