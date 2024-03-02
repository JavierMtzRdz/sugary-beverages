# HW 2 -- Sugary beverages

## Preliminaries

In addition to writing the client report and _doing_ the analysis, we will also
practice good `git`/GitHub collaboration behaviours. So, submissions will be
submitted via pull requests and peer-evaluation will be done using PR Reviews.

One member of your group must fork this repo. Other members should fork that
version (not mine). You are also welcome to collaborate in a single repo, as
you choose.

> Read _all_ these instructions carefully before you do anything. 

If you mess stuff up, I can help you fix it. But the cost of my assistance is a
proportional deduction on your mark. So you are better off trying to figure out
how to fix it yourself.

### Best practices

* Write in language that client can easily understand. Avoid technical 
details. Use examples and figures to explain or illustrate.
* Do not put computer code or output anywhere.
* Explain key concepts and ideas in easy-to-understand language. Do not refer 
the client to references for explanations before giving your own.
* The captions of graphs and tables should be self-contained enough so that the 
meaning is clear without reading the body of the report.
* Use 11 or 12 point sizes.
* The main text of the report should include only the most essential tables 
and figures. 
* Put additional tables and figures in Appendix.
* Title and authors: Choose a reasonable title. Don't call it "Sugary Beverages"
or "Homework 2". 
* You must open your PR by midnight on the due date.

---

## Version 1 (first deadline)

In the first portion of your case report, your goal is to **propose**
methodology to examine / solve the client's questions of interest.

There are two mandatory files:

1. `./report/main.Rmd`/`./report/main.qmd` - The Rmarkdown/Quarto file that 
generates your report.
1. `./report/main.pdf` - The compiled pdf produced.

* You must store those (and only those) in the `./report` directory.
* You may not store _any_ files in the root or your team's repo (`./`)
* Any additional files must be in sub-directories that are nicely laid out. 
I suggest you use sub-directories carefully, especially for long scripts
(put these in, e.g. `./code/01-exploration.R` rather than inside `main.Rmd`).
* Call your branch for this portion `v1.0.0`. You will submit by opening a PR
against the `main` branch.
* DO NOT DO ANY WORK ON THE `main` BRANCH. 

### Expectations

Write a client report summarizing your comments and proposal for the case. This
portion is at most 3 pages (excluding any carefully chosen tables or figure), or
approximately 1000-1500 words. 

1. You must submit your Part 1 via a Pull Request against `main` (from the 
`v1.0.0` branch). That means that you do your individual work directly there, or,
MUCH better, via pull requests into `v1.0.0` from your forks or other branches.
1. You must send the `.pdf` to Estella  by the Part 1 deadline. This will allow 
you to incorporate the changes for Part 2.

Though not required, I suggest you begin your analysis (on a different branch).
For Part 2, you must complete it (see below). 

### Format

1. **Introduction** Describe the background of the scientific problem, and clearly 
state the objective(s) of the study. State the statistical questions the client 
wants to answer.
1. **Data description and summaries** How the data were collected? Is it an 
observational study or experiment? What is the sample size? What variables are 
measured and how they are measured (e.g., continuous data or categorical data, 
units of measurements)? Any missing data?
1. **Exploratory analysis** Suggest appropriate tables and figures for summarizing
the data, such as boxplots or scatterplots.
1. **Formal analysis** Suggest formal statistical models and methods to confirm 
the pre- liminary conclusions from exploratory analysis, such as regression 
models. Keep mathematical equations at minimum.
1. **Conclusions** Summarize your recommendations to the clients.
1. **References** References cited in your report and appropriate references your 
client can understand for further reading. These _must_ be formatted properly. I
suggest using BibTeX or following the instructions 
[here](https://quarto.org/docs/manuscripts/authoring/jupyterlab.html#citations).
1. **Statistical Appendix** You can put mathematical formulas here. _NO CODE_. 
You can also put additional tables/figures here. This _must_ still read like a
document. Organize it into subsections as necessary (possibly following the 
same instructions as above).

### Grade, 10 points

- clarity, conciseness, paragraph organization: 2 pts
- grammatical mistakes: 2 pts
- statistical depth, quality of the analysis: 3 pts
- description of the analysis (general audience): 3 pts

---

## Version 2 (second deadline)

The goal of this part is two fold (1) incorporate the suggestions from Me, the
peer-reviews, and Estella into your 3 page Summary, (2) complete the analysis
and add a Statistical Appendix.

The Appendix may be at most 5 pages. Choose judiciously. Size figures reasonably.

An additional, final page, should be used to list the names of group members, 
with a concise description of the contributions each group member makes to the
assignment.

### Expectations

* Call your branch for this portion `v2.0.0`. You will submit by opening a PR
against the `main` branch.

1. The structure of your initial 3 page client report should remain the same
(and still be less than 3 pages).
1. You should adjust the client report in light of the, now complete,
statistical analysis.
1. **(NEW)** You should add a *Statistical Appendix* detailing your analysis.
The target audience is statisticians (you and your classmates). Describe your
analysis carefully using statistical language and undertaking any EDA,
diagnostics, model checks, that are necessary. Carefully describe issues with
data cleaning. If you do something Bayesian, describe how you know your MCMC
converged. Etc. The page limit for this component is 5 pages. Think of this
as a roadmap carefully describing what you did and why. If you needed to
return to this project in the future, this part tells you, more precisely,
what you did.

### Grade, 10 pts

- statistical depth, quality of the analysis: 4 pts
- description of the analysis, graphical and numerical summaries (statistical
  audience):  3 pts
- incorporation of previous feedback: 3 pts

---

## Peer evaluation

* Please be as detailed as possible. 
* Use the PR review functionality to its fullest.
* Comment on individual lines as well as providing overall assessments.
* "Approve" the PR to indicate that you are done.
* Don't just echo the comments of the other reviewers.
* You may also comment on organization, code in other files, formatting, etc.
* Please consider the following criteria for peer evaluation: 
  1. If you were a non-statistican client, can you understand the report easily?
  1. Are you convinced by the author’s explanations? 
  1. Is the writing clear and easy to follow? Any writing problems or typos or grammar errors? 
  
### Grade, 5 points

* You will be scored based on the above criteria. 
* Only the _best_ evaluators are likely to receive full marks.
