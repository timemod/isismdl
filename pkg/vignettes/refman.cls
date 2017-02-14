% Class file for Isis Reference manual
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{refman}[23/03/2005 Isis Reference Manual]
\LoadClass[10pt,fleqn,a4paper,twoside,soberchapter,spacedtoc,a4margin]{xreport}
%
% We are using the soberchapter option to avoid the text Chapter before chapter numbers.
% That is not needed in the manual.
% A side effect is however that Appendix will never be prefixed to an appendix "letter".
%
% So redefine the macro \appendix to the same definition of the cpbrep.cls style
% as used in the User Manual.
% If you change the soberchapter option you will also have to redefine this macro.
%
\renewcommand{\appendix}{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \newcommand{\app@chapter}{\appendixname~}%
  \newcommand{\app@section}{}%
  \newcommand{\app@subsection}{}%
  \newcommand{\app@subsubsection}{}%
  \newcommand{\app@paragraph}{}%
  \newcommand{\app@subparagraph}{}%
  \renewcommand{\thechapter}{\Alph{chapter}}%
\def\@seccntformat##1{\csname app@##1\endcsname\csname the##1\endcsname\hspace{1em}}%
}
%
\SetUnitIndent{MM}
% Input any local configuration file.
%
\InputIfFileExists{refman.cfg}{%
   \ClassInfo{refman}{Local configuration file used.}%
}{%
   \ClassInfo{refman}{Local configuration file not found.}%
}
%
%
% For standard indenting
%
\newlength{\UnitIndent}
%
% pick up standard unitindent from class
%
\setlength{\UnitIndent}{\unit@indent}
%
\RequirePackage{graphicx}
\RequirePackage{pdfpages}
\RequirePackage{array,fancyvrb,dcolumn,shortvrb,multicol}
\PassOptionsToPackage{round}{natbib}
\RequirePackage{natbib}
%
%
% Define a new columntype to suppress space at the left/right side of a table
%
\newcolumntype{z}{@{}}
%
% Define a columntype for inserting flexible space in between columns
% so that a table can be filled out to a required size
%
\newcolumntype{g}{@{\extracolsep{\fill}}}
%
% define control sequence for a minus sign in a table for negative numbers
%
\newcommand{\Tmin}{\textminus\hspace*{0.111em}}
%
%
\usepackage{makeidx}
\usepackage{needspace}
\RequirePackage[dutch,english]{babel}
\RequirePackage{fancyhdr}
\RequirePackage{needspace}
%\nofiles
\RequirePackage{datetime}
%
\newdateformat{refmandate}{\monthname[\THEMONTH] \THEYEAR}
%
\RequirePackage{color}
%
% colors and links for a paper version
%
\newif\if@destused
\@destusedfalse
\newif\if@topaper
\@topapertrue
%
\newcommand{\@paperdest}{%
\definecolor{tgdcolor}{gray}{0}
\definecolor{argcolor}{gray}{0}
\definecolor{refgray}{gray}{0.80}
\definecolor{brickred}{gray}{0}
\definecolor{blueish}{gray}{0}
\definecolor{outbg}{gray}{.93}%previous value .85 quite dark
\@destusedtrue
}
%
% colors and links for a screen version
%
\newcommand{\@screendest}{%
\definecolor{tgdcolor}{rgb}{0,0,1}
\definecolor{argcolor}{rgb}{0,0.6,0}
\definecolor{softyellow}{rgb}{1.0,1.0,0.96}
%%%\definecolor{refgray}{rgb}{0.95,0.95,0.95}
\definecolor{refgray}{rgb}{1.0,1.0,0.85}
\definecolor{brickred}{cmyk}{0,0.89,0.94,0.28}
\definecolor{blueish}{cmyk}{0.94,0.11,0,0}
\definecolor{outbg}{rgb}{0.94,1,0.87}
\@destusedtrue
\@topaperfalse
}
%
\DeclareOption{paper}{%
\@paperdest
\PassOptionsToPackage{pdftex,hypertexnames=false,hidelinks,hyperindex=false,colorlinks=false,pdfstartview={FitH 1000}}{hyperref}%
}
\DeclareOption{screen}{%
\@screendest
\PassOptionsToPackage{pdftex,hypertexnames=false,hyperindex=true,colorlinks=true,pdfstartview={FitH 1000}}{hyperref}%
}
%
\ProcessOptions
\if@destused
\else
\ClassError{refman}{No [paper] or [screen] option specified}{Specify paper or screen in option section}
\fi
%
\RequirePackage{hyperref}%
\frenchspacing
\raggedbottom
%
% table of contents
%
\setcounter{tocdepth}{1}
%
% Newline before every chapter entry
%
\def\l@chapter{\ifnum\c@tocdepth>\m@ne
\addpenalty{-\@highpenalty}\addvspace{\baselineskip}\fi
\@ftocline{0}}
%
% heading fonts
%
\chapterfont{\normalsize\large\sffamily\bfseries}
\sectionfont{\normalsize\large\sffamily\bfseries}
%%%%%%%%%\sectionfont{\normalsize\sffamily\bfseries}
\subsectionfont{\normalsize\sffamily\bfseries}
\subsubsectionfont{\normalsize\sffamily\bfseries}
%
% outdented heading number levels
%
\newdimen\sec@indent
%% CPB housestyle \setlength{\sec@maxboxwd}{1.6cm}
\setlength{\sec@maxboxwd}{1.0cm}
\setlength{\sec@indent}{0pt}
\def\@seccntformat#1{\hbox to 0pt{\hskip-\sec@maxboxwd\csname the#1\endcsname\hfil}\hss}
\def\chapter{\if@openright\cleardoublepage\else\clearpage\fi
\thispagestyle{plain}%
\global\@topnum\z@
\@afterindentfalse
\@startsection{chapter}{0}{\sec@indent}{-\baselineskip}{\baselineskip}{\chapter@font}}
\renewcommand{\section}{\@startsection {section}{1}{\sec@indent}%
{-\baselineskip}{\baselineskip}{\section@font}}
\renewcommand{\subsection}{\@startsection{subsection}{2}{\sec@indent}%
{-\baselineskip}{.1\baselineskip}{\subsection@font}}
\renewcommand{\subsubsection}{\@startsection{subsubsection}{3}{\sec@indent}%
{-\baselineskip}{.1\baselineskip}{\subsubsection@font}}
\renewcommand{\paragraph}{\@startsection{paragraph}{4}{\z@}%
  {3.25ex \@plus 1ex \@minus .2ex}{-1em}{\paragraph@font}}
\renewcommand{\subparagraph}{\@startsection{subparagraph}{5}{\parindent}%
  {3.25ex \@plus 1ex \@minus .2ex}{-1em}{\subparagraph@font}}
%
%
% to make all long rules have the same distance from baseline
%
\newcommand{\Gttdepth}{\makebox[0pt]{{\tt \phantom{g}\hfill}}}
\newcommand{\procname}[1]{{\tt #1\Gttdepth}}
%
% so we can change easily
%
\newcommand{\Functxt}{Function}
\newcommand{\Proctxt}{Procedure}
%
% Right page with no page number
% from ltxoutput.dtx
%
\renewcommand{\cleardoublepage}{\clearpage\if@twoside \ifodd\c@page\else
    \hbox{}\thispagestyle{empty}\newpage\if@twocolumn\hbox{}\newpage\fi\fi\fi}
%
%
\newcommand{\Rightpage}{\cleardoublepage}
% parts
%
\newcommand{\Rpart}[1]{\Rightpage
\hbox{}\thispagestyle{empty}\vfill\part{#1}\vfill
\Rightpage\setcounter{section}{0}}
%
% for section headings
%
% reserve space on current page
\newcommand{\ProcStart}{\par\addvspace{1cm}}
% to make all boxes same height and depth
%
\newcommand{\Prochd}{\makebox[0pt]{\phantom{gT}\hfill}}
%
\newsavebox{\RF@procbox}
\newsavebox{\SF@procbox}
%
\newlength{\secboxwidth}
\setlength{\secboxwidth}{\linewidth}
\addtolength{\secboxwidth}{-2\fboxsep}
%
\newlength{\fboxsepsave}
\setlength{\fboxsepsave}{\fboxsep}
%
\newcommand{\MoreAbbrFont}{\normalsize}
%
\newcommand{\ProcBox}[1]{%
\savebox{\RF@procbox}{\framebox[\linewidth][l]{\parbox[t]{\secboxwidth}{\section@font\Large\textbf{#1}\Prochd}}}%
\setlength{\fboxsep}{0pt}%
\colorbox{refgray}{\usebox{\RF@procbox}}%
\setlength{\fboxsep}{\fboxsepsave}
}
%
%
% Function/Procedure heading for no abbreviations
%
%  Arg. 1   First name (long name)
%  Arg. 2   labelname
%  Arg. 3   Second shorter name
%
\newcommand{\FPheading}[3]{%
\if@topaper
\Needspace*{3.0cm}%
\else
\newpage
\fi
\hypertarget{HU#2}{\relax}
\phantomsection{}
% for paper no alfabetical refecerence in table of contents
\if@topaper
\else
\addcontentsline{toc}{section}{\numberline{}\protect\Procname{#1}}%
\fi
\section*{\ProcBox{\textcolor{blueish}{#1}\hfill \textcolor{blueish}{#3}}}
\markboth{#1}{#1}%
\label{#2}}
%
% Same with more abbreviations
%
%  Arg. 4  more abbreviations (comma separated list)
%
\newcommand{\FPheadingAbbr}[4]{%
\if@topaper
\Needspace*{4.0cm}%
\else
\newpage
\fi
\hypertarget{HU#2}{\relax}
\phantomsection{}
% for paper no alfabetical refecerence in table of contents
\if@topaper
\else
\addcontentsline{toc}{section}{\numberline{}\protect\Procname{#1}}%
\fi
\section*{\ProcBox{{\textcolor{blueish}{#1}\hfill \textcolor{blueish}{#3}} \MoreAbbrFont\\[-5pt] \mbox{}\hfill \textcolor{blueish}{#4}}}
\markboth{#1}{#1}%
\label{#2}}
%
\newcommand{\Function}[3]{\FPheading{#1}{#2}{#3}}
\newcommand{\Procedure}[3]{\FPheading{#1}{#2}{#3}}
%
\newcommand{\FunctionAbbr}[4]{\FPheadingAbbr{#1}{#2}{#3}{#4}}
\newcommand{\ProcedureAbbr}[4]{\FPheadingAbbr{#1}{#2}{#3}{#4}}
%
%
% for pagestyle headings
%
%%%%%%%\addtolength{\headwidth}{2cm}
%%%\renewcommand{\headrulewidth}{0pt}
%
% for the procedure/function reference part
%
\fancypagestyle{procheadings}{%
\if@topaper
\renewcommand{\headrulewidth}{0.4pt}
\else
\renewcommand{\headrulewidth}{0pt}
\fi
\fancyfoot{}%
\fancyfoot[LE,RO]{\sffamily\small\thepage}%
\fancyhead{}%
\if@topaper
\fancyhead[LE,RO]{\sffamily\small\rightmark}%
\fi
}
%
% for the normal text parts
%
\fancypagestyle{textheadings}{%
\renewcommand{\headrulewidth}{0.4pt}
\fancyhf{}%
\fancyfoot[LE,RO]{\sffamily\small\thepage}%
\fancyhead{}%
\fancyhead[LE,RO]{\sffamily\small\leftmark}%
}
%
% Plain in Latex
%
\fancypagestyle{plain}{%
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
\fancyhf{}%
\fancyhead{}%
\fancyfoot[LE,RO]{\sffamily\small\thepage}%
}
\pagestyle{plain}
%
% Reset mark commands to what we defined (fancyhdr  changes them)
\def\chaptermark#1{\markboth{{#1}}{{#1}}}%
%
\def\sectionmark#1{\markright{{#1}}}%
%\def\ps@plain{\let\@mkboth\@gobbletwo
%     \let\@oddhead\@empty
%     \def\@oddfoot{\reset@font\hfill\sffamily\small\thepage}%
%     \let\@evenhead\@empty
%     \def\@evenfoot{\reset@font\sffamily\small\thepage\hfill}%
%}
%\pagestyle{plain}
%
% all sorts of definitions
%
\newcommand{\Subprocheading}[1]{\subsection*{\textcolor{brickred}{#1}}}
%
\newcommand{\Xspace}[1]{\Needspace{#1\baselineskip}}
\newcommand{\Name}{\section*{Name}}
\newcommand{\Summary}{\Xspace{3}\subsection*{Summary}}
\newcommand{\Description}{\Xspace{3}\Subprocheading{Description}}
\newcommand{\Details}{\Xspace{3}\Subprocheading{Details}}
\newcommand{\Arguments}{\Xspace{7}\Subprocheading{Arguments}}
\newcommand{\Usage}{\Xspace{7}\Subprocheading{Usage}}
\newcommand{\Value}{\Xspace{7}\Subprocheading{Value}}
\newcommand{\Bugs}{\Xspace{3}\Subprocheading{Bugs}}
\newcommand{\Warning}{\Xspace{3}\Subprocheading{Warning}}
\newcommand{\Examples}{\Xspace{5}\Subprocheading{Examples}}
\newcommand{\Seealso}{\Xspace{3}\Subprocheading{See also}}
\newcommand{\Abbreviation}{\Xspace\subsection*{Abbreviation}}
\newcommand{\Abbreviations}{\Xspace\subsection*{Abbreviations}}
\newcommand{\Abbrname}[1]{\textsl{#1}}
\newcommand{\Procname}[1]{\textsf{#1}}
\newcommand{\Argname}[1]{\textsl{#1}}
\newcommand{\Togglename}[1]{\texttt{#1}}
%
\newcommand{\Subsubprocheading}[1]{\Xspace{5}\subsection*{\textsl{#1}}}

\newcommand{\Classheading}[1]{\section*{#1}}
%
\renewcommand{\descriptionlabel}[1]{\hspace\labelsep #1}

%
% Description for arguments
% blank line between arguments
%
\newenvironment{Argdescription}%
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel
                        \itemsep\baselineskip
                        }}%
               {\endlist}%
%
\renewcommand{\labelitemi}{\textbullet}
\renewcommand{\labelitemii}{\textbullet}
\newcommand{\Bulletbox}{\makebox[0pt][r]{\makebox[1cm][l]{\textbullet}}}
%%%\newcommand{\Argitem}[1]{\texttt{\textit{#1}}}
%
%
\newcommand{\Argitem}[1]{\textsl{\textcolor{argcolor}{#1}}}
\newcommand{\Ditem}[1]{\item[#1]\mbox{}\\}
\newcommand{\Argtype}[1]{\emph{#1}}
%
\newlength{\Procnamelen}
\newlength{\Procboxnlen}
\newcommand{\Parg}[5]{%
\settowidth{\Procnamelen}{\Argitem{#1}}\setlength{\Procboxnlen}{.5\linewidth}%
\addtolength{\Procboxnlen}{-\Procnamelen}%
%
% at least two lines + 1 for the \itemsep
%
\Needspace{3\baselineskip}\relax%
\item[{\Argitem{#1}\hspace{\Procboxnlen}{\Argtype{#2 #3 #4 : #5}}}]\mbox{}\\}
\newcommand{\Optarg}[1]{\Parg{#1}{optional}}
\newcommand{\Reqarg}[1]{\Parg{#1}{}}
%
% to supress small indent of first column
%
\newlength{\neglen}
\setlength{\neglen}{-\tabcolsep}
\newcolumntype{z}{@{\extracolsep{\the\neglen}}}
%
%
\newcommand{\argname}[1]{\textsl{\textcolor{argcolor}{#1}}}
\newcommand{\Isisvar}[1]{\texttt{#1}}
\newcommand{\Isisdb}[1]{\texttt{{#1}}}
\newcommand{\Isisdate}[1]{\texttt{#1}}
\newcommand{\IsisNA}[1]{\texttt{#1}}
\newcommand{\ColumnHeader}[1]{\textbf{\textsf{#1}}}
\newcommand{\ProgramName}[1]{\textsf{#1}}
%
\newcommand{\Fnext}[1]{\texttt{#1}}
\newcommand{\Fname}[1]{\texttt{#1}}
\newcommand{\Filter}[1]{\texttt{{#1}}}
%
%\newcommand{\togglename}[1]{\textsf{\textit{#1}}}
\newcommand{\togglename}[1]{\textsf{\textit{\textcolor{tgdcolor}{#1}}}}
\newcommand{\Tgdlabel}[1]{\mbox{\togglename{#1}}\hfill}
\newcommand{\Filelabel}[1]{\mbox{\Fname{#1}}\hfill}
%
% standard (no) itemsep)
%
\newenvironment{toggledescription}[1]%
{\begin{list}{}{%
\renewcommand{\makelabel}{\Tgdlabel}%
\settowidth{\labelwidth}{\togglename{#1}}%
\setlength{\leftmargin}{\labelwidth}%
\addtolength{\leftmargin}{\labelsep}%
\addtolength{\leftmargin}{0.5cm}%
\setlength{\itemsep}{0.5mm}%{0.25\baselineskip}%
}}%
{\end{list}}
%
% with some itemsep
%
\newenvironment{wtoggledescription}[1]%
{\begin{list}{}{%
\renewcommand{\makelabel}{\Tgdlabel}%
\settowidth{\labelwidth}{\togglename{#1}}%
\setlength{\leftmargin}{\labelwidth}%
\addtolength{\leftmargin}{\labelsep}%
\setlength{\itemsep}{1pt}%{0.25\baselineskip}%
}}%
{\end{list}}
%
% no itemsep
%
\newenvironment{vdescription}[1]%
{\begin{list}{}{%
\settowidth{\labelwidth}{#1}%
\def\makelabel##1{##1\hfil}
\setlength{\leftmargin}{\labelwidth}%
\addtolength{\leftmargin}{\labelsep}%
}}%
{\end{list}}
%
% with some itemsep
%
\newenvironment{wdescription}[1]%
{\begin{list}{}{%
\settowidth{\labelwidth}{#1}%
\def\makelabel##1{##1\hfil}
\setlength{\leftmargin}{\labelwidth}%
\addtolength{\leftmargin}{\labelsep}%
\setlength{\itemsep}{0.5\baselineskip}%
}}%
{\end{list}}
%
% list of files with explanations
%
\newenvironment{filedescription}[1]%
{\begin{list}{}{%
\renewcommand{\makelabel}{\Filelabel}%
\settowidth{\labelwidth}{\Fname{#1}}%
\setlength{\leftmargin}{\labelwidth}%
\addtolength{\leftmargin}{\labelsep}%
\addtolength{\leftmargin}{0.5cm}%
\setlength{\itemsep}{0.5mm}%{0.25\baselineskip}%
}}%
{\end{list}}
%
% with some itemsep
%
%
% Indented lines of text
%
\newenvironment{indentleft}{%
\begin{list}{}{\@settopsep[.5,.1,.1]}\item[]}%
{\end{list}}%
%
% Procedure/function overview
%
\newcommand{\Ovwlabel}[1]{\mbox{#1}\hfill}
\newenvironment{Ovwlist}{%
\begin{list}{}{%
    \renewcommand{\makelabel}{\Ovwlabel}%
    \setlength{\labelwidth}{3.7cm}%
    \setlength{\labelsep}{2mm}%
    \setlength{\leftmargin}{3.9cm}%
    }%
}%
{\end{list}}
%
%
%
% Input/Output from file
% baselinestretch doesn't work?
% Optioanl firts argument to get everything on a single page (use true)
%
\newcommand{\VOsmall}{%
   \@setfontsize\Osmall\@ixpt{3.8mm}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \belowdisplayskip \abovedisplayskip
   \lineskip0pt
}
%
\newcommand{\IsisOutputFromFile}[2][false]{%
\begin{trivlist}
\renewcommand{\FancyVerbFormatLine}[1]{\colorbox{outbg}{\makebox[\linewidth][l]{\strut##1}}}%
\VerbatimInput[fontsize=\VOsmall,baselinestretch=0.75,xleftmargin=\UnitIndent,gobble=2,samepage=#1]{#2}%
\end{trivlist}%
}
%
%
% For isis syntax reference
\newlength{\TableRuleHeight}
\setlength{\TableRuleHeight}{0.5pt}
\newcommand{\Tablepreamble}{%
\sffamily\small\rule{\textwidth}{\TableRuleHeight}%
\setlength{\abovecaptionskip}{0mm}}
\newcommand{\Tablepostamble}{\rule{\textwidth}{\TableRuleHeight}}
%
%
%
% For syntax descriptions
%
%  \stxlem  nonterminal
%  \kword   terminal (keyword)
%  \operat  operator
%
%  \Optbegin  start of optional stuff
%  \Optend    end   of optional stuff
%
%  \ntfont switch to standard normal documentfont
%          so that we know the starting situation
%
\newcommand{\ntfont}{\fontfamily{\rmdefault}\upshape\mdseries\selectfont}
%
\newcommand{\stxlm}[1]{\mbox{\ntfont\textit{#1}}}
\newcommand{\kword}[1]{\mbox{\ntfont\texttt{#1}}}
\newcommand{\operat}[1]{\mbox{\ntfont\texttt{#1}}}
\newcommand{\punct}[1]{\mbox{\ntfont\textbf{\texttt{#1}}}}

%
\newcommand{\Optbegin}{{\ntfont[}}
\newcommand{\Optend}{{\ntfont]}}
%
\newcommand{\Sindent}{\hspace*{0cm}{\qquad}}
%
\newenvironment{syntax}{\begin{indentleft}}{\end{indentleft}}
%
% program syntax in model reference
%
\newenvironment{prog}{\begin{list}{}%
{\@settopsep[.5,.1,.1]}\item[]\tt\begin{tabbing}}%
{\end{tabbing}\end{list}}%
%
%
% Description list for syntax (used in model syntax reference)
%
\newcommand{\Dlitem}{\item}
\newlength{\Dlwidth}
\newenvironment{Dlist}[1]{\begin{list}{}{%
 \settowidth{\Dlwidth}{#1}
 \setlength{\labelwidth}{\Dlwidth}
 \setlength{\labelsep}{.5cm} \setlength{\leftmargin}{\labelwidth}
 \addtolength{\leftmargin}{\labelsep}
 \def\makelabel##1{##1\hfil}
 \addtolength{\leftmargin}{\parindent}}}{\end{list}}
%
% Old (will not indent)
%%%%\newenvironment{example}{\verbatim}{\endverbatim}
% New (may indent)
%
\let\OLD@FV@ListVSpace\FV@ListVSpace
\def\FV@ListVSpace{\@settopsep[.5,.1,.1]\OLD@FV@ListVSpace}
%\DefineVerbatimEnvironment{example}{Verbatim}{xleftmargin=\unit@indent}
\DefineVerbatimEnvironment{usage}{Verbatim}{}
%
\newcommand{\VerbFileInput}[1]{\VerbatimInput[{fontsize=\small,xleftmargin=\unit@indent}]{#1}}
%
%%%\def\verbatim@font{\normalfont\ttfamily\small}%
%
\newlength{\abrwidth}
%%\newcommand{\abrxref}[1]{~~~\textsl{\pageref{#1}}}
\newcommand{\abrxref}[1]{}
\newcommand{\abrlabel}[1]{#1\hfill}
\newenvironment{abrxrf}{\begin{list}{}{%
% \settowidth{\abrwidth}{3in}
 \setlength{\abrwidth}{1in}
 \setlength{\labelwidth}{\abrwidth}
 \setlength{\labelsep}{1cm}
 \setlength{\leftmargin}{\labelwidth}
 \addtolength{\leftmargin}{\labelsep}
% \addtolength{\leftmargin}{\parindent}
% \setlength{\rightmargin}{\leftmargin}
\let\makelabel\abrlabel
}}{\end{list}}
%
% new paragraph in normal text
%
\newcommand{\Tpar}{\par}
%
% new paragraph immediately after \end{example}
%
\newcommand{\Xpar}{\par\noindent}
%
\newcommand{\Coverpage}[1]{%
% No nice cover for printed version anymore
%\if@topaper
%\includepdf[offset=0mm -1in]{graphics/coverrefman.pdf}
%\Rightpage
%\fi
\hbox{}\vfill
\thispagestyle{empty}
\begin{center}
{\Huge
#1\\
}
\vspace{20pt}
Department of Information Technology and Research Support
%Berend Hasselman \\
%Anita van der Roest \\
%Rob van Harrevelt \\

\refmandate\today\\
%%#2\\
\end{center}
\vfill
}

\newcommand*{\Needlines}[1]{\Needspace{#1\baselineskip}}
