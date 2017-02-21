% Class file for Isis Reference manual
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{refman}[23/03/2005 IsisMdl methods Reference Manual]
\LoadClass[10pt,a4paper]{article}

% TODO: what is the difference between \usepackage and \RequirePackage?
\usepackage{makeidx}
\usepackage{hyperref}
\usepackage{needspace}
\usepackage{enumitem}
\RequirePackage{color}
\RequirePackage{hyperref}
\RequirePackage{needspace}
\RequirePackage{fancyvrb}
\RequirePackage{calc}

\widowpenalty=1000
\clubpenalty=1000

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
\PassOptionsToPackage{pdftex,hypertexnames=false,hidelinks,hyperindex=true,colorlinks=false,pdfstartview={FitH 1000}}{hyperref}%
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

\newcommand{\Methodname}[1]{\textsf{#1}}
\newcommand{\Classname}[1]{\textsf{#1}}
\newcommand{\Packagename}[1]{\textsf{#1}}
\newcommand{\argname}[1]{\textsl{\textcolor{argcolor}{#1}}}
\newcommand{\code}[1]{\textsf{#1}}

% headings for subsections for each method
\newcommand{\Subprocheading}[1]{\subsection*{\textcolor{brickred}{#1}}}
\newcommand{\Xspace}[1]{\Needspace{#1\baselineskip}}
\newcommand{\Description}{\Xspace{3}\Subprocheading{Description}}
\newcommand{\Details}{\Xspace{3}\Subprocheading{Details}}
\newcommand{\Arguments}{\Xspace{7}\Subprocheading{Arguments}}
\newcommand{\Usage}{\Xspace{7}\Subprocheading{Usage}}
\newcommand{\Value}{\Xspace{7}\Subprocheading{Value}}
\newcommand{\Bugs}{\Xspace{3}\Subprocheading{Bugs}}
\newcommand{\Examples}{\Xspace{5}\Subprocheading{Examples}}
\newcommand{\Seealso}{\Xspace{3}\Subprocheading{See also}}

% usage environment
\DefineVerbatimEnvironment{usage}{Verbatim}{}

%
% Description for arguments
%
% Argument 1 is the longest argument name
%
\newcommand{\Argitem}[1]{\argname{#1}}
\newenvironment{Argdescription}[1]%
        {\begin{description}[leftmargin=!,
                labelwidth=\widthof{\argname{#1}} + 2em]}
        {\end{description}}

% reserve space on current page
\newcommand{\MethodStart}{\par\addvspace{1cm}}

% to make all boxes same height and depth
%
\newcommand{\Methodhd}{\makebox[0pt]{\phantom{gT}\hfill}}

\newsavebox{\RF@methodbox}

\newlength{\secboxwidth}
\setlength{\secboxwidth}{\linewidth}
\addtolength{\secboxwidth}{-2\fboxsep}
%
\newlength{\fboxsepsave}
\setlength{\fboxsepsave}{\fboxsep}
%
\newcommand{\MoreAbbrFont}{\normalsize}
%
\newcommand{\MethodBox}[1]{%
\savebox{\RF@methodbox}{\framebox[\linewidth][l]{\parbox[t]{\secboxwidth}{\Large\textbf{#1}\Methodhd}}}%
\setlength{\fboxsep}{0pt}%
\colorbox{refgray}{\usebox{\RF@methodbox}}%
\setlength{\fboxsep}{\fboxsepsave}
}

%
%
% Method heading single method
%
%  Arg. 1   Method name (long name)
%  Arg. 2   labelname (used for hyperrefs)
%
\newcommand{\Method}[2]{%
\if@topaper
\Needspace*{3.0cm}%
\else
\Needspace*{3.0cm}%
\fi
\hypertarget{HU#2}{\relax}
\phantomsection{}
% for paper no alfabetical refererence in table of contents
\if@topaper
\else
\addcontentsline{toc}{section}{\numberline{}\protect\Methodname{#1}}%
\fi
\section*{\MethodBox{\textcolor{blueish}{#1}}}
\markboth{#1}{#1}%
\label{#2}}

%
%
% Method heading 4 methods method
%
%  Arg. 1   Main method name
%  Arg. 2   labelname (used for hyperrefs)
%  Arg. 3 .. 5: extra methods described in the same header.
%
\newcommand{\FourMethods}[5]{%
\if@topaper
\Needspace*{6.0cm}%
\else
\Needspace*{6.0cm}%
\fi
\hypertarget{HU#2}{\relax}
\phantomsection{}
% for paper no alfabetical refererence in table of contents
\if@topaper
\else
\fi
\addcontentsline{toc}{section}{\numberline{}\protect\Methodname{#1/#3/#4/#5}}%
\section*{\MethodBox{\textcolor{blueish}{#1} \\
                     \textcolor{blueish}{#3} \\
                     \textcolor{blueish}{#4} \\
                     \textcolor{blueish}{#5}}}
\markboth{#1}{#1}%
\label{#2}}
