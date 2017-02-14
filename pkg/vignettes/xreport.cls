%%
%% This is file `xreport.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% xclass.dtx  (with options: `report,indent')
%%
%%  This system is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%
%%
%% IMPORTANT NOTICE:
%%
%% For error reports in case of UNCHANGED versions see bugs.txt.
%%
%% Please do not request updates from us directly.  Distribution is
%% done through Mail-Servers and TeX organizations.
%%
%% You are not allowed to change this file.
%%
%% You are allowed to distribute this file under the condition that
%% it is distributed together with all files mentioned in manifest.txt.
%%
%% If you receive only some of these files from someone, complain!
%%
%% You are NOT ALLOWED to distribute this file alone.  You are NOT
%% ALLOWED to take money for the distribution or use of either this
%% file or a changed version, except for a nominal charge for copying
%% etc.
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{xreport}
              [2001/09/12 v1.0a
 LaTeX document class]
\newcommand{\@ptsize}{}
\newif\if@bigtopmargin
\@bigtopmargintrue
\newif\if@restonecol
\newif\if@titlepage
\@titlepagetrue
 \newif\if@openright
\newif\if@openbib
\@openbibfalse
\newif\if@soberchapter
\@soberchaptertrue
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\DeclareOption{10pt}{\renewcommand{\@ptsize}{0}}
\DeclareOption{11pt}{\renewcommand{\@ptsize}{1}}
\DeclareOption{12pt}{\renewcommand{\@ptsize}{2}}
\DeclareOption{d4margin}{\@bigtopmarginfalse}
\DeclareOption{a4margin}{\@bigtopmargintrue}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\DeclareOption{titlepage}{\@titlepagetrue}
\DeclareOption{notitlepage}{\@titlepagefalse}
 \DeclareOption{openright}{\@openrighttrue}
 \DeclareOption{openany}{\@openrightfalse}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\PassOptionsToPackage{fleqn}{amsmath}\input{xfleqn.clo}}
\DeclareOption{openbib}{\@openbibtrue}
\DeclareOption{soberchapter}{\@soberchaptertrue}
\DeclareOption{fancychapter}{\@soberchapterfalse}
\def\@toc@option{0}
\DeclareOption{spacedtoc}{\def\@toc@option{0}}
\DeclareOption{filledtoc}{\def\@toc@option{1}}
\DeclareOption{dotoldtoc}{\def\@toc@option{2}}
\DeclareOption{spcoldtoc}{\def\@toc@option{3}}
\ExecuteOptions{a4paper,10pt,a4margin,soberchapter,oneside,onecolumn,final,openany}
\ProcessOptions
\input{xpt1\@ptsize.clo}
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand{\baselinestretch}{}
\newskip\@tinyskip \@tinyskip= \z@ plus 1pt
\def\@settopsep[#1,#2,#3]{\topsep=#1\baselineskip \@plus #2\baselineskip
 \@minus #3\baselineskip\relax}
\newif\if@revlabel \@revlabelfalse
\newdimen\unit@indent
\newdimen\@revlabelskip
\def\@headingnumsep{.5em}
\def\SetUnitIndent#1{\settowidth{\unit@indent}{#1}\set@indenting}
\def\set@indenting{\global\parindent=\unit@indent
\global\leftmargini=\unit@indent
\global\@revlabelskip=.5\unit@indent
}
\def\SetHeadingIndent#1{\settowidth{\sec@maxboxwd}{#1\relax\hskip\@headingnumsep}}
\newdimen\sec@maxboxwd
\def\calc@secwid#1{%
\setbox\@tempboxa=\hbox{\csname the#1\endcsname\hskip\@headingnumsep}%
\ifdim\wd\@tempboxa>\sec@maxboxwd
  \hbox{\csname the#1\endcsname\hskip\@headingnumsep}%
\else
  \hbox to \sec@maxboxwd{\csname the#1\endcsname\hfil}%
\fi}
\def\@seccntformat#1{\noexpand\calc@secwid{#1}}
\def\SetAllIndent#1{\SetHeadingIndent{#1}%
\setlength{\unit@indent}{\sec@maxboxwd}\set@indenting}
 \SetAllIndent{\normalsize\bfseries 1.1}
\newdimen\other@margin
\settowidth{\other@margin}{\normalsize (m)\hskip.6em}
\setlength\parskip{\@tinyskip}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand{\topfraction}{.7}
\setcounter{bottomnumber}{1}
\renewcommand{\bottomfraction}{.3}
\setcounter{totalnumber}{3}
\renewcommand{\textfraction}{.2}
\renewcommand{\floatpagefraction}{.5}
\setcounter{dbltopnumber}{2}
\renewcommand{\dbltopfraction}{.7}
\renewcommand{\dblfloatpagefraction}{.5}
\if@twoside
    \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\slshape\leftmark}%
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
      \def\chaptermark##1{%
        \markboth {{%
          \ifnum \c@secnumdepth >\m@ne
            \@thechapterprefix \ %
          \fi
          ##1}}{}}%
      \def\sectionmark##1{%
        \markright {{%
           \ifnum \c@secnumdepth >\z@
            \thesection \ %
           \fi
           ##1}}}}
\else
    \def\ps@headings{%
      \let\@oddfoot\@empty
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
      \def\chaptermark##1{%
        \markright {{%
          \ifnum \c@secnumdepth >\m@ne
            \@thechapterprefix \ %
          \fi
          ##1}}}}
\fi
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\chaptermark\@gobble
    \let\sectionmark\@gobble
    }
\def\ps@flush{\let\@mkboth\@gobbletwo
     \def\@oddhead{}\def\@evenhead{}%
     \def\@oddfoot{\hfil\thepage}%
     \if@twoside\def\@evenfoot{\thepage\hfil}%
           \else\let\@evenfoot\@oddfoot\fi}
\def\titlefont#1{\def\title@font{#1}}
\def\authorfont#1{\def\author@font{#1}}
\def\datefont#1{\def\date@font{#1}}
\def\abstractbodyfont#1{\def\abstractbody@font{#1}}
\def\abstractnamefont#1{\def\abstractname@font{#1}}
\titlefont{\LARGE}
\authorfont{\large}
\datefont{\large}
\abstractbodyfont{\small}
\abstractnamefont{\bfseries}
\if@titlepage
  \newcommand{\maketitle}{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let \footnote \thanks
  \null\vfil
  \vskip 60\p@
  \begin{center}%
    {\title@font \@title \par}%
    \vskip 3em%
    {\author@font
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
      \vskip 1.5em%
    {\date@font \@date \par}%       % Set date in \large size.
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \let\thanks\relax\let\maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\else
\newcommand{\maketitle}{\par
  \begingroup
    \renewcommand{\thefootnote}{\fnsymbol{footnote}}%
    \def\@makefnmark{\hbox to\z@{\@textsuperscript{\@thefnmark}\hss}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hbox to1.8em{\hss\@textsuperscript{\@thefnmark}}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{plain}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \let\thanks\relax
  \let\maketitle\relax\let\@maketitle\relax
  \gdef\@thanks{}\gdef\@author{}\gdef\@title{}}
\def\@maketitle{%
  \newpage
  \null
  \vskip 2em%
  \begin{center}%
    {\title@font \@title \par}%
    \vskip 1.5em%
    {\author@font
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
    \vskip 1em%
    {\date@font \@date}%
  \end{center}%
  \par
  \vskip 1.5em}
\fi
\newcommand{\chaptermark}[1]{}
\setcounter{secnumdepth}{2}
\newcounter {part}
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\renewcommand{\thepart}          {\Roman{part}}
\renewcommand{\thechapter}       {\arabic{chapter}}
\renewcommand{\thesection}       {\thechapter.\arabic{section}}
\renewcommand{\thesubsection}    {\thesection.\arabic{subsection}}
\renewcommand{\thesubsubsection} {\thesubsection .\arabic{subsubsection}}
\renewcommand{\theparagraph}     {\thesubsubsection.\arabic{paragraph}}
\renewcommand{\thesubparagraph}  {\theparagraph.\arabic{subparagraph}}
\newcommand{\@chapapp}{\chaptername}
\def\head@style{\interlinepenalty \@M
    \hyphenpenalty \@M \exhyphenpenalty \@M
    \parindent \z@ \rightskip \z@ plus .7\hsize\relax}
\def\partfont#1{\def\part@font{\normalfont#1}}
\def\sectionfont#1{\def\section@font{\head@style\normalfont#1}}
\def\chapterfont#1{\def\chapter@font{\normalfont#1}}
\def\subsectionfont#1{\def\subsection@font{\head@style\normalfont#1}}
\def\subsubsectionfont#1{\def\subsubsection@font{\head@style\normalfont#1}}
\def\paragraphfont#1{\def\paragraph@font{\head@style\normalfont#1}}
\def\subparagraphfont#1{\def\subparagraph@font{\head@style\normalfont#1}}
\partfont{\Large\bfseries}
\chapterfont{\large\bfseries}
\sectionfont{\normalsize\bfseries}
\subsectionfont{\normalsize\bfseries}
\subsubsectionfont{\normalsize\slshape}
\paragraphfont{\normalsize\slshape}
\subparagraphfont{\normalsize\slshape}
\@onlypreamble\partfont
\@onlypreamble\chapterfont
\@onlypreamble\sectionfont
\@onlypreamble\subsectionfont
\@onlypreamble\subsubsectionfont
\@onlypreamble\paragraphfont
\@onlypreamble\subparagraphfont
 \SetAllIndent{\subsubsection@font 1.1}
\newcommand{\part}{\cleardoublepage
                 \thispagestyle{empty}%
                 \if@twocolumn
                     \onecolumn
                     \@tempswatrue
                   \else
                     \@tempswafalse
                 \fi
                 \hbox{}\vfil
                 \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}
    {\centering
     \interlinepenalty \@M
     \ifnum \c@secnumdepth >-2\relax
       \part@font \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \part@font #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \part@font #1\par}%
    \@endpart}
\def\@endpart{\vfil\newpage
              \if@twoside
                \hbox{}%
                \thispagestyle{empty}%
                \newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}
\if@soberchapter
\def\chapter{\if@openright\cleardoublepage\else\clearpage\fi
\thispagestyle{plain}
\global\@topnum\z@
\@afterindentfalse
\@startsection{chapter}{0}{\z@}{-20pt}{.5\baselineskip}{\chapter@font}}
\def\chaptermark#1{}
\def\@thechapterprefix{\thechapter}
\def\@chapapp{}
\else
\def\@thechapterprefix{\@chapapp{} \thechapter}
\def\@chapapp{\chaptername}
\newcommand{\chapter}{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                         \refstepcounter{chapter}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                    \else
                      \addcontentsline{toc}{chapter}{#1}
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}
\def\@makechapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \parskip \z@ \head@style
    \ifnum \c@secnumdepth >\m@ne
         \chapter@font \@thechapterprefix
         \par\nobreak
         \vskip 20\p@
       \fi
    \chapter@font #1\par
    \nobreak
    \vskip 40\p@
  }}
\def\@schapter#1{\if@twocolumn
                   \@topnewpage[\@makeschapterhead{#1}]%
                 \else
                   \@makeschapterhead{#1}%
                   \@afterheading
                 \fi}
\def\@makeschapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \parskip \z@ \head@style
    \chapter@font  #1\par
    \nobreak
    \vskip 40\p@
  }}
\fi
\newcommand{\section}{\@startsection {section}{1}{\z@}%
{-1\baselineskip \@plus -0.1\baselineskip \@minus -0.1\baselineskip}%
{\baselineskip}{\section@font}}
\newcommand{\subsection}{\@startsection{subsection}{2}{\z@}%
{-1\baselineskip \@plus -.1\baselineskip \@minus -.1\baselineskip}%
{\baselineskip}{\subsection@font}}
\newcommand{\subsubsection}{\@startsection{subsubsection}{3}{\z@}%
{-1\baselineskip \@plus -.1\baselineskip \@minus -.1\baselineskip}%
{\baselineskip}{\subsubsection@font}}
\newcommand{\paragraph}{\@startsection{paragraph}{4}{\z@}%
  {3.25ex \@plus 1ex \@minus .2ex}{-1em}{\paragraph@font}}
\newcommand{\subparagraph}{\@startsection{subparagraph}{5}{\parindent}%
  {3.25ex \@plus 1ex \@minus .2ex}{-1em}{\subparagraph@font}}
\def\list#1#2{\ifnum \@listdepth >5\relax \@toodeep
     \else \global\advance\@listdepth\@ne \fi
  \rightmargin \z@ \listparindent\z@ \itemindent\z@
  \let\makelabel\@mklab % Moved
  \csname @list\romannumeral\the\@listdepth\endcsname
  \def\@itemlabel{#1} \@nmbrlistfalse #2\relax
  \@trivlist
  \parskip\parsep \parindent\listparindent
  \advance\linewidth -\rightmargin \advance\linewidth -\leftmargin
  \advance\@totalleftmargin \leftmargin
  \parshape \@ne \@totalleftmargin \linewidth
  \ignorespaces}
\def\enumerate{\ifnum \@enumdepth >3 \@toodeep\else
      \advance\@enumdepth \@ne
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
      {\csname label\@enumctr\endcsname}{\usecounter
        {\@enumctr} \default@enumlabel}\fi}
\def\itemize{%
  \ifnum \@itemdepth >3 \@toodeep\else \advance\@itemdepth \@ne
  \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
  \list{\csname\@itemitem\endcsname}{\default@itemlabel}\fi}
\def\default@itemlabel{\def\makelabel##1{##1\hfil}}
\let\default@enumlabel=\default@itemlabel
\let\KRN@trivlist\trivlist
\def\trivlist{\@settopsep[.5,.1,.1]\KRN@trivlist}
\setlength\leftmargini{\unit@indent}
\setlength\leftmarginii{\other@margin}
\setlength\leftmarginiii{\other@margin}
\setlength\leftmarginiv{\other@margin}
\setlength\leftmarginv{\other@margin}
\setlength\leftmarginvi{1em}
\setlength\leftmargin    {\leftmargini}
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\setlength\partopsep{0.5\baselineskip}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\def\define@listlabel{\ifnum \@listdepth=1
  \if@revlabel
    \def\makelabel##1{\hskip \@revlabelskip{##1\hfil}}%
  \else
    \def\makelabel##1{\hfil##1}%
  \fi
\else
 \def\makelabel##1{##1\hfil}%
\fi}
\def\@listinit{ \labelsep=.5em
 \labelwidth=\leftmargin \advance\labelwidth by -\labelsep
\itemsep=\@tinyskip
\topsep=\@tinyskip
 \parsep \z@ \define@listlabel}
\def\@listI  {\leftmargin\unit@indent   \@listinit }
\def\@listii {\leftmargin\leftmarginii  \@listinit }
\def\@listiii{\leftmargin\leftmarginiii \@listinit }
\def\@listiv {\leftmargin\leftmarginiv  \@listinit }
\def\@listv  {\leftmargin\leftmarginv   \@listinit }
\def\@listvi {\leftmargin\leftmarginvi  \@listinit }
\let\@listi\@listI
\@listi
\renewcommand{\theenumi}{\arabic{enumi}}
\renewcommand{\theenumii}{\alph{enumii}}
\renewcommand{\theenumiii}{\roman{enumiii}}
\renewcommand{\theenumiv}{\Alph{enumiv}}
\newcommand{\labelenumi}{\theenumi.}
\newcommand{\labelenumii}{(\theenumii)}
\newcommand{\labelenumiii}{\theenumiii.}
\newcommand{\labelenumiv}{\theenumiv.}
\renewcommand{\p@enumii}{\theenumi}
\renewcommand{\p@enumiii}{\theenumi(\theenumii)}
\renewcommand{\p@enumiv}{\p@enumiii\theenumiii}
\newcommand{\labelitemi}{\tiny\textbullet}
\newcommand{\labelitemii}{\tiny\textbullet}
\newcommand{\labelitemiii}{\textasteriskcentered}
\newcommand{\labelitemiv}{\textperiodcentered}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand{\descriptionlabel}[1]{\hspace\labelsep
                                \normalfont\bfseries #1}
\if@titlepage
  \newenvironment{abstract}{%
      \titlepage
      \null\vfil \abstractbody@font
      \begin{center}
        \abstractname@font \abstractname
      \end{center}}
     {\par\vfil\null\endtitlepage}
\else
  \newenvironment{abstract}{%
      \if@twocolumn
        \section*{\abstractname}%
      \else
        \abstractbody@font
        \begin{center}%
          {\abstractname@font \abstractname\vspace{-.5em}\vspace{\z@}}%
        \end{center}%
        \quotation
      \fi}
      {\if@twocolumn\else\endquotation\fi}
\fi
\newenvironment{verse}
               {\let\\=\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item[]}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item[]}
               {\endlist}
\newenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \item[]}
               {\endlist}
\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \if@compatibility
        \setcounter{page}{0}
      \else
        \setcounter{page}{1}%
      \fi}%
    {\if@restonecol\twocolumn \else \newpage \fi
      \setcounter{page}{1}%
    }
\newcommand{\appendix}{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand{\@chapapp}{\appendixname}%
  \renewcommand{\thechapter}{\Alph{chapter}}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\def\@minipagerestore{\parindent\unit@indent}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\@addtoreset{equation}{chapter}
\renewcommand{\theequation}{\thechapter.\arabic{equation}}
\newcounter{figure}[chapter]
\renewcommand{\thefigure}{\thechapter.\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}[chapter]
\renewcommand{\thetable}{\thechapter.\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}
               {\@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\def\captionlabelfont#1{\def\captionlabel@font{#1}}
\def\captionlabelsep#1{\def\captionlabel@sep{#1}}
\captionlabelfont{\bfseries}
\captionlabelsep{ }
\def\leftcaptions{\def\@caplglue{}}
\def\centeredcaptions{\def\@caplglue{\hfil}}
\leftcaptions
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{{\captionlabel@font #1}\captionlabel@sep\ #2}%
  \ifdim \wd\@tempboxa >\hsize
    {\captionlabel@font #1}\captionlabel@sep\ #2\par
  \else
    \global \@minipagefalse
    \hbox to\hsize{\@caplglue\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*{\cal}{\@fontswitch{\relax}{\mathcal}}
\DeclareRobustCommand*{\mit}{\@fontswitch{\relax}{\mathnormal}}
\newcommand{\@pnumwidth}{1.55em}
\newcommand{\@tocrmarg} {2.55em}
\newcommand{\@dotsep}{4.5}
 \setcounter{tocdepth}{2}

\newcommand{\tableofcontents}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\contentsname
        \@mkboth{\contentsname}{\contentsname}}%
    \@starttoc{toc}%
     \if@restonecol\twocolumn\fi
    }
\input{xtoc.r}
\newcommand{\listoffigures}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename
      \@mkboth{\listfigurename}%
              {\listfigurename}}%
  \addcontentsline{toc}{chapter}{\listfigurename}%
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }
\newcommand{\listoftables}{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename
      \@mkboth{\listtablename}{\listtablename}}%
  \addcontentsline{toc}{chapter}{\listtablename}%
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }
\let\l@table\l@figure
\newdimen\bibindent
\bibindent=1.5em
\newenvironment{thebibliography}[1]
     {\chapter*{\bibname
        \@mkboth{\bibname}{\bibname}}%
        \addcontentsline{toc}{chapter}{\bibname}%
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \if@openbib
              \advance\leftmargin\bibindent
              \itemindent -\bibindent
              \listparindent \itemindent
              \parsep \z@
            \fi
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand{\theenumiv}{\arabic{enumiv}}}%
      \if@openbib
        \renewcommand{\newblock}{\par}
      \else
        \renewcommand{\newblock}{\hskip .11em \@plus.33em \@minus.07em}%
      \fi
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand{\newblock}{}
\newenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
       \twocolumn[\chapter*{\indexname}]%
                \@mkboth{\indexname}%
                        {\indexname}%
       \addcontentsline{toc}{chapter}{\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand{\@idxitem}  {\par\hangindent 40\p@}
\newcommand{\subitem}   {\par\hangindent 40\p@ \hspace*{20\p@}}
\newcommand{\subsubitem}{\par\hangindent 40\p@ \hspace*{30\p@}}
\newcommand{\indexspace}{\par \vskip \baselineskip\relax}
\renewcommand{\footnoterule}{%
  \kern-3\p@
  \hrule width 1in %.4\columnwidth too long
  \kern 2.6\p@}
 \@addtoreset{footnote}{chapter}
\long\def\@makefntext#1{%
    \parindent \z@ \@revlabeltrue
     \leavevmode\hbox to \@revlabelskip{\@textsuperscript{\@thefnmark}\hss}#1}
\newcommand{\contentsname}{Contents}
\newcommand{\listfigurename}{List of Figures}
\newcommand{\listtablename}{List of Tables}
\newcommand{\bibname}{Bibliography}
\newcommand{\indexname}{Index}
\newcommand{\figurename}{Figure}
\newcommand{\tablename}{Table}
\newcommand{\partname}{Part}
\newcommand{\chaptername}{Chapter}
\newcommand{\appendixname}{Appendix}
\newcommand{\abstractname}{Abstract}
\newcommand{\today}{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{plain}
\pagenumbering{arabic}      % Arabic page numbers
\if@twoside
\else
  \raggedbottom
\fi
\if@twocolumn
  \twocolumn
  \sloppy
  \flushbottom
\else
  \onecolumn
\fi
\endinput
%%
%% End of file `xreport.cls'.
