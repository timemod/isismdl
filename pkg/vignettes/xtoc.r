%%
%% This is file `xtoc.r',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% xtoc.dtx  (with options: `report')
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







\newdimen\sav@tempdima
\def\@maketocline#1#2#3#4#5{\ifnum #1>\c@tocdepth\else
  \vskip \z@ \@plus.2\p@
  {\leftskip #2\relax  \rightskip \@tocrmarg
   \parfillskip -\rightskip
   \parindent #2\relax \@afterindenttrue
   \interlinepenalty\@M
   \leavevmode
   \sav@tempdima #3\relax \@tempdima\sav@tempdima
   \advance\leftskip \@tempdima \hbox{}\hskip -\leftskip
   \@tempdima\sav@tempdima \@tocstyle{#1}\@tempdima\sav@tempdima
   {#4}\nobreak
   \@tocleaders{#1}\nobreak\@tocpnumfmt{#1}{#5}\par}\fi}

%%% Note: \nobreak's added 7 Jan 86 to prevent bad line break that
%%% left the page number dangling by itself at left edge of a new line.
%%%
%%% Changed 25 Jan 88 to use |\leftskip| instead of |\hangindent| so
%%% leaders of multiple-line contents entries would line up properly.




\ifcase\@toc@option % default fixed space before pagenumber
\def\toc@font#1{\ifcase#1\relax\bfseries\else\ifnum#1<0\large\bfseries
\else\rmfamily\fi\fi}
\def\@tocleaders#1{\hskip 1em}
\def\@tocstyle#1{\rightskip\@tempdima \@plus 1fil\parfillskip\z@\toc@font{#1}}
\def\@tocpnumfmt#1#2{\hbox to\@pnumwidth{\hfil\slshape #2}}
\def\@ftocline#1{\@maketocline{#1}{\z@}{\unit@indent}}
\def\l@part{\ifnum\c@tocdepth>-2\relax
\addpenalty{-\@highpenalty}\addvspace{2.25em \@plus\p@}\fi
\@ftocline{-1}}
\def\l@chapter{\@ftocline{0}}
\def\l@section{\@ftocline{1}}
\def\l@subsection{\@ftocline{2}}
\def\l@subsubsection{\@ftocline{3}}
\def\l@paragraph{\@ftocline{4}}
\def\l@subparagraph{\@ftocline{5}}
\def\l@figure{\@ftocline{1}}
\or % spaces as leaders
\def\toc@font#1{\ifcase#1\relax\bfseries\else\ifnum#1<0\large\bfseries
\else\rmfamily\fi\fi}
\def\@tocleaders#1{\hfil}
\def\@tocstyle#1{\toc@font{#1}}
\def\@tocpnumfmt#1#2{\hbox to\@pnumwidth{\hfil #2}}
\def\@ftocline#1{\@maketocline{#1}{\z@}{\unit@indent}}
\def\l@part{\ifnum\c@tocdepth>-2\relax
\addpenalty{-\@highpenalty}\addvspace{2.25em \@plus\p@}\fi
\@ftocline{-1}}
\def\l@chapter{\@ftocline{0}}
\def\l@section{\@ftocline{1}}
\def\l@subsection{\@ftocline{2}}
\def\l@subsubsection{\@ftocline{3}}
\def\l@paragraph{\@ftocline{4}}
\def\l@subparagraph{\@ftocline{5}}
\def\l@figure{\@ftocline{1}}
\or % oldtoc with dotfill
\def\toc@font#1{\ifcase#1\relax\bfseries\else\ifnum#1<0\large\bfseries
\else\rmfamily\fi\fi}
\def\@tocstyle#1{\toc@font{#1}}
\def\@tocpnumfmt#1#2{\hbox to\@pnumwidth{\hfil #2}}
\def\@tocleaders#1{\ifnum#1>0 \leaders\hbox{$\m@th \mkern \@dotsep mu.\mkern
\@dotsep mu$}\hfill\else\hfill\fi}
\def\l@part#1#2{\ifnum\c@tocdepth>-2\relax
\addpenalty{-\@highpenalty}\addvspace{2.25em \@plus\p@}\fi
\@maketocline{-1}{\z@}{3em}{#1}{#2}}
\def\l@chapter#1#2{\ifnum\c@tocdepth>\m@ne
\addpenalty{-\@highpenalty}\addvspace{1.0em \@plus\p@}\fi
\@maketocline{0}{\z@}{1.5em}{#1}{#2}\penalty\@highpenalty}
\def\l@section{\@maketocline{1}{1.5em}{2.3em}}
\def\l@subsection{\@maketocline{2}{3.8em}{3.2em}}
\def\l@subsubsection{\@maketocline{3}{7.0em}{4.1em}}
\def\l@paragraph{\@maketocline{4}{10em}{5em}}
\def\l@subparagraph{\@maketocline{5}{12em}{6em}}
\def\l@figure{\@maketocline{1}{1.5em}{2.3em}}
\or % oldtoc with no dotfill
\def\toc@font#1{\ifcase#1\relax\bfseries\else\ifnum#1<0\large\bfseries
\else\rmfamily\fi\fi}
\def\@tocstyle#1{\toc@font{#1}}
\def\@tocpnumfmt#1#2{\hbox to\@pnumwidth{\hfil #2}}
\def\@tocleaders#1{\hfill}
\def\l@part#1#2{\ifnum\c@tocdepth>-2\relax
\addpenalty{-\@highpenalty}\addvspace{2.25em \@plus\p@}\fi
\@maketocline{-1}{\z@}{3em}{#1}{#2}}
\def\l@chapter#1#2{\ifnum\c@tocdepth>\m@ne
\addpenalty{-\@highpenalty}\addvspace{1.0em \@plus\p@}\fi
\@maketocline{0}{\z@}{1.5em}{#1}{#2}\penalty\@highpenalty}
\def\l@section{\@maketocline{1}{1.5em}{2.3em}}
\def\l@subsection{\@maketocline{2}{3.8em}{3.2em}}
\def\l@subsubsection{\@maketocline{3}{7.0em}{4.1em}}
\def\l@paragraph{\@maketocline{4}{10em}{5em}}
\def\l@subparagraph{\@maketocline{5}{12em}{6em}}
\def\l@figure{\@maketocline{1}{1.5em}{2.3em}}
\fi
\endinput
%%
%% End of file `xtoc.r'.
