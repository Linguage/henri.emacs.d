\ProvidesClass{henri-diary}[2020/05/04~v1.0~线圈日记本]
\NeedsTeXFormat{LaTeX2e}

\LoadClass[a4paper]{article}
\RequirePackage[UTF8]{ctex}
\RequirePackage[landscape,twocolumn]{geometry}
\RequirePackage{tikz}
\RequirePackage{tikzpagenodes}
\RequirePackage{amssymb}
\usetikzlibrary{shadows,calc,positioning}

\geometry{left=3cm,right=3cm,top=5.5cm,bottom=3cm}
\setlength{\columnsep}{3cm}
\pagestyle{empty}
\linespread{1.18}
\AtBeginDocument{%
  \zihao{-4}%
  \setlength{\parskip}{0pt}%
  \setlength{\parindent}{2em}%
  \setlength{\topsep}{0pt}%
  \setlength{\partopsep}{0pt}%
  \setlength{\itemsep}{0pt}%
  \setlength{\parsep}{0pt}%
}
\def\@listi{%
  \leftmargin\leftmargini
  \topsep 0pt
  \parsep 0pt
  \itemsep 0pt}
\let\@listI\@listi
\@listi

% color
\definecolor{background-color}{HTML}{fffee8}
\definecolor{titlepage-color}{HTML}{fffbb4}
\definecolor{date-color}{HTML}{454545}
\definecolor{map-color}{HTML}{454545}
\definecolor{weather-color}{HTML}{454545}

% ================================  BACKGROUND  ================================
% https://tex.stackexchange.com/a/229947
\def\SpiralNumber{16} % 线圈数
\def\GridColor{black!60}  % 网格线颜色
\def\ShadowXshift{0.5ex}
\def\ShadowYshift{-0.5ex}

\tikzset{%
  spiral/.pic={
    \draw[
      draw=black,
      left color=black!70,
      right color=black!60,
      middle color=gray!40
      ]
      (-1.1,-0.35) rectangle ++(10pt,10pt);
    \draw[
      draw=black,
      left color=black!70,
      right color=black!60,
      middle color=gray!40
      ]
      (-1.77,-0.35) rectangle ++(10pt,10pt);
    \draw[
      double=gray!80,
      double distance=1pt,
      ]
      (-1,-0.2) arc (40:-220:10pt and 2pt);
    \draw[
      double=gray!80,
      double distance=1pt,
      ]
      (-1,-0.05) arc (40:-220:10pt and 2pt);
  }
}

\def\SpiralPosition{%
  ([xshift=36pt,yshift=3pt+2.5cm-\Valor*(\textheight+2.5cm)/(\SpiralNumber-1)]current page text area.north)
}
\RequirePackage{eso-pic}
\AddToShipoutPictureBG{%
  \begin{tikzpicture}[remember picture,overlay]
    % draw frame
    \draw[rounded corners=10pt,fill=background-color,drop shadow={shadow xshift=\ShadowXshift, shadow yshift=\ShadowYshift}]
      ([xshift=-30pt,yshift=20pt+2.5cm]current page text area.north west) rectangle
      ([xshift=-2pt,yshift=-20pt]current page text area.south);
    \draw[rounded corners=10pt,fill=background-color,drop shadow={shadow xshift=\ShadowXshift, shadow yshift=\ShadowYshift}]
      ([xshift=30pt,yshift=-20pt]current page text area.south east) rectangle
      ([xshift=2pt,yshift=20pt+2.5cm]current page text area.north);
    % draw lines
    \draw[\GridColor,opacity=0.3,shift={(0,0.62\baselineskip)}]
          ([xshift=-30pt]current page text area.north west) grid[step=\baselineskip,xstep=0]
          ([xshift=-2pt]current page text area.south);
    \draw[\GridColor,opacity=0.3,shift={(0,0.62\baselineskip)}]
          ([xshift=30pt]current page text area.south east) grid[step=\baselineskip,xstep=0]
          ([xshift=2pt]current page text area.north);
    % top thick line
    \draw[black!70,line width=1pt]
          ([xshift=-30pt,yshift=0.7em]current page text area.north west) --
          ([xshift=-2pt,yshift=0.7em]current page text area.north);
    \draw[black!70,line width=1pt]
          ([xshift=30pt,yshift=0.7em]current page text area.north east) --
          ([xshift=2pt,yshift=0.7em]current page text area.north);
    % draw middle spiral
    \foreach \Valor in {0,1,...,\numexpr\SpiralNumber-1\relax}
      \pic at \SpiralPosition {spiral};
  \end{tikzpicture}
}


% ===================================  ICON  ===================================
\newfontfamily\iconfont[Path=.henri-latex-diary-fonts/,Extension=.ttf]{icofont}

% map icon
\def\map{{\iconfont^^^^ef79}}

% weather icon
\def\sun{{\iconfont^^^^ee81}}
\def\sunny{{\iconfont^^^^ee83}}
\def\hot{{\iconfont^^^^ee7e}}

\def\rainy{{\iconfont^^^^ee6a}}
\def\rainySunny{{\iconfont^^^^ee68}}
\def\rainyThunder{{\iconfont^^^^ee69}}
\def\thunderLight{{\iconfont^^^^ee84}}

\def\windy{{\iconfont^^^^ee9f}}
\def\clouds{{\iconfont^^^^ee51}}

\def\snow{{\iconfont^^^^ee6e}}
\def\snowy{{\iconfont^^^^ee7d}}
\def\snowyRainy{{\iconfont^^^^ee73}}

\def\hail{{\iconfont^^^^ee61}}
\def\dust{{\iconfont^^^^ee53}}

% emotion icon
\def\confused{{\iconfont^^^^eaf7}}
\def\sad{{\iconfont^^^^eb01}}
\def\simpleSmile{{\iconfont^^^^eb02}}
\def\slightlySmile{{\iconfont^^^^eb03}}
\def\angry{{\iconfont^^^^eaf4}}
\def\astonished{{\iconfont^^^^eaf5}}
\def\confounded{{\iconfont^^^^eaf6}}
\def\crying{{\iconfont^^^^eaf8}}
\def\dizzy{{\iconfont^^^^eaf9}}
\def\expressionless{{\iconfont^^^^eafa}}
\def\heartEyes{{\iconfont^^^^eafb}}
\def\laughing{{\iconfont^^^^eafc}}
\def\nerdSmile{{\iconfont^^^^eafd}}
\def\openMouth{{\iconfont^^^^eafe}}
\def\rage{{\iconfont^^^^eaff}}
\def\rollingEyes{{\iconfont^^^^eb00}}
\def\worried{{\iconfont^^^^eb07}}
\def\winkSmile{{\iconfont^^^^eb06}}
\def\stuckOutTongue{{\iconfont^^^^eb05}}
\def\smirk{{\iconfont^^^^eb04}}

% ===============================  COMMAND DAY  ================================
\newcommand\diary@numToMonth[1]{%
  \ifcase#1\or
    January\or February\or March\or April\or May\or June\or
    July\or August\or September\or October\or November\or December\fi
}

% default value
\def\diary@province{河南}
\def\diary@city{濮阳}
\def\diary@weather{\sunny}
\def\diary@weatherName{晴}
\def\diary@beginYear{}
\def\diary@beginMonth{}
\def\diary@beginDay{}
\def\diary@endYear{}
\def\diary@endMonth{}
\def\diary@endDay{}
\def\@title{你忘记写题目了}
\def\@author{张丘洋}

\newcommand\MoveTo[2][]{%
  \def\diary@province{#1}%
  \def\diary@city{#2}%
}%
\newcommand\Weather[2][\sunny]{%
  \def\diary@weather{#1}%
  \def\diary@weatherName{#2}%
}%

\newcommand\BeginAt[3][]{%
  \def\diary@beginYear{#1}%
  \def\diary@beginMonth{#2}%
  \def\diary@beginDay{#3}%
}
\newcommand\EndAt[3][]{%
  \def\diary@endYear{#1}%
  \def\diary@endMonth{#2}%
  \def\diary@endDay{#3}%
}

\newcounter{diary}

\RequirePackage{xparse}
\NewDocumentCommand{\Day}{m m O{}}{%
  \ifx#3\empty%
  \else%
    \def\diary@weather{#3}%
  \fi%
  \stepcounter{diary}%
  \newpage%
  \begin{tikzpicture}[remember picture,overlay,black!80!yellow]
    \if@firstcolumn%
      \node[anchor=north east] at ([xshift=-2.5em,yshift=2.5cm]current page text area.north) (day) {\color{date-color}\fontsize{50}{60}\selectfont #2};%
      \draw[line width=2pt,date-color] ($(day.north west)+(-8pt,0)$) -- ($(day.south west)+(-8pt,0)$);%
      \node[anchor=north east,left=16pt of day,yshift=9pt] (month) {\color{date-color}\zihao{4}#1 月};%
      \node[anchor=south east,left=16pt of day,yshift=-9pt]  (month) {\color{date-color}\zihao{4}\diary@numToMonth{#1}};%
      \node[left=6.9cm of day.east,anchor=center] (location) {\color{map-color}\map\ \ifx\diary@province\empty\else\diary@province{} • \fi\diary@city};%
      \node[left=9.7cm of day.east] {\color{weather-color}\huge\diary@weather};%
      \node at ($(current page text area.south)+(-0.25\textwidth-16pt,-4pt)$) {\color{date-color}第\thediary 篇};%
    \else%
      \node[anchor=north west] at ([xshift=2.5em,yshift=2.5cm]current page text area.north) (day) {\color{date-color}\fontsize{50}{60}\selectfont #2};%
      \draw[line width=2pt,date-color] ($(day.north east)+(8pt,0)$) -- ($(day.south east)+(8pt,0)$);%
      \node[anchor=north west,right=16pt of day,yshift=9pt] (month) {\color{date-color}\zihao{4}#1 月};%
      \node[anchor=south west,right=16pt of day,yshift=-9pt]  (month) {\color{date-color}\zihao{4}\diary@numToMonth{#1}};%
      \node[right=6.9cm of day.west,anchor=center] (location) {\color{map-color}\map\ \ifx\diary@province\empty\else\diary@province{} • \fi\diary@city};%
      \node[right=9.7cm of day.west] {\color{weather-color}\huge\diary@weather};%
      \node at ($(current page text area.south)+(0.25\textwidth+16pt,-4pt)$) {\color{date-color}第\thediary 篇};%
    \fi%
  \end{tikzpicture}%
}%

% 标题页字体
\def\diary@differentTitleCJKFont{}
\def\diary@differentTitleFont{}
\newcommand\TitleCJKFont[1]{%
  \newCJKfontfamily\diary@titleCJKfont{#1}%
  \def\diary@differentTitleCJKFont{1}%
}%
\newcommand\TitleFont[1]{%
  \newfontfamily\diary@titlefont{#1}%
  \def\diary@differentTitleFont{1}%
}%

% titlepage
\renewcommand\maketitle{%
  \begingroup
    \ifx\diary@differentTitleCJKFont\empty\else\diary@titleCJKfont\fi%
    \ifx\diary@differentTitleFont\empty\else\diary@titlefont\fi%
    \par\noindent{\zihao{2}\bfseries \@title}\par%
    \vspace{.45\baselineskip}%
    \noindent{\zihao{5}\@author\hfill\@date}\par%
    \vspace{.8\baselineskip}%
  \endgroup
}%

% =============================== ORG JOURNAL ================================
% Adapter commands for Org's section tree.  Org journal files use a monthly
% top-level heading and daily second-level headings like "2026-05-04 Monday".
\newcommand\OrgDiaryMonth[1]{%
  \par\medskip{\centering\zihao{3}#1\par}\smallskip%
}

\def\OrgDiaryDay#1{%
  \expandafter\OrgDiaryDayParse#1 \@nil%
}

\def\OrgDiaryDayParse#1-#2-#3 #4\@nil{%
  \Day{#2}{#3}%
}

\newcommand\OrgDiaryLocation[2][]{%
  \MoveTo[#1]{#2}%
}

\newcommand\OrgDiaryWeather[2][\sunny]{%
  \Weather[#1]{#2}%
}

\newcommand\OrgDiarySubheading[1]{%
  \par\smallskip{\zihao{-4}\bfseries #1}\par\nobreak%
}
