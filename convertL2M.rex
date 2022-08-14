
\\hxref\{.*\}


^\\need.*


\\ra
$\\rightarrow$

\\\$
$

\\begin\{equation\}
$$

\\end\{equation\}
$$

\\begin\{equation\*\}
$$

\\end\{equation\*\}
$$

\\beq$
$$

\\eeq$
$$

\\begin\{eqnarray\}
NEWLINE\\begin{array}{ccc}

\\end\{eqnarray\}
\\end{array}NEWLINE

\\begin\{eqnarray\*\}
NEWLINE\\begin{array}{ccc}

\\end\{eqnarray\*\}
\\end{array}NEWLINE

\\beqa
NEWLINE\\begin{array}{ccc}

\\eeqa
\\end{array}NEWLINE

\\begin\{itemize\}
\\bi

\\end\{itemize\}
\\ei

\\begin\{enumerate\}
\\be

\\end\{enumerate\}
\\ee

\\bd
\\bi

\\ed
\\ei

\\begin\{description\}
\\bi

\\end\{description\}
\\ei

\\btable\{
\\begin{tabular

\\etable
\\end{tabular}

\\ctabl\{
\\begin{tabular

\\ectabl
\\end{tabular}

\\clearpage


\\newpage


\\centerline\{(.*)\}
\1

\\includegraphics.*\{(.*?)\}
<img src="\1" width="50%">

\\R\}
BACKTICKRBACKTICK}

\\R\\
BACKTICKRBACKTICK

\\R
BACKTICKRBACKTICK

``(.*?)''
"\1"

\\ipacue
 BACKTICKr ipacue()BACKTICK 

\\def\\apacue\{(.*?)\}


\\sound\{(.*?)\}
NEWLINEBACKTICKr mrg(sound("\1"))BACKTICK

\\soundm\{(.*?)\}
NEWLINEBACKTICKr mrg(sound("\1"))BACKTICK

\\bmovie\{(.*?)\}
NEWLINEBACKTICKr mrg(bmovie(\1))BACKTICK

\\ddisc\{(.*?)\}
NEWLINEBACKTICKr mrg(ddisc(\1))BACKTICK

\\movie\{(.*?)\}
NEWLINEBACKTICKr mrg(movie("\1"))BACKTICK

\\disc\{(.*?)\}


\\blog\{(.*?)\}
NEWLINEBACKTICKr mrg(blog("\1"))BACKTICK

\\blogl\{(.*?)\}\{(.*?)\}
NEWLINEBACKTICKr mrg(blogl("\1", "\2"))BACKTICK

\\abd\{(.*?)\}
NEWLINEBACKTICKr mrg(abd("\1"))BACKTICK

\\ems\{(.*?)\}
NEWLINEBACKTICKr mrg(ems("\1"))BACKTICK
 
\\ros\{.*?\}


\\altman\{.*?\}


\\katz\{.*?\}


\\rms\{(.*?)\}
NEWLINEBACKTICKr bookref("RMS", "\1")BACKTICK

\\beqa
$$\\begin{array}{ccc}

\\item\[(.*?)\]
\\item <b>\1</b>

\\Co\{
\\co\{

\Co\{
\co\{

\\\\co\{(.*?)\}
BACKTICK\1BACKTICK

\\co\{(.*?)\}
BACKTICK\1BACKTICK

\co\{(.*?)\}
BACKTICK\1BACKTICK

\\fun\{(.*?)\}


\\funp\{(.*?)\}


\\textbf\{(.*?)\}
**\1**

^<<
NEWLINEBACKTICKBACKTICKBACKTICK{r 

>>=
}

^@$
BACKTICKBACKTICKBACKTICKNEWLINE

^\\chapter\{(.*?)\}
NEWLINE# \1

^\\section\{(.*?)\}
NEWLINE## \1

^\\subsection\{(.*?)\}
NEWLINE### \1

^\\subsubsection\{(.*?)\}
NEWLINE#### \1

\\paragraph\{(.*?)\}
**\1**

\.\\ 
. 

%$


^%(.*)
<!-- \1--->

\\_
_

~\\cite
 \\cite
 
\\citei
\\cite

\\cite\[(.*?)\]\{(.*?)\}
[\2, \1]

\\cite\{(.*?)\}
@\1

\\begin\{verbatim\}
<pre>

\\end\{verbatim\}
</pre>

\\verb\|(.*?)\|
<tt>\1</tt>

\\href\{(.*?)\}\{(.*?)\}
[\2](\1)

\\url\{(.*?)\}
[hhhhHHHH\1](\1)

hhhhHHHHhttp://


hhhhHHHHhttps://


hhhhHHHH


\\emph\{(.*?)\}
_\1_

\{\\em\s*(.*?)\}
_\1_

\\texttt\{(.*?)\}
`\1`

\{\\larger(.*?)\}
\1

\{\\smaller(.*?)\}
<small>\1</small>
w
\{\\small

w
\{\\large

w
\\begin\{

w
\\end\{

w
^\{\\


\\bigskip


\\smallskip


\\quoteit\{(.*?)\}\{(.*?)\}
`r quoteit("\1", "\2")`

\\ignore\{
<!--

.*end ignore$.*
--->

\\footnote\{(.*)\}
\^[\1]

#\s*Fig.*\s\\(\\*.*\\*\\)


\s*#\s*Fig.*\(\*\\ref\{fig*.*


Figure\s*~*\s*\\ref\{fig
\\ref\{fig

~*\\ref\{fig:(.*?)\}
 \@fig-\1

\\a*label\{(.*?)\}
 {#sec-\1}

~*\\ref\{(.*?)\}
 \@sec-\1

\\\\
<br>

prType='latex'
prType='html'

^\\L$


^\\endL$

