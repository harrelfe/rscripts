
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
$$\\begin{array}{ccc}

\\end\{eqnarray\}
\\end{array}$$NEWLINE

\\begin\{eqnarray\*\}
$$\\begin{array}{ccc}

\\end\{eqnarray\*\}
\\end{array}$$NEWLINE

\\beqa
$$\\begin{array}{ccc}

\\eeqa
\\end{array}$$NEWLINE

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
\\"\1\\"

\\ipacue
 BACKTICKr ipacue()BACKTICK 

\\def\\apacue\{(.*?)\}
<!--  BACKTICKr apacue <<- \1BACKTICK--->

\\sound\{(.*?)\}
NEWLINEBACKTICKr sound("\1")BACKTICK

\\soundm\{(.*?)\}
NEWLINEBACKTICKr sound("\1")BACKTICK

\\bmovie\{(.*?)\}
NEWLINEBACKTICKr bmovie(\1)BACKTICK

\\ddisc\{(.*?)\}
NEWLINEBACKTICKr ddisc(\1)BACKTICK

\\movie\{(.*?)\}
NEWLINEBACKTICKr movie("\1")BACKTICK

\\disc\{(.*?)\}
NEWLINEBACKTICKr disc("\1")BACKTICK

\\blog\{(.*?)\}
NEWLINEBACKTICKr blog("\1")BACKTICK

\\abd\{(.*?)\}
NEWLINEBACKTICKr abd("\1")BACKTICK

\\ems\{(.*?)\}
NEWLINEBACKTICKr ems("\1")BACKTICK
 
\\ros\{.*?\}


\\altman\{.*?\}


\\katz\{.*?\}


\\rms\{(.*?)\}
NEWLINEBACKTICKr bookref("RMS", "\1")BACKTICK

\\beqa
$$\\begin{array}{ccc}

\\item\[(.*?)\]
\\item <b>\1</b>

\Co\{
\co\{

\\co\{(.*?)\}
BACKTICK\1BACKTICK

\\textbf\{(.*?)\}
**\1**

^<<
BACKTICKBACKTICKBACKTICK{r 

>>=
}

^@$
BACKTICKBACKTICKBACKTICK

^\\chapter\{(.*?)\}
NEWLINE# \1

^\\section\{(.*?)\}
NEWLINE## \1

^\\subsection\{(.*?)\}
NEWLINE### \1

^\\subsubsection\{(.*?)\}
NEWLINE#### \1

\.\\ 
. 

%$


^%(.*)
<!-- \1--->

\\_
_

~\\cite
 \\cite

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

\\texttt\{(.*?)\}
<tt>\1</tt>

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
<div id="boxes"><div id="leftbox">\1</div><div id="rightbox">\2</div></div><div class="clear"></div>

\\ignore\{
<!--

.*end ignore$.*
--->

\\footnote\{(.*)\}
\^[\1]

\s*#\s*Fig.*\(\*\\ref\{fig*.*


~*\\ref\{fig:(.*?)\}
 \@ref(fig:\1)

\\a*label\{(.*?)\}
<a name="\1"></a>

~*\\ref\{(.*?)\}
<a href="`{r anchorLoc('\1')`">here</a>

<br>
\\\\

prType='latex'
prType='html'
