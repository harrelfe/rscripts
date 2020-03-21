
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


`(.*?)'
\\'\1\\'

``(.*?)''
\\"\1\\"

\\ipacue
 `r apacue <- apacue + 1; ipacue(apacue)` 

\\def\\apacue\{(.*?)\}
 `r apacue <- 0`

\\sound\{(.*?)\}
NEWLINE`r sound("\1")`

\\bmovie\{(.*?)\}
NEWLINE`r bmovie(\1)`

\\ddisc\{(.*?)\}
NEWLINE`r ddisc(\1)`

\\movie\{(.*?)\}
NEWLINE`r movie("\1")`

\\disc\{(.*?)\}
NEWLINE`r disc("\1")`

\\blog\{(.*?)\}
NEWLINE`r blog("\1")`

\\abd\{(.*?)\}
NEWLINE`r abd("\1")`

\\ems\{(.*?)\}
NEWLINE`r ems("\1")`
 
\\ros\{.*?\}


\\altman\{.*?\}


\\katz\{.*?\}


\\rms\{(.*?)\}
NEWLINE`r bookref("RMS", "\1")`

\\beqa
$$\\begin{array}{ccc}

\\item\[(.*?)\]
\\item <b>\1</b>

\Co\{
\co\{

[\\]\\co\{(.*?)\}
`\1`

\\textbf\{(.*?)\}
<b>\1</b>

^<<
```{r 

>>=
}

^@$
```

^\\chapter\{(.*)\}
NEWLINE# \1

^\\section\{(.*)\}
NEWLINE## \1

^\\subsection\{(.*)\}
NEWLINE### \1

^\\subsubsection\{(.*)\}
NEWLINE#### \1

\.\\ 
. 

%$


^%(.*)
<!-- \1-->

~\\cite
 \\cite

\\cite\{(.*?)\}
@\1

\\href\{(.*?)\}\{(.*?)\}
[\2](\1)

\\emph\{(.*?)\}
<em>\1</em>

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

\\label\{(.*?)\}
 {#\1}

\\footnote\{(.*)\}
\^[\1]

\s*#\s*Fig.*\(\*\\ref\{fig*.*


~*\\ref\{(.*?)\}
 \\@ref(\1)

<br>
\\\\

prType='latex'
prType='html'
