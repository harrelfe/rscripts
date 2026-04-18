# `prprint` Python Script

This script pretty-prints R code with color code highlighting, line numbers, and identifiers for beginning and end of code blocks containing {}.

Download the script from [here](https://raw.githubusercontent.com/harrelfe/rscripts/refs/heads/master/prprint).

The script was developed by a [long interaction](https://claude.ai/share/f8e5a434-1146-4f9e-aaab-e59531fae476) with Claude AI to work on MacOS.  It may work on other operating systems.

Store the script in a location that is in your shell's path, such as `~/bin` or `/usr/local/bin`.  Then run it like this from the command line:

`prprint myfile.r`

On Mac this pops up a `Preview` window that can then be printed on a color printer.

![](https://github.com/harrelfe/rscripts/blob/master/prprint.png)

To get a symbol table with line numbers at the end of the PDF listing use

`prprint myfile.r st`

![](https://github.com/harrelfe/rscripts/blob/master/prprint-st.png)
