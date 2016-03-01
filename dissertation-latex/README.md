# Compilation in Ubuntu

1.  Installation of the necessary packages:
<pre><code>sudo apt-get install texlive </code></pre>
2. (Optional) If you are not going to write your dissertation in English (or some part must be in another language), 
you will also need to install language and font packages, for example: 
<pre><code>sudo apt-get install texlive-lang-spanish</code></pre>
3. Compilation: The pdf document can be created using the Makefile, which compiles the latex files and bibtex references. After that, it removes all latex temporary files.
<pre><code>make</code></pre>
In order to have timestamp-based snapshots of the entire folder via the Makefile: 
<pre><code>make snapshot</code></pre>
4. Editor: I strongly recommend using a latex environment to edit your files, specially if you are starting with latex. I like emacs, but TexMaker also works pretty good. 
<pre><code>sudo apt-get install texmaker</code></pre>

# Copyright

This file belongs to the PhD titled "New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems", 
distributed under the Creative Commons Licence Attribution-NonCommercial-NoDerivs 3.0, available in http://creativecommons.org/licenses/by-nc-nd/3.0/. 

This thesis has been written adapting the TeXiS template, a LaTeX template for writting thesis and other documents. 
The complete TeXiS package can be obtained from http://gaia.fdi.ucm.es/projects/texis/. 
TeXis is distributed under the same conditions of the LaTeX Project Public License (http://www.latex-project.org/lppl.txt). 
The complete license is available in http://creativecommons.org/licenses/by-sa/3.0/legalcode 

_Contact:_ Francisco J. Pulido (@FrankS101, <francis@lcc.uma.es>)

Please cite using the following BibTex entry:

```
@phdthesis{pulido2015phd,
  title  = {New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems},
  author = {Pulido, Francisco-Javier},
  school = {University of M\'{a}laga, Spain},
  year   = 2015,
  Url    = {http://riuma.uma.es/xmlui/handle/10630/10204}
}
```
