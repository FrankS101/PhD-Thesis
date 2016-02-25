# Compilation in Ubuntu

1.  Installation of the necessary packages:
<pre><code>sudo apt-get install texlive </code></pre>
2. (Optional) If you are not going to write your dissertation in English (or some part must be in another language), 
you will also need to install language and font packages, for example: 
<pre><code>sudo apt-get install texlive-lang-spanish</code></pre>
3. Editor: I strongly recommend using a latex environment to edit your files, specially if you are a starting with latex.
TexMaker works pretty good. 
<pre><code>sudo apt-get install texmaker</code></pre>
4. Compilation: The pdf document can be created using compile.sh script. The script compiles the latex files and bibtex references. 
<pre><code>./compile.sh</code></pre>

# Copyright

This file belongs to the PhD titled "New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems", 
distributed under the Creative Commons Licence Attribution-NonCommercial-NoDerivs 3.0, available in http://creativecommons.org/licenses/by-nc-nd/3.0/. 

This thesis has been written adapting the TeXiS template, a LaTeX template for writting thesis and other documents. 
The complete TeXiS package can be obtained from http://gaia.fdi.ucm.es/projects/texis/. 
TeXis is distributed under the same conditions of the LaTeX Project Public License (http://www.latex-project.org/lppl.txt). 
The complete license is available in http://creativecommons.org/licenses/by-sa/3.0/legalcode 
