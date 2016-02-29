# Why this can be useful to me?

Become a PhD in Computer Science (or any other field) is never an easy path. I believe Open Research will make science more transparent and reproducible. Have you ever seen a new algorithm or technique and wanted to compare it to your own approach? The aim of publishing the dissertation, source code and even the presentation I used to defend my PhD has a twofold purpose, on one hand, provide an example of a finished PhD to those who can benefit from it, and on the other hand, pave the way for further improvements to the papers that have resulted in this thesis. Having already the source code may help you to extend, improve, criticize or learn from this work.

# Why have I used Lisp?

The LISP programming language was devised as the programming language for Artificial Intelligence research. Nowadays, the functional programming is getting pretty popular, being adopted by the mainstream programming languages such as Java, C++ or Python (actually, Python supports all of Lisp's essential features except Macros). So, why not functional programming? You will learn a new programming paradigm, protoype fast, write short and concise code, and along the way, become a better programmer in languages you already know.


# Compilation

This repository provides the necessary source code and benchmarks to carry out the research corresponding to the PhD titled: "New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems" defended on the University of Málaga in July, 2015. The code is implemented in  Lispworks, a commercial implementation of Common Lisp. The code compilation has been tested on LispWorks 6 under Ubuntu 15.04 and Windows 7. Since no specific Lispworks classes were used, the source code "should" also be able to be compiled under other open source LispWorks dialects. Please refer to the wiki section to see more detailed information about how to compile the source code in Lispworks and run the benchmarks.

_Note:_ In Ubuntu you might need to install libgtk2.0-0:i386, because Lispworks is not 64bit compilant and needs i386 version of gtk2 libraries.

<pre><code>sudo apt-get install libgtk2.0-0:i386 </code></pre>

# Cite this work

This work has been published in the following research journals:

- Pulido, F. J., Mandow, L., & Pérez de la Cruz, J.L. (2014). Multiobjective shortest path
problems with lexicographic goal-based preferences. European Journal of Operational
Research, 239(1), 89–101.
- Pulido, F. J., Mandow, L., & Pérez de la Cruz, J.L. (2015). Dimensionality reduction in
multiobjective shortest path search. Computers & Operations Research, 64, 60–70.

and the PhD dissertation can be cited using the following BibTex entry:

```
@phdthesis{pulido2015phd,
  title  = {New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems},
  author = {Pulido, Francisco-Javier},
  school = {University of M\'{a}laga, Spain},
  year   = 2015,
  Url    = {http://riuma.uma.es/xmlui/handle/10630/10204}
}
```

# Contributing

Contributions are always encouraged! Some interesting things I can think of:

Bring the multiobjective algorithms to open source route planning applications like OSRM, GraphHopper or Valhalla.
Port this code to another Lisp dialect or programming language.
 
If you like it, give it a Star ;).

# Copyright

Copyright 2015 Francisco Javier Pulido Arrebola

This source code belongs to the PhD titled "New Techniques and Algorithms for Multiobjective and Lexicographic Goal-Based Shortest Path Problems", 
distributed under the Creative Commons Licence Attribution-NonCommercial-NoDerivs 3.0, available in http://creativecommons.org/licenses/by-nc-nd/3.0/. 

This source code is licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

_Contact:_ Francisco J. Pulido (@FrankS101, <francis@lcc.uma.es>)


