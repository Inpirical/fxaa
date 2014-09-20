Long-Term FX Momentum (FXAA)
====

This contains the R code used in the analysis of long-term FX momentum trading strategies found at https://fxaa.inpirical.com.

The main reasons for releasing the code are:
- methodoloogical transparency,
- inviting community contributions,
- to permit building on this codebase to perform other financial / economic analysis.

The code is not released as an R package, but rather as several functions libraries, and will require some familiarity with R to put to use in local applications.

Structure
-----
- "data\_functions.R" contains functions used to process currency price data.
- "core\_trading.R" contains core trading functions for implementing momentum strategies.
- "applied\_trading.R" contains applications of the core momentum trading functions.
- "svm.R" contains functions relating to the training and use of support vector machines for trading currencies.

Documentation
----
Functions are documented in-line. Questions about the code can also be posted at https://answers.inpirical.com using tags #FXAA and #code.

License
----
The code has been released under the MIT license.

Contact
----
coder@inpirical.com
