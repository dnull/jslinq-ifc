# JSLINQ Implementation

## What is this?
This software is a research prototype accompanying an academic paper published in 2016 ([PDF](https://www.csc.kth.se/~musard/research/pubs/Codaspy16.pdf), [DOI](https://doi.org/10.1145/2857705.2857717)):

> M. Balliu and B. Liebe and D. Schoepe and A. Sabelfeld. JSLINQ: Building Secure Applications across Tiers. In Proceedings of the ACM Conference on Data and Applications Security and Privacy (CODASPY'16), New Orleans, LA, March 2016.

The idea is to statically verify that all possible information flows in a given program are not violating a defined policy. The supported language is a subset of F# and allows the implementation of three-tier web applications. Frontend code, business logic and database queries are all expressed in the same language. This is made possible by using WebSharper for the frontend and business logic, while database queries are expressed using LINQ.

## How to Build
This version uses updates components and is compatible with .NET Core, which is in contrast to the original version which required VisualStudio 2013. If .NET Core ist present, it can be build with the following steps:

 1. `dotnet restore`
 1. `dotnet build`

## Usage and Examples
The [original website](https://sites.google.com/site/jslinqcodaspy16/) created for the paper contains descriptions of the examples and instructions on how the syntax and usage of the tool.
