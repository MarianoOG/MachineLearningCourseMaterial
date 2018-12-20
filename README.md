# Machine Learning Course Material

The project was done for the Artificial Intelligence laboratory of the Computer Research Center (CIC-IPN) for the "Machine Learning" subject taught by PhD. Salvador Godoy Calderón.

## Structure

The project was carried out in the Lisp programming language. The project contains several function packages, example algorithms and problem files.

### Function packages

This material is organized in four main packages: *pkgLogic*, *pkgInduction*, *pkgReinforcement* and *pkgAuxiliar*. The first three being directly related to the algorithms and the last one is complementary.

* *pkgInduction*, for handling the sample tables.
* *pkgLogic*, for logical expressions.
* *pkgReinforcement*, for the functions of reinforcement learning algorithms.
* *pkgAuxiliar*, for the other support functions.

### Example algorithms

Two sample programs were created to demonstrate the usefulness of these packages: the *star algorithm*, which mainly uses the functions of logical expressions and the *qlearning algorithm*, which mainly uses the functions of the learning package by reinforcement.

### Problem files

Two types of files were generated that contain the prior knowledge of the problems to be solved. One of them, *.cet* (Class Example Tables) files, contain a table with examples where the information of each class is found along with their attributes. The other type of file is *.rt* (Reinforcement Tables) where three sub-type of problems can be found: labyrinths, reinforcement tables and transitions tables. Each one defines in a different way the problem and rewards that are obtained when changing from one state to another.

## Purpose

These packages were created with the purpose to save time to students so they can focus their energy on understanding and generating new alternatives to algorithms instead of concentrating on the repetitive tasks that are inherent in their creation.

The ideal use of this work is to enable the students create new algorithms using the function packages as a start, test them against the problems and modify the algorithms based on their findings.
