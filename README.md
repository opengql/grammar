# GQL ANTLR (gql-antlr)
This repository contains a language-independent [ANTLR](https://www.antlr.org/) grammar for GQL. GQL is the property graph query language defined and published by [ISO in April 2024](https://www.iso.org/standard/76120.html).

Using the ANTLR tools, language-dependent parsers can be generated from the grammar in this repository.

Typically, the lexer rules and parser rules are contained in separate files. e.g. GQLLexer.g4 and GQLParser.g4. But there is currently a bug in the JetBrains ANTLR4 plugin, that manifests itself when the GQL lexer and parser rules are in two separate files. The ANTLR plugin for JetBrains is extremely useful, during the development of the grammar and development of implementations that use the grammar, so they've been combined into a single file: GQL.g4. The lexer rules are at the bottom of the file.

## Creation of the ANTLR Grammar File
An initial version of the grammar file was generated using [gramgen](https://github.com/mburbidg/gramgen), a command line program that generates ANTLR parser and lexer grammars from the XML representation of the BNF for GQL. The XML file is one of the artifacts of the ISO Specification.

This generated version was then hand tweaked to remove ambiguities present in the BNF contained in the ISO Specification. e.g. mutually left recursive productions. The main focus of the changes were related to the _value expression_ and _primary value expression_ productions.

## Permissiveness
The Grammar itself is more permissive than a GQL implementation would be. Type checking and other semantic analysis would be required as post-processing steps to parsing. Most of the _Syntax Rules_ defined by the specification would be enforced by the semantic analysis step.

## Testing and Verification
Not a lot of testing has been done to date, besides spot checking using
various hand generated snippets of GQL. In the future this grammar will be
validated using a [Test Compatibilty Kit (TCK)](https://en.wikipedia.org/wiki/Technology_Compatibility_Kit) for GQL.

