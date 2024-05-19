# GQL ANTLR (gql-antlr)
This repository contains a language-independent [ANTLR](https://www.antlr.org/) grammar for GQL. GQL is the property graph query language defined and published by [ISO in April 2024](https://www.iso.org/standard/76120.html).

Using the ANTLR tools, language-dependent parsers can be generated from the grammar in this repository.

Typically, the lexer rules and parser rules are contained in separate files. e.g. GQLLexer.g4 and GQLParser.g4. But there is currently a bug in the JetBrains ANTLR4 plugin, that manifests itself when the GQL lexer and parser rules are in two separate files. The ANTLR plugin for JetBrains is extremely useful, during the development of the grammar and development of implementations that use the grammar, so they've been combined into a single file: GQL.g4. The lexer rules are at the bottom of the file.

## Creation of the ANTLR Grammar File
An initial version of the grammar file was generated using [gramgen](https://github.com/mburbidg/gramgen), a command line program that generates ANTLR parser and lexer grammars from the XML representation of the BNF for GQL. The XML file is one of the artifacts of the ISO Specification.

This generated version was then hand tweaked to remove ambiguities present in the BNF contained in the ISO Specification. e.g. mutually left recursive productions. The main focus of the changes were related to the _value expression_ and _primary value expression_ productions.

## Alternative Labels

[Alternative labels](https://github.com/antlr/antlr4/blob/master/doc/parser-rules.md#alternative-labels) can be used to associate a label with each top-level alternative of a rule. This has the effect of creating Visitor and Listener methods for each alternative. It also eliminates the need to include code in the parent rule visitor or listener method to determine which alternative was matched. But it has the downside of proliferating the number of visitor and listener methods.

The following guidelines are used in the ANTLR GQL grammar to reach a balance between convenience and method
proliferation.

If each top-level alternative of a rule contains a single non-terminal or terminal rule then alternative labels are not used. In this case, it is very straight forward to determine which alternative was matched and requires less code than if a visitor method is generated for each alternative. Consider the following rule:
```
primitiveDataModifyingStatement
    : insertStatement
    | setStatement
    | removeStatement
    | deleteStatement
    ;
```
In Go the visitor method for primitiveDataModifyingStatement is as follows:
```
func (v *Visitor) VisitPrimitiveDataModifyingStatement(ctx *gen.PrimitiveCatalogModifyingStatementContext) interface{} {
	switch t := ctx.GetChild(0).(type) {
	case *gen.InsertStatementContext:
		return t.Accept(v)
	case *gen.SetStatementContext:
		return t.Accept(v)
	case *gen.RemoveStatementContext:
		return t.Accept(v)
	case *gen.DeleteStatementContext:
		return t.Accept(v)
	default:
		return nil
	}
}
```
This is very straight forward and cleaner than if alternative labels had been used and instead there were 4 separate methods for handling primitiveDataModifyingStatement.

If alternative labels were used in this case, one of the biggest challenges is to come up with names that read well and don't conflict with other names. The labels names are used directly as the name of the visitor method for that alternative. As an example, _insertStatement_ cannot be used as the label name because that name is already taken by the _insertStatement_ rule and corresponding visitor method. Something like _insertStatementAlt_ could be used. But now there are two visitor methods, one named _VisitInsertStatementAlt_ and one named _VisitInsertStatement_. It becomes very cluttered and is one of the reasons why for rules that contain simple alternatives, labels are not used.

If each top-level alternative of a rule is simple enough that it is easy to determine which alternative was matched, labels are not used. Here's an example that fits into this category:
```
optionalOperand
    : simpleMatchStatement
    | LEFT_BRACE matchStatementBlock RIGHT_BRACE
    | LEFT_PAREN matchStatementBlock RIGHT_PAREN
    ;
```
If it is more difficult to determine which alternative was matched in complex rules then alternative labels are used. Consider the following example:
```
labelExpression
    : EXCLAMATION_MARK labelExpression                  #labelExpressionNegation
    | labelExpression AMPERSAND labelExpression         #labelExpressionConjunction
    | labelExpression VERTICAL_BAR labelExpression      #labelExpressionDisjunction
    | labelName                                         #labelExpressionName
    | PERCENT                                           #labelExpressionWildcard
    | LEFT_PAREN labelExpression RIGHT_PAREN            #labelExpressionParenthesized
    ;
```
In addition to eliminating the complexity of determining which alternative was matched, label naming in these cases adds to the clarity of visitor and listeners. There's no label name collisions here and the label names describe the meaning of the alternative.

Whether to use label names or not is a judgement call. But these guidelines describe the reasoning behind alternative label names in the GQL grammar.

## Permissiveness
The Grammar itself is more permissive than a GQL implementation would be. Type checking and other semantic analysis would be required as post-processing steps to parsing. Most of the _Syntax Rules_ defined by the specification would be enforced by the semantic analysis step.

## Testing and Verification
Not a lot of testing has been done to date, besides spot checking using various hand generated snippets of GQL. In the future this grammar will be validated using a [Test Compatibilty Kit (TCK)](https://en.wikipedia.org/wiki/Technology_Compatibility_Kit) for GQL.

