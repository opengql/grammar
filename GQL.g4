grammar GQL;

options { caseInsensitive = true; }

gqlProgram
   : programActivity sessionCloseCommand?
   | sessionCloseCommand
   ;

programActivity
   : sessionActivity
   | transactionActivity
   ;

sessionActivity
   : sessionResetCommand+
   | sessionSetCommand+ sessionResetCommand*
   ;

transactionActivity
   : startTransactionCommand (procedureSpecification endTransactionCommand?)?
   | procedureSpecification endTransactionCommand?
   | endTransactionCommand
   ;

endTransactionCommand
   : rollbackCommand
   | commitCommand
   ;

sessionSetCommand
   : SESSION SET (sessionSetSchemaClause | sessionSetGraphClause | sessionSetTimeZoneClause | sessionSetParameterClause)
   ;

sessionSetSchemaClause
   : SCHEMA schemaReference
   ;

sessionSetGraphClause
   : PROPERTY? GRAPH graphExpression
   ;

sessionSetTimeZoneClause
   : TIME ZONE setTimeZoneValue
   ;

setTimeZoneValue
   : timeZoneString
   ;

sessionSetParameterClause
   : sessionSetGraphParameterClause
   | sessionSetBindingTableParameterClause
   | sessionSetValueParameterClause
   ;

sessionSetGraphParameterClause
   : PROPERTY? GRAPH sessionSetParameterName optTypedGraphInitializer
   ;

sessionSetBindingTableParameterClause
   : BINDING? TABLE sessionSetParameterName optTypedBindingTableInitializer
   ;

sessionSetValueParameterClause
   : VALUE sessionSetParameterName optTypedValueInitializer
   ;

sessionSetParameterName
   : (IF NOT EXISTS)? sessionParameterSpecification
   ;

sessionResetCommand
   : SESSION RESET sessionResetArguments?
   ;

sessionResetArguments
   : ALL? (PARAMETERS | CHARACTERISTICS)
   | SCHEMA
   | PROPERTY? GRAPH
   | TIME ZONE
   | PARAMETER? sessionParameterSpecification
   ;

sessionCloseCommand
   : SESSION CLOSE
   ;

sessionParameterSpecification
   : GENERAL_PARAMETER_REFERENCE
   ;

startTransactionCommand
   : START TRANSACTION transactionCharacteristics?
   ;

transactionCharacteristics
   : transactionMode (COMMA transactionMode)*
   ;

transactionMode
   : transactionAccessMode
   ;

transactionAccessMode
   : READ ONLY
   | READ WRITE
   ;

rollbackCommand
   : ROLLBACK
   ;

commitCommand
   : COMMIT
   ;

nestedProcedureSpecification
   : LEFT_BRACE procedureSpecification RIGHT_BRACE
   ;

procedureSpecification
   : catalogModifyingProcedureSpecification
   | dataModifyingProcedureSpecification
   | querySpecification
   ;

catalogModifyingProcedureSpecification
   : procedureBody
   ;

nestedDataModifyingProcedureSpecification
   : LEFT_BRACE dataModifyingProcedureSpecification RIGHT_BRACE
   ;

dataModifyingProcedureSpecification
   : procedureBody
   ;

nestedQuerySpecification
   : LEFT_BRACE querySpecification RIGHT_BRACE
   ;

querySpecification
   : procedureBody
   ;

procedureBody
   : atSchemaClause? bindingVariableDefinitionBlock? statementBlock
   ;

bindingVariableDefinitionBlock
   : bindingVariableDefinition+
   ;

bindingVariableDefinition
   : graphVariableDefinition
   | bindingTableVariableDefinition
   | valueVariableDefinition
   ;

statementBlock
   : statement nextStatement*
   ;

statement
   : linearCatalogModifyingStatement
   | linearDataModifyingStatement
   | compositeQueryStatement
   ;

nextStatement
   : NEXT yieldClause? statement
   ;

graphVariableDefinition
   : PROPERTY? GRAPH bindingVariable optTypedGraphInitializer
   ;

optTypedGraphInitializer
   : (typed? graphReferenceValueType)? graphInitializer
   ;

graphInitializer
   : EQUALS_OPERATOR graphExpression
   ;

bindingTableVariableDefinition
   : BINDING? TABLE bindingVariable optTypedBindingTableInitializer
   ;

optTypedBindingTableInitializer
   : (typed? bindingTableReferenceValueType)? bindingTableInitializer
   ;

bindingTableInitializer
   : EQUALS_OPERATOR bindingTableExpression
   ;

valueVariableDefinition
   : VALUE bindingVariable optTypedValueInitializer
   ;

optTypedValueInitializer
   : (typed? valueType)? valueInitializer
   ;

valueInitializer
   : EQUALS_OPERATOR valueExpression
   ;

graphExpression
   : objectExpressionPrimary
   | graphReference
   | objectNameOrBindingVariable
   | currentGraph
   ;

currentGraph
   : CURRENT_PROPERTY_GRAPH
   | CURRENT_GRAPH
   ;

bindingTableExpression
   : nestedBindingTableQuerySpecification
   | objectExpressionPrimary
   | bindingTableReference
   | objectNameOrBindingVariable
   ;

nestedBindingTableQuerySpecification
   : nestedQuerySpecification
   ;

objectExpressionPrimary
   : VARIABLE valueExpressionPrimary
   | parenthesizedValueExpression
   | nonParenthesizedValueExpressionPrimarySpecialCase
   ;

linearCatalogModifyingStatement
   : simpleCatalogModifyingStatement+
   ;

simpleCatalogModifyingStatement
   : primitiveCatalogModifyingStatement
   | callCatalogModifyingProcedureStatement
   ;

primitiveCatalogModifyingStatement
   : createSchemaStatement
   | dropSchemaStatement
   | createGraphStatement
   | dropGraphStatement
   | createGraphTypeStatement
   | dropGraphTypeStatement
   ;

createSchemaStatement
   : CREATE SCHEMA (IF NOT EXISTS)? catalogSchemaParentAndName
   ;

dropSchemaStatement
   : DROP SCHEMA (IF EXISTS)? catalogSchemaParentAndName
   ;

createGraphStatement
   : CREATE (PROPERTY? GRAPH (IF NOT EXISTS)? | OR REPLACE PROPERTY? GRAPH) catalogGraphParentAndName (openGraphType | ofGraphType) graphSource?
   ;

openGraphType
   : typed? ANY (PROPERTY? GRAPH)?
   ;

ofGraphType
   : graphTypeLikeGraph
   | typed? graphTypeReference
   | typed? (PROPERTY? GRAPH)? nestedGraphTypeSpecification
   ;

graphTypeLikeGraph
   : LIKE graphExpression
   ;

graphSource
   : AS COPY OF graphExpression
   ;

dropGraphStatement
   : DROP PROPERTY? GRAPH (IF EXISTS)? catalogGraphParentAndName
   ;

createGraphTypeStatement
   : CREATE (PROPERTY? GRAPH TYPE (IF NOT EXISTS)? | OR REPLACE PROPERTY? GRAPH TYPE) catalogGraphTypeParentAndName graphTypeSource
   ;

graphTypeSource
   : AS? copyOfGraphType
   | graphTypeLikeGraph
   | AS? nestedGraphTypeSpecification
   ;

copyOfGraphType
   : COPY OF (graphTypeReference | externalObjectReference)
   ;

dropGraphTypeStatement
   : DROP PROPERTY? GRAPH TYPE (IF EXISTS)? catalogGraphTypeParentAndName
   ;

callCatalogModifyingProcedureStatement
   : callProcedureStatement
   ;

linearDataModifyingStatement
   : focusedLinearDataModifyingStatement
   | ambientLinearDataModifyingStatement
   ;

focusedLinearDataModifyingStatement
   : focusedLinearDataModifyingStatementBody
   | focusedNestedDataModifyingProcedureSpecification
   ;

focusedLinearDataModifyingStatementBody
   : useGraphClause simpleLinearDataAccessingStatement primitiveResultStatement?
   ;

focusedNestedDataModifyingProcedureSpecification
   : useGraphClause nestedDataModifyingProcedureSpecification
   ;

ambientLinearDataModifyingStatement
   : ambientLinearDataModifyingStatementBody
   | nestedDataModifyingProcedureSpecification
   ;

ambientLinearDataModifyingStatementBody
   : simpleLinearDataAccessingStatement primitiveResultStatement?
   ;

simpleLinearDataAccessingStatement
   : simpleDataAccessingStatement+
   ;

simpleDataAccessingStatement
   : simpleQueryStatement
   | simpleDataModifyingStatement
   ;

simpleDataModifyingStatement
   : primitiveDataModifyingStatement
   | callDataModifyingProcedureStatement
   ;

primitiveDataModifyingStatement
   : insertStatement
   | setStatement
   | removeStatement
   | deleteStatement
   ;

insertStatement
   : INSERT insertGraphPattern
   ;

setStatement
   : SET setItemList
   ;

setItemList
   : setItem (COMMA setItem)*
   ;

setItem
   : setPropertyItem
   | setAllPropertiesItem
   | setLabelItem
   ;

setPropertyItem
   : bindingVariableReference PERIOD propertyName EQUALS_OPERATOR valueExpression
   ;

setAllPropertiesItem
   : bindingVariableReference EQUALS_OPERATOR LEFT_BRACE propertyKeyValuePairList? RIGHT_BRACE
   ;

setLabelItem
   : bindingVariableReference isOrColon labelName
   ;

removeStatement
   : REMOVE removeItemList
   ;

removeItemList
   : removeItem (COMMA removeItem)*
   ;

removeItem
   : removePropertyItem
   | removeLabelItem
   ;

removePropertyItem
   : bindingVariableReference PERIOD propertyName
   ;

removeLabelItem
   : bindingVariableReference isOrColon labelName
   ;

deleteStatement
   : (DETACH | NODETACH)? DELETE deleteItemList
   ;

deleteItemList
   : deleteItem (COMMA deleteItem)*
   ;

deleteItem
   : valueExpression
   ;

callDataModifyingProcedureStatement
   : callProcedureStatement
   ;

compositeQueryStatement
   : compositeQueryExpression
   ;

compositeQueryExpression
   : compositeQueryExpression queryConjunction compositeQueryPrimary
   | compositeQueryPrimary
   ;

queryConjunction
   : setOperator
   | OTHERWISE
   ;

setOperator
   : UNION setQuantifier?
   | EXCEPT setQuantifier?
   | INTERSECT setQuantifier?
   ;

compositeQueryPrimary
   : linearQueryStatement
   ;

linearQueryStatement
   : focusedLinearQueryStatement
   | ambientLinearQueryStatement
   ;

focusedLinearQueryStatement
   : focusedLinearQueryStatementPart* focusedLinearQueryAndPrimitiveResultStatementPart
   | focusedPrimitiveResultStatement
   | focusedNestedQuerySpecification
   | selectStatement
   ;

focusedLinearQueryStatementPart
   : useGraphClause simpleLinearQueryStatement
   ;

focusedLinearQueryAndPrimitiveResultStatementPart
   : useGraphClause simpleLinearQueryStatement primitiveResultStatement
   ;

focusedPrimitiveResultStatement
   : useGraphClause primitiveResultStatement
   ;

focusedNestedQuerySpecification
   : useGraphClause nestedQuerySpecification
   ;

ambientLinearQueryStatement
   : simpleLinearQueryStatement? primitiveResultStatement
   | nestedQuerySpecification
   ;

simpleLinearQueryStatement
   : simpleQueryStatement+
   ;

simpleQueryStatement
   : primitiveQueryStatement
   | callQueryStatement
   ;

primitiveQueryStatement
   : matchStatement
   | letStatement
   | forStatement
   | filterStatement
   | orderByAndPageStatement
   ;

matchStatement
   : simpleMatchStatement
   | optionalMatchStatement
   ;

simpleMatchStatement
   : MATCH graphPatternBindingTable
   ;

optionalMatchStatement
   : OPTIONAL optionalOperand
   ;

optionalOperand
   : simpleMatchStatement
   | LEFT_BRACE matchStatementBlock RIGHT_BRACE
   | LEFT_PAREN matchStatementBlock RIGHT_PAREN
   ;

matchStatementBlock
   : matchStatement+
   ;

callQueryStatement
   : callProcedureStatement
   ;

filterStatement
   : FILTER (whereClause | searchCondition)
   ;

letStatement
   : LET letVariableDefinitionList
   ;

letVariableDefinitionList
   : letVariableDefinition (COMMA letVariableDefinition)*
   ;

letVariableDefinition
   : valueVariableDefinition
   | bindingVariable EQUALS_OPERATOR valueExpression
   ;

forStatement
   : FOR forItem forOrdinalityOrOffset?
   ;

forItem
   : forItemAlias forItemSource
   ;

forItemAlias
   : bindingVariable IN
   ;

forItemSource
   : listValueExpression
   | bindingTableReferenceValueExpression
   ;

forOrdinalityOrOffset
   : WITH (ORDINALITY | OFFSET) bindingVariable
   ;

orderByAndPageStatement
   : orderByClause offsetClause? limitClause?
   | offsetClause limitClause?
   | limitClause
   ;

primitiveResultStatement
   : returnStatement orderByAndPageStatement?
   | FINISH
   ;

returnStatement
   : RETURN returnStatementBody
   ;

returnStatementBody
   : setQuantifier? (ASTERISK | returnItemList) groupByClause?
   | NO BINDINGS
   ;

returnItemList
   : returnItem (COMMA returnItem)*
   ;

returnItem
   : aggregatingValueExpression returnItemAlias?
   ;

returnItemAlias
   : AS identifier
   ;

selectStatement
   : SELECT setQuantifier? (ASTERISK | selectItemList) (selectStatementBody whereClause? groupByClause? havingClause? orderByClause? offsetClause? limitClause?)?
   ;

selectItemList
   : selectItem (COMMA selectItem)*
   ;

selectItem
   : aggregatingValueExpression selectItemAlias?
   ;

selectItemAlias
   : AS identifier
   ;

havingClause
   : HAVING searchCondition
   ;

selectStatementBody
   : FROM (selectGraphMatchList | selectQuerySpecification)
   ;

selectGraphMatchList
   : selectGraphMatch (COMMA selectGraphMatch)*
   ;

selectGraphMatch
   : graphExpression matchStatement
   ;

selectQuerySpecification
   : nestedQuerySpecification
   | graphExpression nestedQuerySpecification
   ;

callProcedureStatement
   : OPTIONAL? CALL procedureCall
   ;

procedureCall
   : inlineProcedureCall
   | namedProcedureCall
   ;

inlineProcedureCall
   : variableScopeClause? nestedProcedureSpecification
   ;

variableScopeClause
   : LEFT_PAREN bindingVariableReferenceList? RIGHT_PAREN
   ;

bindingVariableReferenceList
   : bindingVariableReference (COMMA bindingVariableReference)*
   ;

namedProcedureCall
   : procedureReference LEFT_PAREN procedureArgumentList? RIGHT_PAREN yieldClause?
   ;

procedureArgumentList
   : procedureArgument (COMMA procedureArgument)*
   ;

procedureArgument
   : valueExpression
   ;

atSchemaClause
   : AT schemaReference
   ;

useGraphClause
   : USE graphExpression
   ;

graphPatternBindingTable
   : graphPattern graphPatternYieldClause?
   ;

graphPatternYieldClause
   : YIELD graphPatternYieldItemList
   ;

graphPatternYieldItemList
   : graphPatternYieldItem (COMMA graphPatternYieldItem)*
   | NO BINDINGS
   ;

graphPatternYieldItem
   : elementVariableReference
   | pathVariableReference
   ;

graphPattern
   : matchMode? pathPatternList keepClause? graphPatternWhereClause?
   ;

matchMode
   : repeatableElementsMatchMode
   | differentEdgesMatchMode
   ;

repeatableElementsMatchMode
   : REPEATABLE elementBindingsOrElements
   ;

differentEdgesMatchMode
   : DIFFERENT edgeBindingsOrEdges
   ;

elementBindingsOrElements
   : ELEMENT BINDINGS?
   | ELEMENTS
   ;

edgeBindingsOrEdges
   : EDGE_SYNONYM BINDINGS?
   | EDGES_SYNONYM
   ;

pathPatternList
   : pathPattern (COMMA pathPattern)*
   ;

pathPattern
   : pathVariableDeclaration? pathPatternPrefix? pathPatternExpression
   ;

pathVariableDeclaration
   : pathVariable EQUALS_OPERATOR
   ;

keepClause
   : KEEP pathPatternPrefix
   ;

graphPatternWhereClause
   : WHERE searchCondition
   ;

insertGraphPattern
   : insertPathPatternList
   ;

insertPathPatternList
   : insertPathPattern (COMMA insertPathPattern)*
   ;

insertPathPattern
   : insertNodePattern (insertEdgePattern insertNodePattern)*
   ;

insertNodePattern
   : LEFT_PAREN insertElementPatternFiller? RIGHT_PAREN
   ;

insertEdgePattern
   : insertEdgePointingLeft
   | insertEdgePointingRight
   | insertEdgeUndirected
   ;

insertEdgePointingLeft
   : LEFT_ARROW_BRACKET insertElementPatternFiller? RIGHT_BRACKET_MINUS
   ;

insertEdgePointingRight
   : MINUS_LEFT_BRACKET insertElementPatternFiller? BRACKET_RIGHT_ARROW
   ;

insertEdgeUndirected
   : TILDE_LEFT_BRACKET insertElementPatternFiller? RIGHT_BRACKET_TILDE
   ;

insertElementPatternFiller
   : elementVariableDeclaration labelAndPropertySetSpecification?
   | elementVariableDeclaration? labelAndPropertySetSpecification
   ;

labelAndPropertySetSpecification
   : isOrColon labelSetSpecification elementPropertySpecification?
   | (isOrColon labelSetSpecification)? elementPropertySpecification
   ;

pathPatternPrefix
   : pathModePrefix
   | pathSearchPrefix
   ;

pathModePrefix
   : pathMode pathOrPaths?
   ;

pathMode
   : WALK
   | TRAIL
   | SIMPLE
   | ACYCLIC
   ;

pathSearchPrefix
   : allPathSearch
   | anyPathSearch
   | shortestPathSearch
   ;

allPathSearch
   : ALL pathMode? pathOrPaths?
   ;

pathOrPaths
   : PATH
   | PATHS
   ;

anyPathSearch
   : ANY numberOfPaths? pathMode? pathOrPaths?
   ;

numberOfPaths
   : nonNegativeIntegerSpecification
   ;

shortestPathSearch
   : allShortestPathSearch
   | anyShortestPathSearch
   | countedShortestPathSearch
   | countedShortestGroupSearch
   ;

allShortestPathSearch
   : ALL SHORTEST pathMode? pathOrPaths?
   ;

anyShortestPathSearch
   : ANY SHORTEST pathMode? pathOrPaths?
   ;

countedShortestPathSearch
   : SHORTEST numberOfPaths pathMode? pathOrPaths?
   ;

countedShortestGroupSearch
   : SHORTEST numberOfGroups? pathMode? pathOrPaths? (GROUP | GROUPS)
   ;

numberOfGroups
   : nonNegativeIntegerSpecification
   ;

pathPatternExpression
   : pathTerm
   | pathMultisetAlternation
   | pathPatternUnion
   ;

pathMultisetAlternation
   : pathTerm MULTISET_ALTERNATION_OPERATOR pathTerm (MULTISET_ALTERNATION_OPERATOR pathTerm)*
   ;

pathPatternUnion
   : pathTerm VERTICAL_BAR pathTerm (VERTICAL_BAR pathTerm)*
   ;

pathTerm
   : pathFactor             #pathFactorLabel
   | pathTerm pathFactor    #pathConcatenationLabel
   ;

pathFactor
   : pathPrimary
   | quantifiedPathPrimary
   | questionedPathPrimary
   ;

quantifiedPathPrimary
   : pathPrimary graphPatternQuantifier
   ;

questionedPathPrimary
   : pathPrimary QUESTION_MARK
   ;

pathPrimary
   : elementPattern
   | parenthesizedPathPatternExpression
   | simplifiedPathPatternExpression
   ;

elementPattern
   : nodePattern
   | edgePattern
   ;

nodePattern
   : LEFT_PAREN elementPatternFiller RIGHT_PAREN
   ;

elementPatternFiller
   : elementVariableDeclaration? isLabelExpression? elementPatternPredicate?
   ;

elementVariableDeclaration
   : TEMP? elementVariable
   ;

isLabelExpression
   : isOrColon labelExpression
   ;

isOrColon
   : IS
   | COLON
   ;

elementPatternPredicate
   : elementPatternWhereClause
   | elementPropertySpecification
   ;

elementPatternWhereClause
   : WHERE searchCondition
   ;

elementPropertySpecification
   : LEFT_BRACE propertyKeyValuePairList RIGHT_BRACE
   ;

propertyKeyValuePairList
   : propertyKeyValuePair (COMMA propertyKeyValuePair)*
   ;

propertyKeyValuePair
   : propertyName COLON valueExpression
   ;

edgePattern
   : fullEdgePattern
   | abbreviatedEdgePattern
   ;

fullEdgePattern
   : fullEdgePointingLeft
   | fullEdgeUndirected
   | fullEdgePointingRight
   | fullEdgeLeftOrUndirected
   | fullEdgeUndirectedOrRight
   | fullEdgeLeftOrRight
   | fullEdgeAnyDirection
   ;

fullEdgePointingLeft
   : LEFT_ARROW_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
   ;

fullEdgeUndirected
   : TILDE_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
   ;

fullEdgePointingRight
   : MINUS_LEFT_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
   ;

fullEdgeLeftOrUndirected
   : LEFT_ARROW_TILDE_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
   ;

fullEdgeUndirectedOrRight
   : TILDE_LEFT_BRACKET elementPatternFiller BRACKET_TILDE_RIGHT_ARROW
   ;

fullEdgeLeftOrRight
   : LEFT_ARROW_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
   ;

fullEdgeAnyDirection
   : MINUS_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
   ;

abbreviatedEdgePattern
   : LEFT_ARROW
   | TILDE
   | RIGHT_ARROW
   | LEFT_ARROW_TILDE
   | TILDE_RIGHT_ARROW
   | LEFT_MINUS_RIGHT
   | MINUS_SIGN
   ;

parenthesizedPathPatternExpression
   : LEFT_PAREN subpathVariableDeclaration? pathModePrefix? pathPatternExpression parenthesizedPathPatternWhereClause? RIGHT_PAREN
   ;

subpathVariableDeclaration
   : subpathVariable EQUALS_OPERATOR
   ;

parenthesizedPathPatternWhereClause
   : WHERE searchCondition
   ;

labelExpression
   : labelTerm                                  #labelTermLabel
   | labelExpression VERTICAL_BAR labelTerm     #labelDisjunctionLabel
   ;

labelTerm
   : labelFactor                                #labelFactorLabel
   | labelTerm AMPERSAND labelFactor            #labelConjunctionLabel
   ;

labelFactor
   : labelPrimary
   | labelNegation
   ;

labelNegation
   : EXCLAMATION_MARK labelPrimary
   ;

labelPrimary
   : labelName
   | wildcardLabel
   | parenthesizedLabelExpression
   ;

wildcardLabel
   : PERCENT
   ;

parenthesizedLabelExpression
   : LEFT_PAREN labelExpression RIGHT_PAREN
   ;

pathVariableReference
   : bindingVariableReference
   ;

elementVariableReference
   : bindingVariableReference
   ;

graphPatternQuantifier
   : ASTERISK
   | PLUS_SIGN
   | fixedQuantifier
   | generalQuantifier
   ;

fixedQuantifier
   : LEFT_BRACE unsignedInteger RIGHT_BRACE
   ;

generalQuantifier
   : LEFT_BRACE lowerBound? COMMA upperBound? RIGHT_BRACE
   ;

lowerBound
   : unsignedInteger
   ;

upperBound
   : unsignedInteger
   ;

simplifiedPathPatternExpression
   : simplifiedDefaultingLeft
   | simplifiedDefaultingUndirected
   | simplifiedDefaultingRight
   | simplifiedDefaultingLeftOrUndirected
   | simplifiedDefaultingUndirectedOrRight
   | simplifiedDefaultingLeftOrRight
   | simplifiedDefaultingAnyDirection
   ;

simplifiedDefaultingLeft
   : LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS
   ;

simplifiedDefaultingUndirected
   : TILDE_SLASH simplifiedContents SLASH_TILDE
   ;

simplifiedDefaultingRight
   : MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT
   ;

simplifiedDefaultingLeftOrUndirected
   : LEFT_TILDE_SLASH simplifiedContents SLASH_TILDE
   ;

simplifiedDefaultingUndirectedOrRight
   : TILDE_SLASH simplifiedContents SLASH_TILDE_RIGHT
   ;

simplifiedDefaultingLeftOrRight
   : LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT
   ;

simplifiedDefaultingAnyDirection
   : MINUS_SLASH simplifiedContents SLASH_MINUS
   ;

simplifiedContents
   : simplifiedTerm
   | simplifiedPathUnion
   | simplifiedMultisetAlternation
   ;

simplifiedPathUnion
   : simplifiedTerm VERTICAL_BAR simplifiedTerm (VERTICAL_BAR simplifiedTerm)*
   ;

simplifiedMultisetAlternation
   : simplifiedTerm MULTISET_ALTERNATION_OPERATOR simplifiedTerm (MULTISET_ALTERNATION_OPERATOR simplifiedTerm)*
   ;

simplifiedTerm
   : simplifiedFactorLow                    #simplifiedFactorLowLabel
   | simplifiedTerm simplifiedFactorLow     #simplifiedConcatenationLabel
   ;

simplifiedFactorLow
   : simplifiedFactorHigh                               #simplifiedFactorHighLabel
   | simplifiedFactorLow AMPERSAND simplifiedFactorHigh #simplifiedConjunctionLabel
   ;

simplifiedFactorHigh
   : simplifiedTertiary
   | simplifiedQuantified
   | simplifiedQuestioned
   ;

simplifiedQuantified
   : simplifiedTertiary graphPatternQuantifier
   ;

simplifiedQuestioned
   : simplifiedTertiary QUESTION_MARK
   ;

simplifiedTertiary
   : simplifiedDirectionOverride
   | simplifiedSecondary
   ;

simplifiedDirectionOverride
   : simplifiedOverrideLeft
   | simplifiedOverrideUndirected
   | simplifiedOverrideRight
   | simplifiedOverrideLeftOrUndirected
   | simplifiedOverrideUndirectedOrRight
   | simplifiedOverrideLeftOrRight
   | simplifiedOverrideAnyDirection
   ;

simplifiedOverrideLeft
   : LEFT_ANGLE_BRACKET simplifiedSecondary
   ;

simplifiedOverrideUndirected
   : TILDE simplifiedSecondary
   ;

simplifiedOverrideRight
   : simplifiedSecondary RIGHT_ANGLE_BRACKET
   ;

simplifiedOverrideLeftOrUndirected
   : LEFT_ARROW_TILDE simplifiedSecondary
   ;

simplifiedOverrideUndirectedOrRight
   : TILDE simplifiedSecondary RIGHT_ANGLE_BRACKET
   ;

simplifiedOverrideLeftOrRight
   : LEFT_ANGLE_BRACKET simplifiedSecondary RIGHT_ANGLE_BRACKET
   ;

simplifiedOverrideAnyDirection
   : MINUS_SIGN simplifiedSecondary
   ;

simplifiedSecondary
   : simplifiedPrimary
   | simplifiedNegation
   ;

simplifiedNegation
   : EXCLAMATION_MARK simplifiedPrimary
   ;

simplifiedPrimary
   : labelName
   | LEFT_PAREN simplifiedContents RIGHT_PAREN
   ;

whereClause
   : WHERE searchCondition
   ;

yieldClause
   : YIELD yieldItemList
   ;

yieldItemList
   : yieldItem (COMMA yieldItem)*
   ;

yieldItem
   : (yieldItemName yieldItemAlias?)
   ;

yieldItemName
   : fieldName
   ;

yieldItemAlias
   : AS bindingVariable
   ;

groupByClause
   : GROUP BY groupingElementList
   ;

groupingElementList
   : groupingElement (COMMA groupingElement)*
   | emptyGroupingSet
   ;

groupingElement
   : bindingVariableReference
   ;

emptyGroupingSet
   : LEFT_PAREN RIGHT_PAREN
   ;

orderByClause
   : ORDER BY sortSpecificationList
   ;

sortSpecificationList
   : sortSpecification (COMMA sortSpecification)*
   ;

sortSpecification
   : sortKey orderingSpecification? nullOrdering?
   ;

sortKey
   : aggregatingValueExpression
   ;

orderingSpecification
   : ASC
   | ASCENDING
   | DESC
   | DESCENDING
   ;

nullOrdering
   : NULLS FIRST
   | NULLS LAST
   ;

limitClause
   : LIMIT nonNegativeIntegerSpecification
   ;

offsetClause
   : offsetSynonym nonNegativeIntegerSpecification
   ;

offsetSynonym
   : OFFSET
   | SKIP_RESERVED_WORD
   ;

schemaReference
   : absoluteCatalogSchemaReference
   | relativeCatalogSchemaReference
   | referenceParameterSpecification
   ;

absoluteCatalogSchemaReference
   : SOLIDUS
   | absoluteDirectoryPath schemaName
   ;

catalogSchemaParentAndName
   : absoluteDirectoryPath schemaName
   ;

relativeCatalogSchemaReference
   : predefinedSchemaReference
   | relativeDirectoryPath schemaName
   ;

predefinedSchemaReference
   : HOME_SCHEMA
   | CURRENT_SCHEMA
   | PERIOD
   ;

absoluteDirectoryPath
   : SOLIDUS simpleDirectoryPath?
   ;

relativeDirectoryPath
   : DOUBLE_PERIOD ( (SOLIDUS DOUBLE_PERIOD)+ SOLIDUS simpleDirectoryPath?)?
   ;

simpleDirectoryPath
   : (directoryName SOLIDUS)+
   ;

graphReference
   : catalogObjectParentReference graphName
   | delimitedGraphName
   | homeGraph
   | referenceParameterSpecification
   ;

catalogGraphParentAndName
   : catalogObjectParentReference? graphName
   ;

homeGraph
   : HOME_PROPERTY_GRAPH
   | HOME_GRAPH
   ;

graphTypeReference
   : catalogGraphTypeParentAndName
   | referenceParameterSpecification
   ;

catalogGraphTypeParentAndName
   : catalogObjectParentReference? graphTypeName
   ;

bindingTableReference
   : catalogObjectParentReference bindingTableName
   | delimitedBindingTableName
   | referenceParameterSpecification
   ;

procedureReference
   : catalogProcedureParentAndName
   | referenceParameterSpecification
   ;

catalogProcedureParentAndName
   : catalogObjectParentReference? procedureName
   ;

catalogObjectParentReference
   : schemaReference SOLIDUS? (objectName PERIOD)*
   |  (objectName PERIOD)+
   ;

referenceParameterSpecification
   : SUBSTITUTED_PARAMETER_REFERENCE
   ;

externalObjectReference
   :
   ;

nestedGraphTypeSpecification
   : LEFT_BRACE graphTypeSpecificationBody RIGHT_BRACE
   ;

graphTypeSpecificationBody
   : elementTypeList
   ;

elementTypeList
   : elementTypeSpecification (COMMA elementTypeSpecification)*
   ;

elementTypeSpecification
   : nodeTypeSpecification
   | edgeTypeSpecification
   ;

nodeTypeSpecification
   : nodeTypePattern
   | nodeTypePhrase
   ;

nodeTypePattern
   : (NODE_SYNONYM TYPE? nodeTypeName)? LEFT_PAREN localNodeTypeAlias? nodeTypeFiller? RIGHT_PAREN
   ;

nodeTypePhrase
   : NODE_SYNONYM TYPE? nodeTypePhraseFiller (AS localNodeTypeAlias)?
   ;

nodeTypePhraseFiller
   : nodeTypeName nodeTypeFiller?
   | nodeTypeFiller
   ;

nodeTypeFiller
   : nodeTypeKeyLabelSet nodeTypeImpliedContent?
   | nodeTypeImpliedContent
   ;

localNodeTypeAlias
   : REGULAR_IDENTIFIER
   ;

nodeTypeImpliedContent
   : nodeTypeLabelSet
   | nodeTypePropertyTypes
   | nodeTypeLabelSet nodeTypePropertyTypes
   ;

nodeTypeKeyLabelSet
   : labelSetPhrase? IMPLIES
   ;

nodeTypeLabelSet
   : labelSetPhrase
   ;

nodeTypePropertyTypes
   : propertyTypesSpecification
   ;

edgeTypeSpecification
   : edgeTypePattern
   | edgeTypePhrase
   ;

edgeTypePattern
   : (edgeKind? EDGE_SYNONYM TYPE? edgeTypeName)? (edgeTypePatternDirected | edgeTypePatternUndirected)
   ;

edgeTypePhrase
   : edgeKind EDGE_SYNONYM TYPE? edgeTypePhraseFiller endpointPairPhrase
   ;

edgeTypePhraseFiller
   : edgeTypeName edgeTypeFiller?
   | edgeTypeFiller
   ;

edgeTypeFiller
   : edgeTypeKeyLabelSet edgeTypeImpliedContent?
   | edgeTypeImpliedContent
   ;

edgeTypeImpliedContent
   : edgeTypeLabelSet
   | edgeTypePropertyTypes
   | edgeTypeLabelSet edgeTypePropertyTypes
   ;

edgeTypeKeyLabelSet
   : labelSetPhrase? IMPLIES
   ;

edgeTypeLabelSet
   : labelSetPhrase
   ;

edgeTypePropertyTypes
   : propertyTypesSpecification
   ;

edgeTypePatternDirected
   : edgeTypePatternPointingRight
   | edgeTypePatternPointingLeft
   ;

edgeTypePatternPointingRight
   : sourceNodeTypeReference arcTypePointingRight destinationNodeTypeReference
   ;

edgeTypePatternPointingLeft
   : destinationNodeTypeReference arcTypePointingLeft sourceNodeTypeReference
   ;

edgeTypePatternUndirected
   : sourceNodeTypeReference arcTypeUndirected destinationNodeTypeReference
   ;

arcTypePointingRight
   : MINUS_LEFT_BRACKET edgeTypeFiller BRACKET_RIGHT_ARROW
   ;

arcTypePointingLeft
   : LEFT_ARROW_BRACKET edgeTypeFiller RIGHT_BRACKET_MINUS
   ;

arcTypeUndirected
   : TILDE_LEFT_BRACKET edgeTypeFiller RIGHT_BRACKET_TILDE
   ;

sourceNodeTypeReference
   : LEFT_PAREN sourceNodeTypeAlias RIGHT_PAREN
   | LEFT_PAREN nodeTypeFiller? RIGHT_PAREN
   ;

destinationNodeTypeReference
   : LEFT_PAREN destinationNodeTypeAlias RIGHT_PAREN
   | LEFT_PAREN nodeTypeFiller? RIGHT_PAREN
   ;

edgeKind
   : DIRECTED
   | UNDIRECTED
   ;

endpointPairPhrase
   : CONNECTING endpointPair
   ;

endpointPair
   : endpointPairDirected
   | endpointPairUndirected
   ;

endpointPairDirected
   : endpointPairPointingRight
   | endpointPairPointingLeft
   ;

endpointPairPointingRight
   : LEFT_PAREN sourceNodeTypeAlias connectorPointingRight destinationNodeTypeAlias RIGHT_PAREN
   ;

endpointPairPointingLeft
   : LEFT_PAREN destinationNodeTypeAlias LEFT_ARROW sourceNodeTypeAlias RIGHT_PAREN
   ;

endpointPairUndirected
   : LEFT_PAREN sourceNodeTypeAlias connectorUndirected destinationNodeTypeAlias RIGHT_PAREN
   ;

connectorPointingRight
   : TO
   | RIGHT_ARROW
   ;

connectorUndirected
   : TO
   | TILDE
   ;

sourceNodeTypeAlias
   : REGULAR_IDENTIFIER
   ;

destinationNodeTypeAlias
   : REGULAR_IDENTIFIER
   ;

labelSetPhrase
   : LABEL labelName
   | LABELS labelSetSpecification
   | isOrColon labelSetSpecification
   ;

labelSetSpecification
   : labelName (AMPERSAND labelName)*
   ;

propertyTypesSpecification
   : LEFT_BRACE propertyTypeList? RIGHT_BRACE
   ;

propertyTypeList
   : propertyType (COMMA propertyType)*
   ;

propertyType
   : propertyName typed? propertyValueType
   ;

propertyValueType
   : valueType
   ;

bindingTableType
   : BINDING? TABLE fieldTypesSpecification
   ;

valueType
   : predefinedType                                                                                                         #predefinedTypeLabel
   // <constructed value type>
   | pathValueType                                                                                                          #pathValueTypeLabel
   | listValueTypeName LEFT_ANGLE_BRACKET valueType RIGHT_ANGLE_BRACKET (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?    #listValueTypeAlt1
   | valueType listValueTypeName (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?                                           #listValueTypeAlt2
   | listValueTypeName (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?                                                     #listValueTypeAlt3
   | recordType                                                                                                             #recordTypeLabel
   // <dynamic union type>
   | ANY VALUE? notNull?                                                                                                    #openDynamicUnionTypeLabel
   | ANY? PROPERTY VALUE notNull?                                                                                           #dynamicPropertyValueTypeLabel
   // <closed dynamic union type>
   | ANY VALUE? LEFT_ANGLE_BRACKET valueType (VERTICAL_BAR valueType)* RIGHT_ANGLE_BRACKET                                  #closedDynamicUnionTypeAtl1
   | valueType VERTICAL_BAR valueType                                                                                       #closedDynamicUnionTypeAtl2
   ;

typed
   : DOUBLE_COLON
   | TYPED
   ;

predefinedType
   : booleanType
   | characterStringType
   | byteStringType
   | numericType
   | temporalType
   | referenceValueType
   | immaterialValueType
   ;

booleanType
   : (BOOL | BOOLEAN) notNull?
   ;

characterStringType
   : STRING (LEFT_PAREN (minLength COMMA)? maxLength RIGHT_PAREN)? notNull?
   | CHAR (LEFT_PAREN fixedLength RIGHT_PAREN)? notNull?
   | VARCHAR (LEFT_PAREN maxLength RIGHT_PAREN)? notNull?
   ;

byteStringType
   : BYTES (LEFT_PAREN (minLength COMMA)? maxLength RIGHT_PAREN)? notNull?
   | BINARY (LEFT_PAREN fixedLength RIGHT_PAREN)? notNull?
   | VARBINARY (LEFT_PAREN maxLength RIGHT_PAREN)? notNull?
   ;

minLength
   : unsignedInteger
   ;

maxLength
   : unsignedInteger
   ;

fixedLength
   : unsignedInteger
   ;

numericType
   : exactNumericType
   | approximateNumericType
   ;

exactNumericType
   : binaryExactNumericType
   | decimalExactNumericType
   ;

binaryExactNumericType
   : signedBinaryExactNumericType
   | unsignedBinaryExactNumericType
   ;

signedBinaryExactNumericType
   : INT8 notNull?
   | INT16 notNull?
   | INT32 notNull?
   | INT64 notNull?
   | INT128 notNull?
   | INT256 notNull?
   | SMALLINT notNull?
   | INT (LEFT_PAREN precision RIGHT_PAREN)? notNull?
   | BIGINT notNull?
   | SIGNED? verboseBinaryExactNumericType
   ;

unsignedBinaryExactNumericType
   : UINT8 notNull?
   | UINT16 notNull?
   | UINT32 notNull?
   | UINT64 notNull?
   | UINT128 notNull?
   | UINT256 notNull?
   | USMALLINT notNull?
   | UINT (LEFT_PAREN precision RIGHT_PAREN)? notNull?
   | UBIGINT notNull?
   | UNSIGNED verboseBinaryExactNumericType
   ;

verboseBinaryExactNumericType
   : INTEGER8 notNull?
   | INTEGER16 notNull?
   | INTEGER32 notNull?
   | INTEGER64 notNull?
   | INTEGER128 notNull?
   | INTEGER256 notNull?
   | SMALL INTEGER notNull?
   | INTEGER (LEFT_PAREN precision RIGHT_PAREN)? notNull?
   | BIG INTEGER notNull?
   ;

decimalExactNumericType
   : (DECIMAL | DEC) (LEFT_PAREN precision (COMMA scale)? RIGHT_PAREN notNull?)?
   ;

precision
   : unsignedDecimalInteger
   ;

scale
   : unsignedDecimalInteger
   ;

approximateNumericType
   : FLOAT16 notNull?
   | FLOAT32 notNull?
   | FLOAT64 notNull?
   | FLOAT128 notNull?
   | FLOAT256 notNull?
   | FLOAT (LEFT_PAREN precision (COMMA scale)? RIGHT_PAREN)? notNull?
   | REAL notNull?
   | DOUBLE PRECISION? notNull?
   ;

temporalType
   : temporalInstantType
   | temporalDurationType
   ;

temporalInstantType
   : datetimeType
   | localdatetimeType
   | dateType
   | timeType
   | localtimeType
   ;

datetimeType
   : ZONED DATETIME notNull?
   | TIMESTAMP WITH TIME ZONE notNull?
   ;

localdatetimeType
   : LOCAL DATETIME notNull?
   | TIMESTAMP (WITHOUT TIME ZONE)? notNull?
   ;

dateType
   : DATE notNull?
   ;

timeType
   : ZONED TIME notNull?
   | TIME WITH TIME ZONE notNull?
   ;

localtimeType
   : LOCAL TIME notNull?
   | TIME WITHOUT TIME ZONE notNull?
   ;

temporalDurationType
   : DURATION LEFT_PAREN temporalDurationQualifier RIGHT_PAREN notNull?
   ;

temporalDurationQualifier
   : YEAR TO MONTH
   | DAY TO SECOND
   ;

referenceValueType
   : graphReferenceValueType
   | bindingTableReferenceValueType
   | nodeReferenceValueType
   | edgeReferenceValueType
   ;

immaterialValueType
   : nullType
   | emptyType
   ;

nullType
   : NULL
   ;

emptyType
   : NULL notNull
   | NOTHING
   ;

graphReferenceValueType
   : openGraphReferenceValueType
   | closedGraphReferenceValueType
   ;

closedGraphReferenceValueType
   : PROPERTY? GRAPH nestedGraphTypeSpecification notNull?
   ;

openGraphReferenceValueType
   : ANY PROPERTY? GRAPH notNull?
   ;

bindingTableReferenceValueType
   : bindingTableType notNull?
   ;

nodeReferenceValueType
   : openNodeReferenceValueType
   | closedNodeReferenceValueType
   ;

closedNodeReferenceValueType
   : nodeTypeSpecification notNull?
   ;

openNodeReferenceValueType
   : ANY? NODE_SYNONYM notNull?
   ;

edgeReferenceValueType
   : openEdgeReferenceValueType
   | closedEdgeReferenceValueType
   ;

closedEdgeReferenceValueType
   : edgeTypeSpecification notNull?
   ;

openEdgeReferenceValueType
   : ANY? EDGE_SYNONYM notNull?
   ;

pathValueType
   : PATH notNull?
   ;

listValueTypeName
   : GROUP? listValueTypeNameSynonym
   ;

listValueTypeNameSynonym
   : LIST
   | ARRAY
   ;

recordType
   : ANY? RECORD notNull?
   | RECORD? fieldTypesSpecification notNull?
   ;

fieldTypesSpecification
   : LEFT_BRACE fieldTypeList? RIGHT_BRACE
   ;

fieldTypeList
   : fieldType (COMMA fieldType)*
   ;

notNull
   :  NOT NULL
   ;

fieldType
   : fieldName typed? valueType
   ;

searchCondition
   : booleanValueExpression
   ;

predicate
   : comparisonPredicate
   | existsPredicate
   | nullPredicate
   | normalizedPredicate
   | valueTypePredicate
   | directedPredicate
   | labeledPredicate
   | sourceDestinationPredicate
   | all_differentPredicate
   | samePredicate
   | property_existsPredicate
   ;

comparisonPredicate
   : comparisonPredicand comparisonPredicatePart2
   ;

comparisonPredicatePart2
   : compOp comparisonPredicand
   ;


compOp
    : EQUALS_OPERATOR
    | NOT_EQUALS_OPERATOR
    | LEFT_ANGLE_BRACKET
    | RIGHT_ANGLE_BRACKET
    | LESS_THAN_OR_EQUALS_OPERATOR
    | GREATER_THAN_OR_EQUALS_OPERATOR
    ;

comparisonPredicand
   : commonValueExpression
   | booleanPredicand
   ;

existsPredicate
   : EXISTS (LEFT_BRACE graphPattern RIGHT_BRACE | LEFT_PAREN graphPattern RIGHT_PAREN | LEFT_BRACE matchStatementBlock RIGHT_BRACE | LEFT_PAREN matchStatementBlock RIGHT_PAREN | nestedQuerySpecification)
   ;

nullPredicate
   : valueExpressionPrimary nullPredicatePart2
   ;

nullPredicatePart2
   : IS NOT? NULL
   ;

valueTypePredicate
   : valueExpressionPrimary valueTypePredicatePart2
   ;

valueTypePredicatePart2
   : IS NOT? typed valueType
   ;

normalizedPredicate
   : stringValueExpression normalizedPredicatePart2
   ;

normalizedPredicatePart2
   : IS NOT? normalForm? NORMALIZED
   ;

directedPredicate
   : elementVariableReference directedPredicatePart2
   ;

directedPredicatePart2
   : IS NOT? DIRECTED
   ;

labeledPredicate
   : elementVariableReference labeledPredicatePart2
   ;

labeledPredicatePart2
   : isLabeledOrColon labelExpression
   ;

isLabeledOrColon
   : IS NOT? LABELED
   | COLON
   ;

sourceDestinationPredicate
   : nodeReference sourcePredicatePart2
   | nodeReference destinationPredicatePart2
   ;

nodeReference
   : elementVariableReference
   ;

sourcePredicatePart2
   : IS NOT? SOURCE OF edgeReference
   ;

destinationPredicatePart2
   : IS NOT? DESTINATION OF edgeReference
   ;

edgeReference
   : elementVariableReference
   ;

all_differentPredicate
   : ALL_DIFFERENT LEFT_PAREN elementVariableReference COMMA elementVariableReference (COMMA elementVariableReference)* RIGHT_PAREN
   ;

samePredicate
   : SAME LEFT_PAREN elementVariableReference COMMA elementVariableReference (COMMA elementVariableReference)* RIGHT_PAREN
   ;

property_existsPredicate
   : PROPERTY_EXISTS LEFT_PAREN elementVariableReference COMMA propertyName RIGHT_PAREN
   ;

valueExpression
   : commonValueExpression
   | booleanValueExpression
   ;

commonValueExpression
   : numericValueExpression
   | stringValueExpression
   | datetimeValueExpression
   | durationValueExpression
   | listValueExpression
   | recordExpression
   | pathValueExpression
   | referenceValueExpression
   ;

referenceValueExpression
   : graphReferenceValueExpression
   | bindingTableReferenceValueExpression
   | nodeReferenceValueExpression
   | edgeReferenceValueExpression
   ;

graphReferenceValueExpression
   : PROPERTY? GRAPH graphExpression
   | valueExpressionPrimary
   ;

bindingTableReferenceValueExpression
   : BINDING? TABLE bindingTableExpression
   | valueExpressionPrimary
   ;

nodeReferenceValueExpression
   : valueExpressionPrimary
   ;

edgeReferenceValueExpression
   : valueExpressionPrimary
   ;

recordExpression
   : valueExpressionPrimary
   ;

aggregatingValueExpression
   : valueExpression
   ;

valueExpressionPrimary
   : parenthesizedValueExpression
   | aggregateFunction
   | unsignedValueSpecification
   | listValueConstructor
   | recordConstructor
   | pathValueConstructor
   | valueExpressionPrimary PERIOD propertyName     // <propertyReference
   | valueQueryExpression
   | caseExpression
   | castSpecification
   | element_idFunction
   | letValueExpression
   | bindingVariableReference
   ;

parenthesizedValueExpression
   : LEFT_PAREN valueExpression RIGHT_PAREN
   ;

nonParenthesizedValueExpressionPrimary
   : nonParenthesizedValueExpressionPrimarySpecialCase
   | bindingVariableReference
   ;

nonParenthesizedValueExpressionPrimarySpecialCase
   : aggregateFunction
   | unsignedValueSpecification
   | listValueConstructor
   | recordConstructor
   | pathValueConstructor
   | valueExpressionPrimary PERIOD propertyName     // <property reference>
   | valueQueryExpression
   | caseExpression
   | castSpecification
   | element_idFunction
   | letValueExpression
   ;

unsignedValueSpecification
   : unsignedLiteral
   | generalValueSpecification
   ;

nonNegativeIntegerSpecification
   : unsignedInteger
   | dynamicParameterSpecification
   ;

generalValueSpecification
   : dynamicParameterSpecification
   | SESSION_USER
   ;

dynamicParameterSpecification
   : GENERAL_PARAMETER_REFERENCE
   ;

letValueExpression
   : LET letVariableDefinitionList IN valueExpression END
   ;

valueQueryExpression
   : VALUE nestedQuerySpecification
   ;

caseExpression
   : caseAbbreviation
   | caseSpecification
   ;

caseAbbreviation
   : NULLIF LEFT_PAREN valueExpression COMMA valueExpression RIGHT_PAREN
   | COALESCE LEFT_PAREN valueExpression (COMMA valueExpression)+ RIGHT_PAREN
   ;

caseSpecification
   : simpleCase
   | searchedCase
   ;

simpleCase
   : CASE caseOperand simpleWhenClause+ elseClause? END
   ;

searchedCase
   : CASE searchedWhenClause+ elseClause? END
   ;

simpleWhenClause
   : WHEN whenOperandList THEN result
   ;

searchedWhenClause
   : WHEN searchCondition THEN result
   ;

elseClause
   : ELSE result
   ;

caseOperand
   : nonParenthesizedValueExpressionPrimary
   | elementVariableReference
   ;

whenOperandList
   : whenOperand (COMMA whenOperand)*
   ;

whenOperand
   : nonParenthesizedValueExpressionPrimary
   | comparisonPredicatePart2
   | nullPredicatePart2
   | valueTypePredicatePart2
   | normalizedPredicatePart2
   | directedPredicatePart2
   | labeledPredicatePart2
   | sourcePredicatePart2
   | destinationPredicatePart2
   ;

result
   : resultExpression
   | nullLiteral
   ;

resultExpression
   : valueExpression
   ;

castSpecification
   : CAST LEFT_PAREN castOperand AS castTarget RIGHT_PAREN
   ;

castOperand
   : valueExpression
   | nullLiteral
   ;

castTarget
   : valueType
   ;

aggregateFunction
   : COUNT LEFT_PAREN ASTERISK RIGHT_PAREN
   | generalSetFunction
   | binarySetFunction
   ;

generalSetFunction
   : generalSetFunctionType LEFT_PAREN setQuantifier? valueExpression RIGHT_PAREN
   ;

binarySetFunction
   : binarySetFunctionType LEFT_PAREN dependentValueExpression COMMA independentValueExpression RIGHT_PAREN
   ;

generalSetFunctionType
   : AVG
   | COUNT
   | MAX
   | MIN
   | SUM
   | COLLECT_LIST
   | STDDEV_SAMP
   | STDDEV_POP
   ;

setQuantifier
   : DISTINCT
   | ALL
   ;

binarySetFunctionType
   : PERCENTILE_CONT
   | PERCENTILE_DISC
   ;

dependentValueExpression
   : setQuantifier? numericValueExpression
   ;

independentValueExpression
   : numericValueExpression
   ;

element_idFunction
   : ELEMENT_ID LEFT_PAREN elementVariableReference RIGHT_PAREN
   ;

bindingVariableReference
   : bindingVariable
   ;

pathValueExpression
   : pathValueExpression CONCATENATION_OPERATOR pathValuePrimary    #pathValueConcatenationLabel
   | pathValuePrimary                                               #pathValuePrimaryLabel
   ;

pathValuePrimary
   : valueExpressionPrimary
   ;

pathValueConstructor
   : pathValueConstructorByEnumeration
   ;

pathValueConstructorByEnumeration
   : PATH LEFT_BRACKET pathElementList RIGHT_BRACKET
   ;

pathElementList
   : pathElementListStart pathElementListStep*
   ;

pathElementListStart
   : nodeReferenceValueExpression
   ;

pathElementListStep
   : COMMA edgeReferenceValueExpression COMMA nodeReferenceValueExpression
   ;

listValueExpression
   : listValueExpression CONCATENATION_OPERATOR listPrimary     #listConcatenationLabel
   | listPrimary                                                #listPrimaryLabel
   ;

listPrimary
   : listValueFunction
   | valueExpressionPrimary
   ;

listValueFunction
   : trimListFunction
   | elementsFunction
   ;

trimListFunction
   : TRIM LEFT_PAREN listValueExpression COMMA numericValueExpression RIGHT_PAREN
   ;

elementsFunction
   : ELEMENTS LEFT_PAREN pathValueExpression RIGHT_PAREN
   ;

listValueConstructor
   : listValueConstructorByEnumeration
   ;

listValueConstructorByEnumeration
   : listValueTypeName? LEFT_BRACKET listElementList? RIGHT_BRACKET
   ;

listElementList
   : listElement (COMMA listElement)*
   ;

listElement
   : valueExpression
   ;

recordConstructor
   : RECORD? fieldsSpecification
   ;

fieldsSpecification
   : LEFT_BRACE fieldList? RIGHT_BRACE
   ;

fieldList
   : field (COMMA field)*
   ;

field
   : fieldName COLON valueExpression
   ;

booleanValueExpression
   : booleanTerm
   | booleanValueExpression OR booleanTerm
   | booleanValueExpression XOR booleanTerm
   ;

booleanTerm
   : booleanFactor
   | booleanTerm AND booleanFactor
   ;

booleanFactor
   : NOT? booleanTest
   ;

booleanTest
   : booleanPrimary (IS NOT? truthValue)?
   ;

truthValue
   : BOOLEAN_LITERAL
   ;

booleanPrimary
   : predicate
   | booleanPredicand
   ;

booleanPredicand
   : parenthesizedBooleanValueExpression
   | nonParenthesizedValueExpressionPrimary
   ;

parenthesizedBooleanValueExpression
   : LEFT_PAREN booleanValueExpression RIGHT_PAREN
   ;

numericValueExpression
   : term
   | numericValueExpression PLUS_SIGN term
   | numericValueExpression MINUS_SIGN term
   ;

term
   : factor
   | term ASTERISK factor
   | term SOLIDUS factor
   ;

factor
   : (PLUS_SIGN | MINUS_SIGN)? numericPrimary
   ;

numericPrimary
   : valueExpressionPrimary
   | numericValueFunction
   ;

numericValueFunction
   : lengthExpression
   | cardinalityExpression
   | absoluteValueExpression
   | modulusExpression
   | trigonometricFunction
   | generalLogarithmFunction
   | commonLogarithm
   | naturalLogarithm
   | exponentialFunction
   | powerFunction
   | squareRoot
   | floorFunction
   | ceilingFunction
   ;

lengthExpression
   : charLengthExpression
   | byteLengthExpression
   | pathLengthExpression
   ;

cardinalityExpression
   : CARDINALITY LEFT_PAREN cardinalityExpressionArgument RIGHT_PAREN
   | SIZE LEFT_PAREN listValueExpression RIGHT_PAREN
   ;

cardinalityExpressionArgument
   : bindingTableReferenceValueExpression
   | pathValueExpression
   | listValueExpression
   | recordExpression
   ;

charLengthExpression
   : (CHAR_LENGTH | CHARACTER_LENGTH) LEFT_PAREN characterStringValueExpression RIGHT_PAREN
   ;

byteLengthExpression
   : (BYTE_LENGTH | OCTET_LENGTH) LEFT_PAREN byteStringValueExpression RIGHT_PAREN
   ;

pathLengthExpression
   : PATH_LENGTH LEFT_PAREN pathValueExpression RIGHT_PAREN
   ;

absoluteValueExpression
   : ABS LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

modulusExpression
   : MOD LEFT_PAREN numericValueExpressionDividend COMMA numericValueExpressionDivisor RIGHT_PAREN
   ;

numericValueExpressionDividend
   : numericValueExpression
   ;

numericValueExpressionDivisor
   : numericValueExpression
   ;

trigonometricFunction
   : trigonometricFunctionName LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

trigonometricFunctionName
   : SIN
   | COS
   | TAN
   | COT
   | SINH
   | COSH
   | TANH
   | ASIN
   | ACOS
   | ATAN
   | DEGREES
   | RADIANS
   ;

generalLogarithmFunction
   : LOG LEFT_PAREN generalLogarithmBase COMMA generalLogarithmArgument RIGHT_PAREN
   ;

generalLogarithmBase
   : numericValueExpression
   ;

generalLogarithmArgument
   : numericValueExpression
   ;

commonLogarithm
   : LOG10 LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

naturalLogarithm
   : LN LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

exponentialFunction
   : EXP LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

powerFunction
   : POWER LEFT_PAREN numericValueExpressionBase COMMA numericValueExpressionExponent RIGHT_PAREN
   ;

numericValueExpressionBase
   : numericValueExpression
   ;

numericValueExpressionExponent
   : numericValueExpression
   ;

squareRoot
   : SQRT LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

floorFunction
   : FLOOR LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

ceilingFunction
   : (CEIL | CEILING) LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

stringValueExpression
   : characterStringValueExpression
   | byteStringValueExpression
   ;

characterStringValueExpression
   : characterStringValueExpression CONCATENATION_OPERATOR characterStringPrimary   #characterStringConcatenationLabel
   | characterStringPrimary                                                         #characterStringPrimaryLabel
   ;

characterStringPrimary
   : valueExpressionPrimary
   | characterStringFunction
   ;

byteStringValueExpression
   : byteStringValueExpression CONCATENATION_OPERATOR byteStringPrimary #byteStringConcatenationLabel
   | byteStringPrimary                                                  #byteStringPrimaryLabel
   ;

byteStringPrimary
   : valueExpressionPrimary
   | byteStringFunction
   ;

characterStringFunction
   : substringFunction
   | fold
   | trimFunction
   | normalizeFunction
   ;

substringFunction
   : (LEFT | RIGHT) LEFT_PAREN characterStringValueExpression COMMA stringLength RIGHT_PAREN
   ;

fold
   : (UPPER | LOWER) LEFT_PAREN characterStringValueExpression RIGHT_PAREN
   ;

trimFunction
   : singleCharacterTrimFunction
   | multiCharacterTrimFunction
   ;

singleCharacterTrimFunction
   : TRIM LEFT_PAREN trimOperands RIGHT_PAREN
   ;

multiCharacterTrimFunction
   : (BTRIM | LTRIM | RTRIM) LEFT_PAREN trimSource (COMMA trimCharacterString)? RIGHT_PAREN
   ;

trimOperands
   : (trimSpecification? trimCharacterString? FROM)? trimSource
   ;

trimSource
   : characterStringValueExpression
   ;

trimSpecification
   : LEADING
   | TRAILING
   | BOTH
   ;

trimCharacterString
   : characterStringValueExpression
   ;

normalizeFunction
   : NORMALIZE LEFT_PAREN characterStringValueExpression (COMMA normalForm)? RIGHT_PAREN
   ;

normalForm
   : NFC
   | NFD
   | NFKC
   | NFKD
   ;

stringLength
   : numericValueExpression
   ;

byteStringFunction
   : byteStringSubstringFunction
   | byteStringTrimFunction
   ;

byteStringSubstringFunction
   : (LEFT | RIGHT) LEFT_PAREN byteStringValueExpression COMMA stringLength RIGHT_PAREN
   ;

byteStringTrimFunction
   : TRIM LEFT_PAREN byteStringTrimOperands RIGHT_PAREN
   ;

byteStringTrimOperands
   : (trimSpecification? trimByteString? FROM)? byteStringTrimSource
   ;

byteStringTrimSource
   : byteStringValueExpression
   ;

trimByteString
   : byteStringValueExpression
   ;

datetimeValueExpression
   : datetimePrimary
   | durationValueExpression PLUS_SIGN datetimePrimary
   | datetimeValueExpression PLUS_SIGN durationTerm
   | datetimeValueExpression MINUS_SIGN durationTerm
   ;

datetimePrimary
   : valueExpressionPrimary
   | datetimeValueFunction
   ;

datetimeValueFunction
   : dateFunction
   | timeFunction
   | datetimeFunction
   | localtimeFunction
   | localdatetimeFunction
   ;

dateFunction
   : CURRENT_DATE
   | DATE LEFT_PAREN dateFunctionParameters? RIGHT_PAREN
   ;

timeFunction
   : CURRENT_TIME
   | ZONED_TIME LEFT_PAREN timeFunctionParameters? RIGHT_PAREN
   ;

localtimeFunction
   : LOCAL_TIME (LEFT_PAREN timeFunctionParameters? RIGHT_PAREN)?
   ;

datetimeFunction
   : CURRENT_TIMESTAMP
   | ZONED_DATETIME LEFT_PAREN datetimeFunctionParameters? RIGHT_PAREN
   ;

localdatetimeFunction
   : LOCAL_TIMESTAMP
   | LOCAL_DATETIME LEFT_PAREN datetimeFunctionParameters? RIGHT_PAREN
   ;

dateFunctionParameters
   : dateString
   | recordConstructor
   ;

timeFunctionParameters
   : timeString
   | recordConstructor
   ;

datetimeFunctionParameters
   : datetimeString
   | recordConstructor
   ;

durationValueExpression
   : durationTerm                                       #durationTermLabel
   | durationValueExpression PLUS_SIGN durationTerm     #durationAdditionLabel
   | durationValueExpression MINUS_SIGN durationTerm    #durationSubtractionLabel
   | datetimeSubtraction                                #datetimeSubtractionLabel
   ;

datetimeSubtraction
   : DURATION_BETWEEN LEFT_PAREN datetimeSubtractionParameters RIGHT_PAREN temporalDurationQualifier?
   ;

datetimeSubtractionParameters
   : datetimeValueExpression1 COMMA datetimeValueExpression2
   ;

durationTerm
   : durationFactor
   | durationTerm ASTERISK factor
   | durationTerm SOLIDUS factor
   | term ASTERISK durationFactor
   ;

durationFactor
   : (PLUS_SIGN | MINUS_SIGN)? durationPrimary
   ;

durationPrimary
   : valueExpressionPrimary
   | durationValueFunction
   ;

datetimeValueExpression1
   : datetimeValueExpression
   ;

datetimeValueExpression2
   : datetimeValueExpression
   ;

durationValueFunction
   : durationFunction
   | durationAbsoluteValueFunction
   ;

durationFunction
   : DURATION LEFT_PAREN durationFunctionParameters RIGHT_PAREN
   ;

durationFunctionParameters
   : durationString
   | recordConstructor
   ;

durationAbsoluteValueFunction
   : ABS LEFT_PAREN durationValueExpression RIGHT_PAREN
   ;

objectName
   : identifier
   ;

objectNameOrBindingVariable
   : REGULAR_IDENTIFIER
   ;

directoryName
   : identifier
   ;

schemaName
   : identifier
   ;

graphName
   : REGULAR_IDENTIFIER
   | delimitedGraphName
   ;

delimitedGraphName
    // DELIMITED_IDENTIFIER
    : DOUBLE_QUOTED_CHARACTER_SEQUENCE
    | ACCENT_QUOTED_CHARACTER_SEQUENCE
    ;

graphTypeName
   : identifier
   ;

nodeTypeName
   : identifier
   ;

edgeTypeName
   : identifier
   ;

bindingTableName
   : REGULAR_IDENTIFIER
   | delimitedBindingTableName
   ;

delimitedBindingTableName
    // DELIMITED_IDENTIFIER
    : DOUBLE_QUOTED_CHARACTER_SEQUENCE
    | ACCENT_QUOTED_CHARACTER_SEQUENCE
    ;

procedureName
   : identifier
   ;

labelName
   : identifier
   ;

propertyName
   : identifier
   ;

fieldName
   : identifier
   ;

elementVariable
   : bindingVariable
   ;

pathVariable
   : bindingVariable
   ;

subpathVariable
   : REGULAR_IDENTIFIER
   ;

bindingVariable
   : REGULAR_IDENTIFIER
   ;

unsignedLiteral
   : unsignedNumericLiteral
   | generalLiteral
   ;

generalLiteral
    : BOOLEAN_LITERAL
    | characterStringLiteral
    | BYTE_STRING_LITERAL
    | temporalLiteral
    | durationLiteral
    | nullLiteral
    | listLiteral
    | recordLiteral
    ;

temporalLiteral
    : dateLiteral
    | timeLiteral
    | datetimeLiteral
//    | sqlDatetimeLiteral
    ;

dateLiteral
    : DATE dateString
    ;

timeLiteral
    : TIME timeString
    ;

datetimeLiteral
    : (DATETIME | TIMESTAMP) datetimeString
    ;

listLiteral
   : listValueConstructorByEnumeration
   ;

recordLiteral
   : recordConstructor
   ;

identifier
    : REGULAR_IDENTIFIER
    // DELIMITED_IDENTIFIER
    | DOUBLE_QUOTED_CHARACTER_SEQUENCE
    | ACCENT_QUOTED_CHARACTER_SEQUENCE
    ;

timeZoneString
    : characterStringLiteral
    ;

characterStringLiteral
    : SINGLE_QUOTED_CHARACTER_SEQUENCE
    | DOUBLE_QUOTED_CHARACTER_SEQUENCE
    ;

// Validate that the NUMERIC_LITERAL is an unsignedInteger.
unsignedInteger
    : NUMERIC_LITERAL
    ;

// Validate that the NUMERIC_LITERAL is an unsignedDecimalInteger.
unsignedDecimalInteger
    : NUMERIC_LITERAL
    ;

// Validate that the NUMERIC_LITERAL is an unsignedNumericLiteral.
unsignedNumericLiteral
    : NUMERIC_LITERAL
    ;

nullLiteral
    : NULL
    ;

dateString
    : characterStringLiteral
    ;

timeString
    : characterStringLiteral
    ;

datetimeString
    : characterStringLiteral
    ;

durationLiteral
    : DURATION durationString
//    | sqlIntervalLiteral
    ;

durationString
    : characterStringLiteral
    ;

// 21.1 Names and Variables

fragment PARAMETER_NAME
    : SEPARATED_IDENTIFIER
    ;

// 21.2 <literal>

BOOLEAN_LITERAL
    : 'TRUE'
    | 'FALSE'
    | 'UNKNOWN'
    ;

SINGLE_QUOTED_CHARACTER_SEQUENCE
    : NO_ESCAPE? UNBROKEN_SINGLE_QUOTED_CHARACTER_SEQUENCE
    ;

DOUBLE_QUOTED_CHARACTER_SEQUENCE
    : NO_ESCAPE? UNBROKEN_DOUBLE_QUOTED_CHARACTER_SEQUENCE
    ;

ACCENT_QUOTED_CHARACTER_SEQUENCE
    :NO_ESCAPE? UNBROKEN_ACCENT_QUOTED_CHARACTER_SEQUENCE
    ;

NO_ESCAPE
    : COMMERCIAL_AT
    ;

fragment UNBROKEN_SINGLE_QUOTED_CHARACTER_SEQUENCE
    : QUOTE SINGLE_QUOTED_CHARACTER_REPRESENTATION* QUOTE
    ;

fragment UNBROKEN_DOUBLE_QUOTED_CHARACTER_SEQUENCE
    : DOUBLE_QUOTE DOUBLE_QUOTED_CHARACTER_REPRESENTATION* DOUBLE_QUOTE
    ;

fragment UNBROKEN_ACCENT_QUOTED_CHARACTER_SEQUENCE
    : GRAVE_ACCENT ACCENT_QUOTED_CHARACTER_REPRESENTATION* GRAVE_ACCENT
    ;

fragment SINGLE_QUOTED_CHARACTER_REPRESENTATION:
	(ESCAPED_CHARACTER | ~['\\\r\n])+
	;

fragment DOUBLE_QUOTED_CHARACTER_REPRESENTATION:
	(ESCAPED_CHARACTER | ~["\\\r\n])+
	;

fragment ACCENT_QUOTED_CHARACTER_REPRESENTATION:
	(ESCAPED_CHARACTER | ~[`\\\r\n])+
	;

fragment ESCAPED_CHARACTER
    : ESCAPED_REVERSE_SOLIDUS
	| ESCAPED_QUOTE
	| ESCAPED_DOUBLE_QUOTE
	| ESCAPED_GRAVE_ACCENT
	| ESCAPED_TAB
	| ESCAPED_BACKSPACE
	| ESCAPED_NEW_LINE
	| ESCAPED_CARRIAGE_RETURN
	| ESCAPED_FORM_FEED
	| ESCAPED_UNICODE4_DIGIT_VALUE
	| ESCAPED_UNICODE6_DIGIT_VALUE
	;

fragment ESCAPED_REVERSE_SOLIDUS: REVERSE_SOLIDUS REVERSE_SOLIDUS;
fragment ESCAPED_QUOTE: REVERSE_SOLIDUS QUOTE;
fragment ESCAPED_DOUBLE_QUOTE: REVERSE_SOLIDUS DOUBLE_QUOTE;
fragment ESCAPED_GRAVE_ACCENT: REVERSE_SOLIDUS GRAVE_ACCENT;
fragment ESCAPED_TAB: REVERSE_SOLIDUS 't';
fragment ESCAPED_BACKSPACE: REVERSE_SOLIDUS 'b';
fragment ESCAPED_NEW_LINE: REVERSE_SOLIDUS 'n';
fragment ESCAPED_CARRIAGE_RETURN: REVERSE_SOLIDUS 'r';
fragment ESCAPED_FORM_FEED: REVERSE_SOLIDUS 'f';
fragment ESCAPED_UNICODE4_DIGIT_VALUE:
	REVERSE_SOLIDUS 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;
fragment ESCAPED_UNICODE6_DIGIT_VALUE:
	REVERSE_SOLIDUS 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;

// Todo: Finish this. It is tricky how it interacts with <separator>
BYTE_STRING_LITERAL
    : 'X' QUOTE SPACE* (HEX_DIGIT SPACE* HEX_DIGIT SPACE*)* QUOTE
    ;

NUMERIC_LITERAL
    : (PLUS_SIGN | MINUS_SIGN)? UNSIGNED_NUMERIC_LITERAL
    ;

fragment UNSIGNED_NUMERIC_LITERAL
    : EXACT_NUMERIC_LITERAL
    | APPROXIMATE_NUMERIC_LITERAL
    ;

fragment EXACT_NUMERIC_LITERAL
    : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATIOON EXACT_NUMBER_SUFFIX
    | UNSIGNED_DECIMAL_INTEGER_IN_COMMON_NOTATION EXACT_NUMBER_SUFFIX?
    | UNSIGNED_DECIMAL_INTEGER EXACT_NUMBER_SUFFIX
    | UNSIGNED_INTEGER
    ;

fragment EXACT_NUMBER_SUFFIX
    : 'M'
    ;

fragment UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATIOON
    : MANTISSA 'E' EXPONENT
    ;

fragment MANTISSA
    : UNSIGNED_DECIMAL_INTEGER_IN_COMMON_NOTATION
    | UNSIGNED_DECIMAL_INTEGER
    ;

fragment EXPONENT
    : SIGNED_DECIMAL_INTEGER
    ;

fragment UNSIGNED_DECIMAL_INTEGER_IN_COMMON_NOTATION
    : UNSIGNED_DECIMAL_INTEGER (PERIOD UNSIGNED_DECIMAL_INTEGER?)
    | PERIOD UNSIGNED_DECIMAL_INTEGER
    ;

fragment UNSIGNED_INTEGER
    : UNSIGNED_DECIMAL_INTEGER
    | UNSIGNED_HEXADECIMAL_INTEGER
    | UNSIGNED_OCTAL_INTEGER
    | UNSIGNED_BINARY_INTEGER
    ;

fragment UNSIGNED_DECIMAL_INTEGER
    : DIGIT (UNDERSCORE? DIGIT)*
    ;

fragment SIGNED_DECIMAL_INTEGER
    : (PLUS_SIGN | MINUS_SIGN)? UNSIGNED_DECIMAL_INTEGER
    ;

UNSIGNED_HEXADECIMAL_INTEGER
    : '0x' ('_'? HEX_DIGIT)+
    ;

UNSIGNED_OCTAL_INTEGER
    : '0o' ('_'? OCTAL_DIGIT)+
    ;

UNSIGNED_BINARY_INTEGER
    : '0b' ('_'? BINARY_DIGIT)+
    ;

fragment APPROXIMATE_NUMERIC_LITERAL
    : UNSIGNED_DECIMAL_IN_SCIENTIFIC_NOTATIOON APPROXIMATE_NUMBER_SUFFIX?
    | UNSIGNED_DECIMAL_INTEGER_IN_COMMON_NOTATION APPROXIMATE_NUMBER_SUFFIX
    | UNSIGNED_DECIMAL_INTEGER APPROXIMATE_NUMBER_SUFFIX
    ;

fragment APPROXIMATE_NUMBER_SUFFIX
    : 'F'
    | 'D'
    ;

// 21.3 <token>, <separator>, and <identifier>

// Reserved words
ABS: 'ABS';
ACOS: 'ACOS';
ALL: 'ALL';
ALL_DIFFERENT: 'ALL_DIFFERENT';
AND: 'AND';
ANY: 'ANY';
ARRAY: 'ARRAY';
AS: 'AS';
ASC: 'ASC';
ASCENDING: 'ASCENDING';
ASIN: 'ASIN';
AT: 'AT';
ATAN: 'ATAN';
AVG: 'AVG';
BIG: 'BIG';
BIGINT: 'BIGINT';
BINARY: 'BINARY';
BOOL: 'BOOL';
BOOLEAN: 'BOOLEAN';
BOTH: 'BOTH';
BTRIM: 'BTRIM';
BY: 'BY';
BYTE_LENGTH: 'BYTE_LENGTH';
BYTES: 'BYTES';
CALL: 'CALL';
CARDINALITY: 'CARDINALITY';
CASE: 'CASE';
CAST: 'CAST';
CEIL: 'CEIL';
CEILING: 'CEILING';
CHAR: 'CHAR';
CHAR_LENGTH: 'CHAR_LENGTH';
CHARACTER_LENGTH: 'CHARACTER_LENGTH';
CHARACTERISTICS: 'CHARACTERISTICS';
CLOSE: 'CLOSE';
COALESCE: 'COALESCE';
COLLECT_LIST: 'COLLECT_LIST';
COMMIT: 'COMMIT';
COPY: 'COPY';
COS: 'COS';
COSH: 'COSH';
COT: 'COT';
COUNT: 'COUNT';
CREATE: 'CREATE';
CURRENT_DATE: 'CURRENT_DATE';
CURRENT_GRAPH: 'CURRENT_GRAPH';
CURRENT_PROPERTY_GRAPH: 'CURRENT_PROPERTY_GRAPH';
CURRENT_SCHEMA: 'CURRENT_SCHEMA';
CURRENT_TIME: 'CURRENT_TIME';
CURRENT_TIMESTAMP: 'CURRENT_TIMESTAMP';
DATE: 'DATE';
DATETIME: 'DATETIME';
DAY: 'DAY';
DEC: 'DEC';
DECIMAL: 'DECIMAL';
DEGREES: 'DEGREES';
DELETE: 'DELETE';
DESC: 'DESC';
DESCENDING: 'DESCENDING';
DETACH: 'DETACH';
DISTINCT: 'DISTINCT';
DOUBLE: 'DOUBLE';
DROP: 'DROP';
DURATION: 'DURATION';
DURATION_BETWEEN: 'DURATION_BETWEEN';
ELEMENT_ID: 'ELEMENT_ID';
ELSE: 'ELSE';
END: 'END';
EXCEPT: 'EXCEPT';
EXISTS: 'EXISTS';
EXP: 'EXP';
FILTER: 'FILTER';
FINISH: 'FINISH';
FLOAT: 'FLOAT';
FLOAT16: 'FLOAT16';
FLOAT32: 'FLOAT32';
FLOAT64: 'FLOAT64';
FLOAT128: 'FLOAT128';
FLOAT256: 'FLOAT256';
FLOOR: 'FLOOR';
FOR: 'FOR';
FROM: 'FROM';
GROUP: 'GROUP';
HAVING: 'HAVING';
HOME_GRAPH: 'HOME_GRAPH';
HOME_PROPERTY_GRAPH: 'HOME_PROPERTY_GRAPH';
HOME_SCHEMA: 'HOME_SCHEMA';
HOUR: 'HOUR';
IF: 'IF';
IMPLIES: 'IMPLIES';
IN: 'IN';
INSERT: 'INSERT';
INT: 'INT';
INTEGER: 'INTEGER';
INT8: 'INT8';
INTEGER8: 'INTEGER8';
INT16: 'INT16';
INTEGER16: 'INTEGER16';
INT32: 'INT32';
INTEGER32: 'INTEGER32';
INT64: 'INT64';
INTEGER64: 'INTEGER64';
INT128: 'INT128';
INTEGER128: 'INTEGER128';
INT256: 'INT256';
INTEGER256: 'INTEGER256';
INTERSECT: 'INTERSECT';
INTERVAL: 'INTERVAL';
IS: 'IS';
LEADING: 'LEADING';
LEFT: 'LEFT';
LET: 'LET';
LIKE: 'LIKE';
LIMIT: 'LIMIT';
LIST: 'LIST';
LN: 'LN';
LOCAL: 'LOCAL';
LOCAL_DATETIME: 'LOCAL_DATETIME';
LOCAL_TIME: 'LOCAL_TIME';
LOCAL_TIMESTAMP: 'LOCAL_TIMESTAMP';
LOG: 'LOG';
LOG10: 'LOG10';
LOWER: 'LOWER';
LTRIM: 'LTRIM';
MATCH: 'MATCH';
MAX: 'MAX';
MIN: 'MIN';
MINUTE: 'MINUTE';
MOD: 'MOD';
MONTH: 'MONTH';
NEXT: 'NEXT';
NODETACH: 'NODETACH';
NORMALIZE: 'NORMALIZE';
NOT: 'NOT';
NOTHING: 'NOTHING';
NULL: 'NULL';
NULLS: 'NULLS';
NULLIF: 'NULLIF';
OCTET_LENGTH: 'OCTET_LENGTH';
OF: 'OF';
OFFSET: 'OFFSET';
OPTIONAL: 'OPTIONAL';
OR: 'OR';
ORDER: 'ORDER';
OTHERWISE: 'OTHERWISE';
PARAMETER: 'PARAMETER';
PARAMETERS: 'PARAMETERS';
PATH: 'PATH';
PATH_LENGTH: 'PATH_LENGTH';
PATHS: 'PATHS';
PERCENTILE_CONT: 'PERCENTILE_CONT';
PERCENTILE_DISC: 'PERCENTILE_DISC';
POWER: 'POWER';
PRECISION: 'PRECISION';
PROPERTY_EXISTS: 'PROPERTY_EXISTS';
RADIANS: 'RADIANS';
REAL: 'REAL';
RECORD: 'RECORD';
REMOVE: 'REMOVE';
REPLACE: 'REPLACE';
RESET: 'RESET';
RETURN: 'RETURN';
RIGHT: 'RIGHT';
ROLLBACK: 'ROLLBACK';
RTRIM: 'RTRIM';
SAME: 'SAME';
SCHEMA: 'SCHEMA';
SECOND: 'SECOND';
SELECT: 'SELECT';
SESSION: 'SESSION';
SESSION_USER: 'SESSION_USER';
SET: 'SET';
SIGNED: 'SIGNED';
SIN: 'SIN';
SINH: 'SINH';
SIZE: 'SIZE';
SKIP_RESERVED_WORD: 'SKIP';
SMALL: 'SMALL';
SMALLINT: 'SMALLINT';
SQRT: 'SQRT';
START: 'START';
STDDEV_POP: 'STDDEV_POP';
STDDEV_SAMP: 'STDDEV_SAMP';
STRING: 'STRING';
SUM: 'SUM';
TAN: 'TAN';
TANH: 'TANH';
THEN: 'THEN';
TIME: 'TIME';
TIMESTAMP: 'TIMESTAMP';
TRAILING: 'TRAILING';
TRIM: 'TRIM';
TYPED: 'TYPED';
UBIGINT: 'UBIGINT';
UINT: 'UINT';
UINT8: 'UINT8';
UINT16: 'UINT16';
UINT32: 'UINT32';
UINT64: 'UINT64';
UINT128: 'UINT128';
UINT256: 'UINT256';
UNION: 'UNION';
UNSIGNED: 'UNSIGNED';
UPPER: 'UPPER';
USE: 'USE';
USMALLINT: 'USMALLINT';
VALUE: 'VALUE';
VARBINARY: 'VARBINARY';
VARCHAR: 'VARCHAR';
VARIABLE: 'VARIABLE';
WHEN: 'WHEN';
WHERE: 'WHERE';
WITH: 'WITH';
XOR: 'XOR';
YEAR: 'YEAR';
YIELD: 'YIELD';
ZONED: 'ZONED';
ZONED_DATETIME: 'ZONED_DATETIME';
ZONED_TIME: 'ZONED_TIME';

// Prereserved words
ABSTRACT: 'ABSTRACT';
AGGREGATE: 'AGGREGATE';
AGGREGATES: 'AGGREGATES';
ALTER: 'ALTER';
CATALOG: 'CATALOG';
CLEAR: 'CLEAR';
CLONE: 'CLONE';
CONSTRAINT: 'CONSTRAINT';
CURRENT_ROLE: 'CURRENT_ROLE';
CURRENT_USER: 'CURRENT_USER';
DATA: 'DATA';
DIRECTORY: 'DIRECTORY';
DRYRUN: 'DRYRUN';
EXACT: 'EXACT';
EXISTING: 'EXISTING';
FUNCTION: 'FUNCTION';
GQLSTATUS: 'GQLSTATUS';
GRANT: 'GRANT';
INSTANT: 'INSTANT';
INFINITY: 'INFINITY';
NUMBER: 'NUMBER';
NUMERIC: 'NUMERIC';
ON: 'ON';
OPEN: 'OPEN';
PARTITION: 'PARTITION';
PROCEDURE: 'PROCEDURE';
PRODUCT: 'PRODUCT';
PROJECT: 'PROJECT';
QUERY: 'QUERY';
RECORDS: 'RECORDS';
REFERENCE: 'REFERENCE';
RENAME: 'RENAME';
REVOKE: 'REVOKE';
SUBSTRING: 'SUBSTRING';
SYSTEM_USER: 'SYSTEM_USER';
TEMPORAL: 'TEMPORAL';
UNIQUE: 'UNIQUE';
UNIT: 'UNIT';
VALUES: 'VALUES';

// Nonreserved words
ACYCLIC: 'ACYCLIC';
BINDING: 'BINDING';
BINDINGS: 'BINDINGS';
CONNECTING: 'CONNECTING';
DESTINATION: 'DESTINATION';
DIFFERENT: 'DIFFERENT';
DIRECTED: 'DIRECTED';
EDGE: 'EDGE';
EDGES: 'EDGES';
ELEMENT: 'ELEMENT';
ELEMENTS: 'ELEMENTS';
FIRST: 'FIRST';
GRAPH: 'GRAPH';
GROUPS: 'GROUPS';
KEEP: 'KEEP';
LABEL: 'LABEL';
LABELED: 'LABELED';
LABELS: 'LABELS';
LAST: 'LAST';
NFC: 'NFC';
NFD: 'NFD';
NFKC: 'NFKC';
NFKD: 'NFKD';
NO: 'NO';
NODE: 'NODE';
NORMALIZED: 'NORMALIZED';
ONLY: 'ONLY';
ORDINALITY: 'ORDINALITY';
PROPERTY: 'PROPERTY';
READ: 'READ';
RELATIONSHIP: 'RELATIONSHIP';
RELATIONSHIPS: 'RELATIONSHIPS';
REPEATABLE: 'REPEATABLE';
SHORTEST: 'SHORTEST';
SIMPLE: 'SIMPLE';
SOURCE: 'SOURCE';
TABLE: 'TABLE';
TEMP: 'TEMP';
TO: 'TO';
TRAIL: 'TRAIL';
TRANSACTION: 'TRANSACTION';
TYPE: 'TYPE';
UNDIRECTED: 'UNDIRECTED';
VERTEX: 'VERTEX';
WALK: 'WALK';
WITHOUT: 'WITHOUT';
WRITE: 'WRITE';
ZONE: 'ZONE';

fragment SEPARATED_IDENTIFIER
    : DELIMITED_IDENTIFIER
    | EXTENDED_IDENTIFIER
    ;

REGULAR_IDENTIFIER
    : IDENTIFIER_START IDENTIFIER_EXTEND*
    ;

fragment EXTENDED_IDENTIFIER
    : IDENTIFIER_EXTEND+
    ;

fragment DELIMITED_IDENTIFIER
    : DOUBLE_QUOTED_CHARACTER_SEQUENCE
    | ACCENT_QUOTED_CHARACTER_SEQUENCE
    ;

SUBSTITUTED_PARAMETER_REFERENCE
    : DOUBLE_DOLLAR_SIGN PARAMETER_NAME
    ;

GENERAL_PARAMETER_REFERENCE
    : DOLLAR_SIGN PARAMETER_NAME
    ;

fragment IDENTIFIER_START
    : ID_Start
    | Pc
    ;

fragment IDENTIFIER_EXTEND
    : ID_Continue
    ;

fragment ID_Start
    : [\p{ID_Start}]
    ;

fragment ID_Continue
    : [\p{ID_Continue}]
    ;

MULTISET_ALTERNATION_OPERATOR: '|+|';

BRACKET_RIGHT_ARROW: ']->';
BRACKET_TILDE_RIGHT_ARROW: ']~>';
CONCATENATION_OPERATOR: '||';
DOUBLE_COLON: '::';
DOUBLE_DOLLAR_SIGN: '$$';
DOUBLE_MINUS_SIGN: '--';
DOUBLE_PERIOD: '..';
GREATER_THAN_OR_EQUALS_OPERATOR: '>=';
LEFT_ARROW: '<-';
LEFT_ARROW_TILDE: '<~';
LEFT_ARROW_BRACKET: '<-[';
LEFT_ARROW_TILDE_BRACKET: '<~[';
LEFT_MINUS_RIGHT: '<->';
LEFT_MINUS_SLASH: '<-/';
LEFT_TILDE_SLASH: '<~/';
LESS_THAN_OR_EQUALS_OPERATOR: '<=';
MINUS_LEFT_BRACKET: '-[';
MINUS_SLASH: '-/';
NOT_EQUALS_OPERATOR: '<>';
RIGHT_ARROW: '->';
RIGHT_BRACKET_MINUS: ']-';
RIGHT_BRACKET_TILDE: ']~';
RIGHT_DOUBLE_ARROW: '=>';
SLASH_MINUS: '/-';
SLASH_MINUS_RIGHT: '/->';
SLASH_TILDE: '/~';
SLASH_TILDE_RIGHT: '/~>';
TILDE_LEFT_BRACKET: '~[';
TILDE_RIGHT_ARROW: '~>';
TILDE_SLASH: '~/';
DOUBLE_SOLIDUS: '//';

EDGE_SYNONYM
    : EDGE
    | RELATIONSHIP
    ;

EDGES_SYNONYM
    : EDGES
    | RELATIONSHIPS
    ;

NODE_SYNONYM
    : NODE
    | VERTEX
    ;

IMPLIES_SYNONYM
    : RIGHT_DOUBLE_ARROW
    | IMPLIES
    ;

// 21.4 GQL terminal characters


AMPERSAND: '&';
ASTERISK: '*';
COLON: ':';
COMMA: ',';
COMMERCIAL_AT: '@';
DOLLAR_SIGN: '$';
DOUBLE_QUOTE: '"';
EQUALS_OPERATOR: '=';
EXCLAMATION_MARK: '!';
RIGHT_ANGLE_BRACKET: '>';
GRAVE_ACCENT: '`';
LEFT_BRACE: '{';
LEFT_BRACKET: '[';
LEFT_PAREN: '(';
LEFT_ANGLE_BRACKET: '<';
MINUS_SIGN: '-';
PERCENT: '%';
PERIOD: '.';
PLUS_SIGN: '+';
QUESTION_MARK: '?';
QUOTE: '\'';
REVERSE_SOLIDUS: '\\';
RIGHT_BRACE: '}';
RIGHT_BRACKET: ']';
RIGHT_PAREN: ')';
SOLIDUS: '/';
TILDE: '~';
UNDERSCORE: '_';
VERTICAL_BAR: '|';

fragment HEX_DIGIT
    : [0-9a-f]
    ;

fragment DIGIT
    : [0-9]
    ;

fragment OCTAL_DIGIT
    : [0-7]
    ;

fragment BINARY_DIGIT
    : [0-1]
    ;

SP
  : (WHITESPACE)+
  -> channel(HIDDEN)
  ;

WHITESPACE
    : SPACE
    | TAB
    | LF
    | VT
    | FF
    | CR
    | FS
    | GS
    | RS
    | US
    | '\u1680'
    | '\u180e'
    | '\u2000'
    | '\u2001'
    | '\u2002'
    | '\u2003'
    | '\u2004'
    | '\u2005'
    | '\u2006'
    | '\u2008'
    | '\u2009'
    | '\u200a'
    | '\u2028'
    | '\u2029'
    | '\u205f'
    | '\u3000'
    | '\u00a0'
    | '\u2007'
    | '\u202f'
    ;

COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

fragment GS : [\u001D];

fragment FS : [\u001C];

fragment CR : [\r];

fragment Sc : [\p{Sc}];

fragment SPACE : [ ];

fragment Pc : [\p{Pc}];

fragment TAB : [\t];

fragment LF : [\n];

fragment VT : [\u000B];

fragment US : [\u001F];

fragment FF: [\f];

fragment RS: [\u001E];
