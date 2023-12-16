lexer grammar GQLLexer;

options { caseInsensitive = true; }

// 21.1 Names and Variables

fragment PARAMETER_NAME
    : SEPARATED_IDENTIFIER
    ;

// 21.2 <literal>

BOOLEAN_LITERAL
    : 'TRUE'
    | 'FASLE'
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

fragment SIGN
    : PLUS_SIGN
    | MINUS_SIGN
    ;

NUMERIC_LITERAL
    : SIGN? UNSIGNED_NUMERIC_LITERAL
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
    : SIGN? UNSIGNED_DECIMAL_INTEGER
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
FALSE: 'FALSE';
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
fragment TRUE: 'TRUE';
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
fragment UNKNOWN: 'UNKNOWN';
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
