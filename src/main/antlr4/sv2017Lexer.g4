// Originally retrived from https://github.com/Nic30/hdlConvertor March 2020
// See LICENSE.Nic30 for license details.
// NOTICE : Added limited support for pre-processor lexing

/*
 * SystemVerilog IEEE1800-2017 standard grammar
 *
 * This grammar is generated by hdlConvertor/utils/sv/main.py from the PDF with SystemVerilog IEEE1800-2017 standard.
 * The grammar is target language dependent, but mentioned script can generate grammar for multiple target languages (java, python, c++).
 **/
lexer grammar sv2017Lexer;

@lexer::members{
  public static final int PREPROC=2;
  public static final int COMMENTS=3;
  public static final int WHITESPACES=4;
}


/* SUPPLEMENTARY PARTIAL PREPROCESSING TOKENS */

TICK_TIMESCALE: '`timescale' ;
TICK_DEFAULT_NETTYPE: '`default_nettype' ;
TICK_INCLUDE: '`include' ;

TICK_DEFINE: '`define' ;
TICK_IFNDEF: '`ifndef' ;
TICK_IFDEF: '`ifdef' ;
TICK_ELSIF: '`elsif' ;
TICK_UNDEF: '`undef' ;

TICK_KW_VALUE:
  ( TICK_DEFINE
    | TICK_IFNDEF
    | TICK_IFDEF
    | TICK_ELSIF
    | TICK_UNDEF
    ) ( ~[\r\n]*? '\\' '\r'? '\n' )* ~[\r\n]*? '\r'? '\n' -> channel(2) ;

TICK_ELSE: '`else' -> channel(2) ;
TICK_ENDIF: '`endif' -> channel(2) ;

TICK_BEGIN_KEYWORDS: '`begin_keywords' -> channel(2) ;
TICK_END_KEYWORDS: '`end_keywords' -> channel(2) ;
TICK_PRAGMA: '`pragma' -> channel(2) ;
TICK_UNDEFINEALL: '`undefineall' -> channel(2) ;
TICK_RESETALL: '`resetall' -> channel(2) ;
TICK_CELLDEFINE: '`celldefine' -> channel(2) ;
TICK_ENDCELLDEFINE: '`endcelldefine' -> channel(2) ;

TICK_LINE: '`line' .*? '\r'? '\n' -> channel(2) ;

/* END OF SUPPLEMENTARY PARTIAL PREPROCESSING TOKENS */

KW_DOLAR_ERROR: '$error';
KW_DOLAR_FATAL: '$fatal';
KW_DOLAR_FULLSKEW: '$fullskew';
KW_DOLAR_HOLD: '$hold';
KW_DOLAR_INFO: '$info';
KW_DOLAR_NOCHANGE: '$nochange';
KW_DOLAR_PERIOD: '$period';
KW_DOLAR_RECOVERY: '$recovery';
KW_DOLAR_RECREM: '$recrem';
KW_DOLAR_REMOVAL: '$removal';
KW_DOLAR_ROOT: '$root';
KW_DOLAR_SETUP: '$setup';
KW_DOLAR_SETUPHOLD: '$setuphold';
KW_DOLAR_SKEW: '$skew';
KW_DOLAR_TIMESKEW: '$timeskew';
KW_DOLAR_UNIT: '$unit';
KW_DOLAR_WARNING: '$warning';
KW_DOLAR_WIDTH: '$width';
KW_1STEP: '1step';
KW_PATHPULSE_DOLAR: 'PATHPULSE$';
KW_ACCEPT_ON: 'accept_on';
KW_ALIAS: 'alias';
KW_ALWAYS: 'always';
KW_ALWAYS_COMB: 'always_comb';
KW_ALWAYS_FF: 'always_ff';
KW_ALWAYS_LATCH: 'always_latch';
KW_AND: 'and';
KW_ASSERT: 'assert';
KW_ASSIGN: 'assign';
KW_ASSUME: 'assume';
KW_AUTOMATIC: 'automatic';
KW_BEFORE: 'before';
KW_BEGIN: 'begin';
KW_BIND: 'bind';
KW_BINS: 'bins';
KW_BINSOF: 'binsof';
KW_BIT: 'bit';
KW_BREAK: 'break';
KW_BUF: 'buf';
KW_BUFIF0: 'bufif0';
KW_BUFIF1: 'bufif1';
KW_BYTE: 'byte';
KW_CASE: 'case';
KW_CASEX: 'casex';
KW_CASEZ: 'casez';
KW_CELL: 'cell';
KW_CHANDLE: 'chandle';
KW_CHECKER: 'checker';
KW_CLASS: 'class';
KW_CLOCKING: 'clocking';
KW_CMOS: 'cmos';
KW_CONFIG: 'config';
KW_CONST: 'const';
KW_CONSTRAINT: 'constraint';
KW_CONTEXT: 'context';
KW_CONTINUE: 'continue';
KW_COVER: 'cover';
KW_COVERGROUP: 'covergroup';
KW_COVERPOINT: 'coverpoint';
KW_CROSS: 'cross';
KW_DEASSIGN: 'deassign';
KW_DEFAULT: 'default';
KW_DEFPARAM: 'defparam';
KW_DESIGN: 'design';
KW_DISABLE: 'disable';
KW_DIST: 'dist';
KW_DO: 'do';
KW_EDGE: 'edge';
KW_ELSE: 'else';
KW_END: 'end';
KW_ENDCASE: 'endcase';
KW_ENDCHECKER: 'endchecker';
KW_ENDCLASS: 'endclass';
KW_ENDCLOCKING: 'endclocking';
KW_ENDCONFIG: 'endconfig';
KW_ENDFUNCTION: 'endfunction';
KW_ENDGENERATE: 'endgenerate';
KW_ENDGROUP: 'endgroup';
KW_ENDINTERFACE: 'endinterface';
KW_ENDMODULE: 'endmodule';
KW_ENDPACKAGE: 'endpackage';
KW_ENDPRIMITIVE: 'endprimitive';
KW_ENDPROGRAM: 'endprogram';
KW_ENDPROPERTY: 'endproperty';
KW_ENDSEQUENCE: 'endsequence';
KW_ENDSPECIFY: 'endspecify';
KW_ENDTASK: 'endtask';
KW_ENUM: 'enum';
KW_EVENT: 'event';
KW_EVENTUALLY: 'eventually';
KW_EXPECT: 'expect';
KW_EXPORT: 'export';
KW_EXTENDS: 'extends';
KW_EXTERN: 'extern';
KW_FINAL: 'final';
KW_FIRST_MATCH: 'first_match';
KW_FOR: 'for';
KW_FORCE: 'force';
KW_FOREACH: 'foreach';
KW_FOREVER: 'forever';
KW_FORK: 'fork';
KW_FORKJOIN: 'forkjoin';
KW_FUNCTION: 'function';
KW_GENERATE: 'generate';
KW_GENVAR: 'genvar';
KW_GLOBAL: 'global';
KW_HIGHZ0: 'highz0';
KW_HIGHZ1: 'highz1';
KW_IF: 'if';
KW_IFF: 'iff';
KW_IFNONE: 'ifnone';
KW_IGNORE_BINS: 'ignore_bins';
KW_ILLEGAL_BINS: 'illegal_bins';
KW_IMPLEMENTS: 'implements';
KW_IMPLIES: 'implies';
KW_IMPORT: 'import';
KW_INITIAL: 'initial';
KW_INOUT: 'inout';
KW_INPUT: 'input';
KW_INSIDE: 'inside';
KW_INSTANCE: 'instance';
KW_INT: 'int';
KW_INTEGER: 'integer';
KW_INTERCONNECT: 'interconnect';
KW_INTERFACE: 'interface';
KW_INTERSECT: 'intersect';
KW_JOIN: 'join';
KW_JOIN_ANY: 'join_any';
KW_JOIN_NONE: 'join_none';
KW_LARGE: 'large';
KW_LET: 'let';
KW_LIBLIST: 'liblist';
KW_LOCAL: 'local';
KW_LOCALPARAM: 'localparam';
KW_LOGIC: 'logic';
KW_LONGINT: 'longint';
KW_MACROMODULE: 'macromodule';
KW_MATCHES: 'matches';
KW_MEDIUM: 'medium';
KW_MODPORT: 'modport';
KW_MODULE: 'module';
KW_NAND: 'nand';
KW_NEGEDGE: 'negedge';
KW_NETTYPE: 'nettype';
KW_NEW: 'new';
KW_NEXTTIME: 'nexttime';
KW_NMOS: 'nmos';
KW_NOR: 'nor';
KW_NONE: 'none';
KW_NOSHOWCANCELLED: 'noshowcancelled';
KW_NOT: 'not';
KW_NOTIF0: 'notif0';
KW_NOTIF1: 'notif1';
KW_NULL: 'null';
KW_OPTION: 'option';
KW_OR: 'or';
KW_OUTPUT: 'output';
KW_PACKAGE: 'package';
KW_PACKED: 'packed';
KW_PARAMETER: 'parameter';
KW_PMOS: 'pmos';
KW_POSEDGE: 'posedge';
KW_PRIMITIVE: 'primitive';
KW_PRIORITY: 'priority';
KW_PROGRAM: 'program';
KW_PROPERTY: 'property';
KW_PROTECTED: 'protected';
KW_PULL0: 'pull0';
KW_PULL1: 'pull1';
KW_PULLDOWN: 'pulldown';
KW_PULLUP: 'pullup';
KW_PULSESTYLE_ONDETECT: 'pulsestyle_ondetect';
KW_PULSESTYLE_ONEVENT: 'pulsestyle_onevent';
KW_PURE: 'pure';
KW_RAND: 'rand';
KW_RANDC: 'randc';
KW_RANDCASE: 'randcase';
KW_RANDOMIZE: 'randomize';
KW_RANDSEQUENCE: 'randsequence';
KW_RCMOS: 'rcmos';
KW_REAL: 'real';
KW_REALTIME: 'realtime';
KW_REF: 'ref';
KW_REG: 'reg';
KW_REJECT_ON: 'reject_on';
KW_RELEASE: 'release';
KW_REPEAT: 'repeat';
KW_RESTRICT: 'restrict';
KW_RETURN: 'return';
KW_RNMOS: 'rnmos';
KW_RPMOS: 'rpmos';
KW_RTRAN: 'rtran';
KW_RTRANIF0: 'rtranif0';
KW_RTRANIF1: 'rtranif1';
KW_S_ALWAYS: 's_always';
KW_S_EVENTUALLY: 's_eventually';
KW_S_NEXTTIME: 's_nexttime';
KW_S_UNTIL: 's_until';
KW_S_UNTIL_WITH: 's_until_with';
KW_SAMPLE: 'sample';
KW_SCALARED: 'scalared';
KW_SEQUENCE: 'sequence';
KW_SHORTINT: 'shortint';
KW_SHORTREAL: 'shortreal';
KW_SHOWCANCELLED: 'showcancelled';
KW_SIGNED: 'signed';
KW_SMALL: 'small';
KW_SOFT: 'soft';
KW_SOLVE: 'solve';
KW_SPECIFY: 'specify';
KW_SPECPARAM: 'specparam';
KW_STATIC: 'static';
KW_STD: 'std';
KW_STRING: 'string';
KW_STRONG: 'strong';
KW_STRONG0: 'strong0';
KW_STRONG1: 'strong1';
KW_STRUCT: 'struct';
KW_SUPER: 'super';
KW_SUPPLY0: 'supply0';
KW_SUPPLY1: 'supply1';
KW_SYNC_ACCEPT_ON: 'sync_accept_on';
KW_SYNC_REJECT_ON: 'sync_reject_on';
KW_TABLE: 'table' -> pushMode(TABLE_MODE);
KW_TAGGED: 'tagged';
KW_TASK: 'task';
KW_THIS: 'this';
KW_THROUGHOUT: 'throughout';
KW_TIME: 'time';
KW_TIMEPRECISION: 'timeprecision';
KW_TIMEUNIT: 'timeunit';
KW_TRAN: 'tran';
KW_TRANIF0: 'tranif0';
KW_TRANIF1: 'tranif1';
KW_TRI: 'tri';
KW_TRI0: 'tri0';
KW_TRI1: 'tri1';
KW_TRIAND: 'triand';
KW_TRIOR: 'trior';
KW_TRIREG: 'trireg';
KW_TYPE: 'type';
KW_TYPE_OPTION: 'type_option';
KW_TYPEDEF: 'typedef';
KW_UNION: 'union';
KW_UNIQUE: 'unique';
KW_UNIQUE0: 'unique0';
KW_UNSIGNED: 'unsigned';
KW_UNTIL: 'until';
KW_UNTIL_WITH: 'until_with';
KW_UNTYPED: 'untyped';
KW_USE: 'use';
KW_UWIRE: 'uwire';
KW_VAR: 'var';
KW_VECTORED: 'vectored';
KW_VIRTUAL: 'virtual';
KW_VOID: 'void';
KW_WAIT: 'wait';
KW_WAIT_ORDER: 'wait_order';
KW_WAND: 'wand';
KW_WEAK: 'weak';
KW_WEAK0: 'weak0';
KW_WEAK1: 'weak1';
KW_WHILE: 'while';
KW_WILDCARD: 'wildcard';
KW_WIRE: 'wire';
KW_WITH: 'with';
KW_WITHIN: 'within';
KW_WOR: 'wor';
KW_XNOR: 'xnor';
KW_XOR: 'xor';
EDGE_CONTROL_SPECIFIER:
 'edge' LSQUARE_BR EDGE_DESCRIPTOR ( COMMA EDGE_DESCRIPTOR )* RSQUARE_BR;
TIME_LITERAL:
 ( UNSIGNED_NUMBER 
  | FIXED_POINT_NUMBER 
  ) TIME_UNIT;
ANY_BASED_NUMBER:
 OCTAL_NUMBER 
  | DECIMAL_NUMBER_WITH_BASE 
  | BINARY_NUMBER 
  | DECIMAL_INVALID_NUMBER_WITH_BASE 
  | DECIMAL_TRISTATE_NUMBER_WITH_BASE 
  | HEX_NUMBER 
 ;
BASED_NUMBER_WITH_SIZE:
  UNSIGNED_NUMBER ANY_BASED_NUMBER;
BASED_NUMBER_WITH_TICK_SIZE:
  TICK_IDENTIFIER ANY_BASED_NUMBER;

REAL_NUMBER_WITH_EXP:
 UNSIGNED_NUMBER ( DOT UNSIGNED_NUMBER )? EXP ( SIGN )? UNSIGNED_NUMBER;
FIXED_POINT_NUMBER:
 UNSIGNED_NUMBER DOT UNSIGNED_NUMBER;
UNSIGNED_NUMBER:
 DECIMAL_DIGIT ( UNDERSCORE 
                  | DECIMAL_DIGIT 
                  )*;
UNBASED_UNSIZED_LITERAL:
 APOSTROPHE Z_OR_X 
  | '\'0' 
  | '\'1' 
 ;
STRING_LITERAL:
 DBLQUOTE ( ANY_ASCII_CHARACTERS )* DBLQUOTE;
C_IDENTIFIER:
 [a-zA-Z_] ( [a-zA-Z0-9_] )*;
TICK: '`';
TICK_IDENTIFIER: TICK C_IDENTIFIER;
ESCAPED_IDENTIFIER:
 BACKSLASH ( ANY_PRINTABLE_ASCII_CHARACTER_EXCEPT_WHITE_SPACE )* WHITE_SPACE;
SIMPLE_IDENTIFIER:
 [a-zA-Z_] ( [a-zA-Z0-9_$] )*;
SYSTEM_TF_IDENTIFIER:
 DOLAR ( [a-zA-Z0-9_$] )+;
SEMI: ';';
LPAREN: '(';
RPAREN: ')';
LSQUARE_BR: '[';
RSQUARE_BR: ']';
LBRACE: '{';
RBRACE: '}';
APOSTROPHE: '\'';
APOSTROPHE_LBRACE: '\'{';
SHIFT_LEFT: '<<';
SHIFT_RIGHT: '>>';
ARITH_SHIFT_LEFT: '<<<';
ARITH_SHIFT_RIGHT: '>>>';
DOLAR: '$';
MOD: '%';
NOT: '!';
NEG: '~';
NAND: '~&';
NOR: '~|';
XOR: '^';
NXOR: '~^';
XORN: '^~';
COMMA: ',';
DOT: '.';
QUESTIONMARK: '?';
COLON: ':';
DOUBLE_COLON: '::';
EQ: '==';
NEQ: '!=';
CASE_EQ: '===';
CASE_NEQ: '!==';
WILDCARD_EQ: '==?';
WILDCARD_NEQ: '!=?';
ASSIGN: '=';
LT: '<';
GT: '>';
GE: '>=';
LE: '<=';
PLUS_ASSIGN: '+=';
MINUS_ASSIGN: '-=';
MUL_ASSIGN: '*=';
DIV_ASSIGN: '/=';
MOD_ASSIGN: '%=';
AND_ASSIGN: '&=';
OR_ASSIGN: '|=';
XOR_ASSIGN: '^=';
SHIFT_LEFT_ASSIGN: '<<=';
SHIFT_RIGHT_ASSIGN: '>>=';
ARITH_SHIFT_LEFT_ASSIGN: '<<<=';
ARITH_SHIFT_RIGHT_ASSIGN: '>>>=';
PLUS: '+';
MINUS: '-';
AMPERSAND: '&';
LOG_AND: '&&';
BAR: '|';
LOG_OR: '||';
BACKSLASH: '\\';
MUL: '*';
DIV: '/';
DOUBLESTAR: '**';
BI_DIR_ARROW: '<->';
ARROW: '->';
DOUBLE_RIGHT_ARROW: '->>';
INCR: '++';
DECR: '--';
DIST_WEIGHT_ASSIGN: ':=';
OVERLAPPING_IMPL: '|->';
NONOVERLAPPING_IMPL: '|=>';
IMPLIES: '=>';
IMPLIES_P: '-=>';
IMPLIES_N: '+=>';
PATH_FULL: '*>';
HASH_MINUS_HASH: '#-#';
HASH_EQ_HASH: '#=#';
AT: '@';
DOUBLE_AT: '@@';
HASH: '#';
DOUBLE_HASH: '##';
TRIPLE_AND: '&&&';
ONE_LINE_COMMENT:
 '//' .*? ( '\r' )? ( EOF 
                  | '\n' 
                  ) -> channel(3);
BLOCK_COMMENT:
 '/*' .*? '*/' -> channel(3);
WHITE_SPACE: [ \t\n\r\f] + -> channel(4);
fragment EDGE_DESCRIPTOR:
 Z_OR_X ZERO_OR_ONE 
  | ZERO_OR_ONE Z_OR_X 
  | '01' 
  | '10' 
 ;
fragment ZERO_OR_ONE: [01];
fragment Z_OR_X: [xXzZ];
fragment TIME_UNIT:
 's' 
  | 'ms' 
  | 'us' 
  | 'ns' 
  | 'ps' 
  | 'fs' 
 ;
fragment DECIMAL_NUMBER_WITH_BASE:
 DECIMAL_BASE UNSIGNED_NUMBER;
fragment DECIMAL_INVALID_NUMBER_WITH_BASE:
 DECIMAL_BASE X_DIGIT ( UNDERSCORE )*;
fragment DECIMAL_TRISTATE_NUMBER_WITH_BASE:
 DECIMAL_BASE Z_DIGIT ( UNDERSCORE )*;
fragment BINARY_NUMBER:
 BINARY_BASE BINARY_VALUE;
fragment OCTAL_NUMBER:
 OCTAL_BASE OCTAL_VALUE;
fragment HEX_NUMBER:
 HEX_BASE HEX_VALUE;
fragment SIGN:
 PLUS 
  | MINUS 
 ;
fragment SIZE: NON_ZERO_UNSIGNED_NUMBER;
fragment NON_ZERO_UNSIGNED_NUMBER:
 NON_ZERO_DECIMAL_DIGIT ( UNDERSCORE 
                          | DECIMAL_DIGIT 
                          )*;
fragment EXP: [eE];
fragment BINARY_VALUE:
 BINARY_DIGIT ( UNDERSCORE 
                  | BINARY_DIGIT 
                  )*;
fragment OCTAL_VALUE:
 OCTAL_DIGIT ( UNDERSCORE 
              | OCTAL_DIGIT 
              )*;
fragment HEX_VALUE:
 HEX_DIGIT ( UNDERSCORE 
              | HEX_DIGIT 
              )*;
fragment DECIMAL_BASE:
 APOSTROPHE ( WHITE_SPACE )? ( [sS] )? ( WHITE_SPACE )? [dD] ( WHITE_SPACE )?;
fragment BINARY_BASE:
 APOSTROPHE ( WHITE_SPACE )? ( [sS] )? ( WHITE_SPACE )? [bB] ( WHITE_SPACE )?;
fragment OCTAL_BASE:
 APOSTROPHE ( WHITE_SPACE )? ( [sS] )? ( WHITE_SPACE )? [oO] ( WHITE_SPACE )?;
fragment HEX_BASE:
 APOSTROPHE ( WHITE_SPACE )? ( [sS] )? ( WHITE_SPACE )? [hH] ( WHITE_SPACE )?;
fragment NON_ZERO_DECIMAL_DIGIT: [1-9];
fragment DECIMAL_DIGIT: [0-9];
fragment BINARY_DIGIT:
 X_DIGIT 
  | Z_DIGIT 
  | [01] 
 ;
fragment OCTAL_DIGIT:
 X_DIGIT 
  | Z_DIGIT 
  | [0-7] 
 ;
fragment HEX_DIGIT:
 X_DIGIT 
  | Z_DIGIT 
  | [0-9a-fA-F] 
 ;
fragment X_DIGIT: [xX];
fragment Z_DIGIT:
 QUESTIONMARK 
  | [zZ] 
 ;
fragment DBLQUOTE: '"';
fragment UNDERSCORE: '_';
fragment ANY_ASCII_CHARACTERS:
 ~["\\\r\n] 
  | '\\\n' 
  | '\\\r\n' 
  | '\\' [nt\\"vfa%] 
  | '\\' [0-9] [0-9]? [0-9]? 
  | '\\' 'x' [0-9A-Fa-f] [0-9A-Fa-f]? 
 ;
fragment ANY_PRINTABLE_ASCII_CHARACTER_EXCEPT_WHITE_SPACE: '\u0021'..'\u007E';

mode TABLE_MODE;
    KW_ENDTABLE: 'endtable' -> popMode;
    LEVEL_SYMBOL:
 QUESTIONMARK 
  | [01xXbB] 
 ;
    EDGE_SYMBOL:
 MUL 
  | [rRfFpPnN] 
 ;
    TABLE_MODE_BLOCK_COMMENT:
 '/*' .*? '*/' -> channel(3),type(BLOCK_COMMENT);
    TABLE_MODE_COLON: ':' -> type(COLON);
    TABLE_MODE_LPAREN: '(' -> type(LPAREN);
    TABLE_MODE_MINUS: '-' -> type(MINUS);
    TABLE_MODE_ONE_LINE_COMMENT:
 '//' .*? ( '\r' )? ( EOF 
                  | '\n' 
                  ) -> channel(3),type(ONE_LINE_COMMENT);
    TABLE_MODE_RPAREN: ')' -> type(RPAREN);
    TABLE_MODE_SEMI: ';' -> type(SEMI);
    TABLE_MODE_WHITE_SPACE: [ \t\n\r] + -> channel(HIDDEN),type(WHITE_SPACE);
