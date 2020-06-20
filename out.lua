local m = require 'pegparser.parser'
local coder = require 'pegparser.coder'
local util = require'pegparser.util'

g = [[
translationUnit <-  Spacing (externalDeclaration  /  !(EOT  /  !.) %{Err_001} .)+ EOT
externalDeclaration <-  functionDefinition  /  declaration
functionDefinition <-  declarationSpecifiers declarator declarationList? compoundStatemet
declarationList <-  declaration+
statement       <-  labeledStatement  /  compoundStatemet  /  expressionStatement  /  selectionStatement  /  iterationStatement  /  jumpStatement
labeledStatement <-  Identifier COLON statement  /  CASE constantExpression^Err_002 COLON^Err_003 statement^Err_004  /  DEFAULT COLON^Err_005 statement^Err_006
compoundStatemet <-  LWING (declaration  /  statement)* RWING
expressionStatement <-  expression? SEMI
selectionStatement <-  IF LPAR^Err_007 expression^Err_008 RPAR^Err_009 statement^Err_010 (ELSE statement^Err_011  /  !(WHILE  /  VOLATILE  /  VOID  /  UNSIGNED  /  UNION  /  TYPEDEF  /  TILDA  /  StringLiteral  /  SWITCH  /  STRUCT  /  STDCALL  /  STATIC  /  STAR  /  SIZEOF  /  SIGNED  /  SHORT  /  SEMI  /  RWING  /  RETURN  /  RESTRICT  /  REGISTER  /  PLUS  /  MINUS  /  LWING  /  LPAR  /  LONG  /  Identifier  /  INT  /  INLINE  /  INC  /  IF  /  GOTO  /  FOR  /  FLOAT  /  EXTERN  /  ENUM  /  ELSE  /  DOUBLE  /  DO  /  DEFAULT  /  DECLSPEC  /  DEC  /  Constant  /  CONTINUE  /  CONST  /  COMPLEX  /  CHAR  /  CASE  /  BREAK  /  BOOL  /  BANG  /  AUTO  /  ATTRIBUTE  /  AND  /  !.) %{Err_012} .)?  /  SWITCH LPAR^Err_013 expression^Err_014 RPAR^Err_015 statement^Err_016
iterationStatement <-  WHILE LPAR expression RPAR statement  /  DO statement^Err_017 WHILE^Err_018 LPAR^Err_019 expression^Err_020 RPAR^Err_021 SEMI^Err_022  /  FOR LPAR expression? SEMI expression? SEMI expression? RPAR statement  /  FOR LPAR^Err_023 declaration^Err_024 (expression  /  !(SEMI  /  !.) %{Err_025} .)? SEMI^Err_026 (expression  /  !(RPAR  /  !.) %{Err_027} .)? RPAR^Err_028 statement^Err_029
jumpStatement   <-  GOTO Identifier^Err_030 SEMI^Err_031  /  CONTINUE SEMI^Err_032  /  BREAK SEMI^Err_033  /  RETURN (expression  /  !(SEMI  /  !.) %{Err_034} .)? SEMI^Err_035
declaration     <-  declarationSpecifiers initDeclaratorList? SEMI
initDeclaratorList <-  initDeclarator (COMMA initDeclarator)*
initDeclarator  <-  declarator (EQU initializer)?
constantExpression <-  conditionalExpression
conditionalExpression <-  logicalORExpression (QUERY expression^Err_036 COLON^Err_037 logicalORExpression^Err_038)*
logicalORExpression <-  logicalANDExpression (OROR logicalANDExpression^Err_039)*
logicalANDExpression <-  inclusiveORExpression (ANDAND inclusiveORExpression^Err_040)*
inclusiveORExpression <-  exclusiveORExpression (OR exclusiveORExpression^Err_041)*
exclusiveORExpression <-  andExpression (HAT andExpression^Err_042)*
andExpression   <-  equalityExpression (AND equalityExpression)*
equalityExpression <-  relationalExpression ((EQUEQU  /  BANGEQU) relationalExpression^Err_043)*
relationalExpression <-  shiftExpression ((LE  /  GE  /  LT  /  GT) shiftExpression^Err_044)*
shiftExpression <-  additiveExpression ((LEFT  /  RIGHT) additiveExpression^Err_045)*
additiveExpression <-  multiplicativeExpression ((PLUS  /  MINUS) multiplicativeExpression)*
multiplicativeExpression <-  castExpression ((STAR  /  DIV  /  MOD) castExpression)*
castExpression  <-  (LPAR typeName RPAR)* unaryExpression
unaryExpression <-  postfixExpression  /  INC unaryExpression  /  DEC unaryExpression  /  unaryOperator castExpression  /  SIZEOF (unaryExpression  /  LPAR^Err_046 typeName^Err_047 RPAR^Err_048)^Err_049
unaryOperator   <-  AND  /  STAR  /  PLUS  /  MINUS  /  TILDA  /  BANG
postfixExpression <-  (primaryExpression  /  LPAR typeName RPAR LWING initializerList COMMA? RWING) (LBRK expression RBRK  /  LPAR argumentExpressionList? RPAR  /  DOT Identifier  /  PTR Identifier^Err_050  /  INC  /  DEC)*
typeName        <-  specifierQualifierList abstractDeclarator?
initializerList <-  designation? initializer (COMMA designation? initializer)*
designation     <-  designator+ EQU
designator      <-  LBRK constantExpression RBRK  /  DOT Identifier
initializer     <-  assignmentExpression  /  LWING initializerList COMMA? RWING
argumentExpressionList <-  assignmentExpression (COMMA assignmentExpression)*
specifierQualifierList <-  typeQualifier* typedefName typeQualifier*  /  (typeSpecifier  /  typeQualifier)+
typeQualifier   <-  CONST  /  RESTRICT  /  VOLATILE  /  DECLSPEC LPAR^Err_051 Identifier^Err_052 RPAR^Err_053
typedefName     <-  Identifier
typeSpecifier   <-  VOID  /  CHAR  /  SHORT  /  INT  /  LONG  /  FLOAT  /  DOUBLE  /  SIGNED  /  UNSIGNED  /  BOOL  /  COMPLEX  /  structOrUnionSpecifier  /  enumSpecifier
structOrUnionSpecifier <-  structOrUnion (Identifier? LWING structDeclaration+ RWING  /  Identifier^Err_054)^Err_055
enumSpecifier   <-  ENUM (Identifier? LWING enumeratorList (COMMA  /  !(RWING  /  !.) %{Err_056} .)? RWING^Err_057  /  Identifier^Err_058)^Err_059
enumeratorList  <-  enumerator (COMMA enumerator  /  !(RWING  /  COMMA  /  !.) %{Err_060} .)*
enumerator      <-  EnumerationConstant (EQU constantExpression^Err_061  /  !(RWING  /  COMMA  /  !.) %{Err_062} .)?
structOrUnion   <-  STRUCT  /  UNION
structDeclaration <-  specifierQualifierList structDeclaratorList SEMI
structDeclaratorList <-  structDeclarator (COMMA structDeclarator)*
structDeclarator <-  declarator? COLON constantExpression  /  declarator
declarator      <-  pointer? directDeclarator
pointer         <-  (STAR typeQualifier*)+
directDeclarator <-  (Identifier  /  LPAR declarator RPAR) (LBRK typeQualifier* assignmentExpression? RBRK  /  LBRK STATIC (typeQualifier  /  !(TILDA  /  StringLiteral  /  STAR  /  SIZEOF  /  PLUS  /  MINUS  /  LPAR  /  Identifier  /  INC  /  DEC  /  Constant  /  BANG  /  AND  /  !.) %{Err_063} .)* assignmentExpression^Err_064 RBRK^Err_065  /  LBRK typeQualifier+ STATIC assignmentExpression RBRK  /  LBRK typeQualifier* STAR RBRK  /  LPAR parameterTypeList RPAR  /  LPAR identifierList? RPAR)*
parameterTypeList <-  parameterList (COMMA ELLIPSIS)?
identifierList  <-  Identifier (COMMA Identifier)*
parameterList   <-  parameterDeclaration (COMMA parameterDeclaration)*
parameterDeclaration <-  declarationSpecifiers (declarator  /  abstractDeclarator)?
declarationSpecifiers <-  (storageClassSpecifier  /  typeQualifier  /  functionSpecifier)* typedefName (storageClassSpecifier  /  typeQualifier  /  functionSpecifier)*  /  (storageClassSpecifier  /  typeSpecifier  /  typeQualifier  /  functionSpecifier)+
abstractDeclarator <-  pointer? directAbstractDeclarator  /  pointer
directAbstractDeclarator <-  (LPAR abstractDeclarator RPAR  /  LBRK (assignmentExpression  /  STAR)? RBRK  /  LPAR parameterTypeList? RPAR) (LBRK (assignmentExpression  /  STAR)? RBRK  /  LPAR parameterTypeList? RPAR)*
storageClassSpecifier <-  TYPEDEF  /  EXTERN  /  STATIC  /  AUTO  /  REGISTER  /  ATTRIBUTE LPAR^Err_066 LPAR^Err_067 (!RPAR .  /  !(RPAR  /  !.) %{Err_068} .)* RPAR^Err_069 RPAR^Err_070
functionSpecifier <-  INLINE  /  STDCALL
primaryExpression <-  Identifier  /  Constant  /  StringLiteral  /  LPAR expression RPAR
expression      <-  assignmentExpression (COMMA assignmentExpression)*
assignmentExpression <-  unaryExpression assignmentOperator assignmentExpression  /  conditionalExpression
assignmentOperator <-  EQU  /  STAREQU  /  DIVEQU  /  MODEQU  /  PLUSEQU  /  MINUSEQU  /  LEFTEQU  /  RIGHTEQU  /  ANDEQU  /  HATEQU  /  OREQU
StringLiteral   <-  '"' (!'"' .)* '"' Spacing
Spacing         <-  (LongComment  /  LineComment  /  Pragma  /  %nl)*
COMMENT         <-  LongComment  /  LineComment
LongComment     <-  "/*" (!"*/" .)* "*/"
LineComment     <-  "//" (!%nl .)*
Pragma          <-  "#" (!%nl .)*
Identifier      <-  !Keyword IdNondigit IdChar* Spacing
IdChar          <-  [a-z]  /  [A-Z]  /  [0-9]
IdNondigit      <-  [a-z]  /  [A-Z]
Constant        <-  FloatConstant  /  IntegerConstant  /  EnumerationConstant  /  CharacterConstant
FloatConstant   <-  DecimalFloatConstant Spacing
DecimalFloatConstant <-  Fraction  /  [0-9]+
Fraction        <-  [0-9]* "." [0-9]+  /  [0-9]+ "."
IntegerConstant <-  DecimalConstant Spacing
DecimalConstant <-  [1-9] [0-9]*
EnumerationConstant <-  Identifier
CharacterConstant <-  "L"? "'" Char* "'" Spacing
Char            <-  [a-z]  /  [A-Z]  /  !"'" .
Keyword         <-  ("auto"  /  "break"  /  "case"  /  "char"  /  "const"  /  "continue"  /  "default"  /  "double"  /  "do"  /  "else"  /  "enum"  /  "extern"  /  "float"  /  "for"  /  "goto"  /  "if"  /  "int"  /  "inline"  /  "long"  /  "register"  /  "restrict"  /  "return"  /  "short"  /  "signed"  /  "sizeof"  /  "static"  /  "struct"  /  "switch"  /  "typedef"  /  "union"  /  "unsigned"  /  "void"  /  "volatile"  /  "while"  /  "_Bool"  /  "_Complex"  /  "_Imaginary"  /  "_stdcall"  /  "__declspec"  /  "__attribute__") !IdChar
AUTO            <-  "auto" !IdChar Spacing
BREAK           <-  "break" !IdChar Spacing
CASE            <-  "case" !IdChar Spacing
CHAR            <-  "char" !IdChar Spacing
CONST           <-  "const" !IdChar Spacing
CONTINUE        <-  "continue" !IdChar Spacing
DEFAULT         <-  "default" !IdChar Spacing
DOUBLE          <-  "double" !IdChar Spacing
DO              <-  "do" !IdChar Spacing
ELSE            <-  "else" !IdChar Spacing
ENUM            <-  "enum" !IdChar Spacing
EXTERN          <-  "extern" !IdChar Spacing
FLOAT           <-  "float" !IdChar Spacing
FOR             <-  "for" !IdChar Spacing
GOTO            <-  "goto" !IdChar Spacing
IF              <-  "if" !IdChar Spacing
INT             <-  "int" !IdChar Spacing
INLINE          <-  "inline" !IdChar Spacing
LONG            <-  "long" !IdChar Spacing
REGISTER        <-  "register" !IdChar Spacing
RESTRICT        <-  "restrict" !IdChar Spacing
RETURN          <-  "return" !IdChar Spacing
SHORT           <-  "short" !IdChar Spacing
SIGNED          <-  "signed" !IdChar Spacing
SIZEOF          <-  "sizeof" !IdChar Spacing
STATIC          <-  "static" !IdChar Spacing
STRUCT          <-  "struct" !IdChar Spacing
SWITCH          <-  "switch" !IdChar Spacing
TYPEDEF         <-  "typedef" !IdChar Spacing
UNION           <-  "union" !IdChar Spacing
UNSIGNED        <-  "unsigned" !IdChar Spacing
VOID            <-  "void" !IdChar Spacing
VOLATILE        <-  "volatile" !IdChar Spacing
WHILE           <-  "while" !IdChar Spacing
BOOL            <-  "_Bool" !IdChar Spacing
COMPLEX         <-  "_Complex" !IdChar Spacing
STDCALL         <-  "_stdcall" !IdChar Spacing
DECLSPEC        <-  "__declspec" !IdChar Spacing
ATTRIBUTE       <-  "__attribute__" !IdChar Spacing
LBRK            <-  "[" Spacing
RBRK            <-  "]" Spacing
LPAR            <-  "(" Spacing
RPAR            <-  ")" Spacing
LWING           <-  "{" Spacing
RWING           <-  "}" Spacing
DOT             <-  "." Spacing
PTR             <-  "->" Spacing
INC             <-  "++" Spacing
DEC             <-  "--" Spacing
AND             <-  "&" !"&" Spacing
STAR            <-  "*" !"=" Spacing
PLUS            <-  "+" !"+=" Spacing
MINUS           <-  "-" !"\-=>" Spacing
TILDA           <-  "~" Spacing
BANG            <-  "!" !"=" Spacing
DIV             <-  "/" !"=" Spacing
MOD             <-  "%" !"=>" Spacing
LEFT            <-  "<<" !"=" Spacing
RIGHT           <-  ">>" !"=" Spacing
LT              <-  "<" !"=" Spacing
GT              <-  ">" !"=" Spacing
LE              <-  "<=" Spacing
GE              <-  ">=" Spacing
EQUEQU          <-  "==" Spacing
BANGEQU         <-  "!=" Spacing
HAT             <-  "^" !"=" Spacing
OR              <-  "|" ![=|] Spacing
ANDAND          <-  "&&" Spacing
OROR            <-  "||" Spacing
QUERY           <-  "?" Spacing
COLON           <-  ":" !">" Spacing
SEMI            <-  ";" Spacing
ELLIPSIS        <-  "..." Spacing
EQU             <-  "=" !"=" Spacing
STAREQU         <-  "*=" Spacing
DIVEQU          <-  "/=" Spacing
MODEQU          <-  "%=" Spacing
PLUSEQU         <-  "+=" Spacing
MINUSEQU        <-  "-=" Spacing
LEFTEQU         <-  "<<=" Spacing
RIGHTEQU        <-  ">>=" Spacing
ANDEQU          <-  "&=" Spacing
HATEQU          <-  "^=" Spacing
OREQU           <-  "|=" Spacing
COMMA           <-  "," Spacing
EOT             <-  !.
SPACE           <-  [ 	
]  /  COMMENT
SKIP            <-  ([ 	
]  /  COMMENT)*
]]

local g = m.match(g)
local p = coder.makeg(g, 'ast')

local dir = util.getPath(arg[0])

util.testYes(dir .. '/yes/', 'c', p)
util.testYes(dir .. '/yes2/', 'c', p)
util.testNo(dir .. '/no/', 'c', p)
util.testNo(dir .. '/no-strict/', 'c', p)
