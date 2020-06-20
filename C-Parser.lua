local m = require 'pegparser.parser'
local coder = require 'pegparser.coder'
local util = require'pegparser.util'

local parser = [[
    

-- EXTERNAL DEFINITIONS 
    
    translationUnit             <-      Spacing externalDeclaration+ EOT

    externalDeclaration         <-      functionDefinition / declaration 

    functionDefinition          <-      declarationSpecifiers declarator declarationList? compoundStatemet

    declarationList             <-      declaration+

    statement                    <-      labeledStatement / compoundStatemet / expressionStatement / selectionStatement /
                                        iterationStatement / jumpStatement

    labeledStatement            <-      Identifier COLON statement / 
                                        CASE constantExpression COLON statement /
                                        DEFAULT COLON statement

    compoundStatemet            <-      LWING ( declaration / statement )* RWING

    expressionStatement         <-      expression? SEMI

    selectionStatement          <-      IF LPAR expression RPAR statement ( ELSE statement)? /
                                        SWITCH LPAR expression RPAR statement

    iterationStatement          <-      WHILE LPAR expression RPAR statement /
                                        DO statement WHILE LPAR expression RPAR SEMI /
                                        FOR LPAR expression? SEMI expression? SEMI expression? RPAR statement /
                                        FOR LPAR declaration expression? SEMI expression? RPAR statement

    jumpStatement               <-      GOTO Identifier SEMI /
                                        CONTINUE SEMI /
                                        BREAK SEMI / 
                                        RETURN expression? SEMI                                                                                

    declaration                 <-      declarationSpecifiers initDeclaratorList? SEMI 

    initDeclaratorList          <-      initDeclarator ( COMMA initDeclarator )*

    initDeclarator              <-      declarator ( EQU initializer )?                                     

    constantExpression          <-      conditionalExpression

    conditionalExpression       <-      logicalORExpression (QUERY expression COLON logicalORExpression)*

    logicalORExpression         <-      logicalANDExpression (OROR logicalANDExpression)*

    logicalANDExpression        <-      inclusiveORExpression (ANDAND inclusiveORExpression)*

    inclusiveORExpression       <-      exclusiveORExpression (OR exclusiveORExpression)*

    exclusiveORExpression       <-      andExpression (HAT andExpression)*

    andExpression               <-      equalityExpression (AND equalityExpression)*

    equalityExpression          <-      relationalExpression ((EQUEQU / BANGEQU) relationalExpression)*

    relationalExpression        <-      shiftExpression ((LE / GE / LT / GT) shiftExpression)*

    shiftExpression             <-      additiveExpression ((LEFT / RIGHT) additiveExpression)*

    additiveExpression          <-      multiplicativeExpression ((PLUS / MINUS) multiplicativeExpression)*

    multiplicativeExpression    <-      castExpression ((STAR / DIV / MOD) castExpression)*

    castExpression              <-      (LPAR typeName RPAR)* unaryExpression

    unaryExpression             <-      postfixExpression /
                                        INC unaryExpression /
                                        DEC unaryExpression /
                                        unaryOperator castExpression /
                                        SIZEOF (unaryExpression / LPAR typeName RPAR )

    unaryOperator               <-      AND / STAR / PLUS / MINUS / TILDA / BANG                                        

    postfixExpression           <-      ( primaryExpression /
                                        LPAR typeName RPAR LWING initializerList COMMA? RWING )
                                        ( LBRK expression RBRK /
                                        LPAR argumentExpressionList? RPAR /
                                        DOT Identifier /
                                        PTR Identifier /
                                        INC /
                                        DEC )*

    typeName                    <-      specifierQualifierList abstractDeclarator?

    initializerList             <-      designation? initializer (COMMA designation? initializer)*

    designation                 <-      designator+ EQU

    designator                  <-      LBRK constantExpression RBRK / DOT Identifier

    initializer                 <-      assignmentExpression / LWING initializerList COMMA? RWING

    argumentExpressionList      <-      assignmentExpression (COMMA assignmentExpression)*

    specifierQualifierList      <-      ( typeQualifier* typedefName typeQualifier* ) / 
                                        ( typeSpecifier / typeQualifier )+

    typeQualifier               <-      CONST / RESTRICT / VOLATILE / DECLSPEC LPAR Identifier RPAR 

    typedefName                 <-      Identifier

    typeSpecifier               <-      VOID / CHAR / SHORT / INT / LONG / FLOAT / DOUBLE / SIGNED / UNSIGNED /
                                        BOOL / COMPLEX / structOrUnionSpecifier / enumSpecifier

    structOrUnionSpecifier      <-      structOrUnion ( Identifier? LWING structDeclaration+  RWING / Identifier ) 

    enumSpecifier               <-      ENUM ( Identifier? LWING enumeratorList COMMA? RWING / Identifier )

    enumeratorList              <-      enumerator (COMMA enumerator)*

    enumerator                  <-      EnumerationConstant (EQU constantExpression)?  

    structOrUnion               <-      STRUCT / UNION 

    structDeclaration           <-      specifierQualifierList structDeclaratorList SEMI

    structDeclaratorList        <-      structDeclarator (COMMA structDeclarator)*

    structDeclarator            <-      declarator? COLON constantExpression / declarator     

    declarator                  <-      pointer? directDeclarator

    pointer                     <-      ( STAR typeQualifier* )+

    directDeclarator            <-      ( Identifier / LPAR declarator RPAR) 
                                        ( LBRK typeQualifier* assignmentExpression? RBRK
                                        / LBRK STATIC typeQualifier* assignmentExpression RBRK
                                        / LBRK typeQualifier+ STATIC assignmentExpression RBRK
                                        / LBRK typeQualifier* STAR RBRK
                                        / LPAR parameterTypeList RPAR
                                        / LPAR identifierList? RPAR )* 

    parameterTypeList           <-      parameterList (COMMA ELLIPSIS)?  

    identifierList              <-      Identifier (COMMA Identifier)*  

    parameterList               <-      parameterDeclaration (COMMA parameterDeclaration)*

    parameterDeclaration        <-      declarationSpecifiers ( declarator / abstractDeclarator )? 

    declarationSpecifiers       <-      (( storageClassSpecifier / typeQualifier / functionSpecifier )* typedefName ( storageClassSpecifier / typeQualifier / functionSpecifier )*) /
                                        ( storageClassSpecifier / typeSpecifier / typeQualifier / functionSpecifier )+  

    abstractDeclarator          <-      pointer? directAbstractDeclarator / pointer
    
    directAbstractDeclarator    <-      ( LPAR abstractDeclarator RPAR /
                                        LBRK (assignmentExpression / STAR)? RBRK /
                                        LPAR parameterTypeList? RPAR )
                                        ( LBRK (assignmentExpression / STAR)? RBRK
                                        / LPAR parameterTypeList? RPAR )*                                         

    storageClassSpecifier       <-      TYPEDEF / EXTERN / STATIC / AUTO / REGISTER /
                                        ATTRIBUTE LPAR LPAR (!RPAR .)* RPAR RPAR     

    functionSpecifier           <-      INLINE / STDCALL                                                                                                          

    primaryExpression           <-      Identifier / Constant / StringLiteral / LPAR expression RPAR

    expression                  <-      assignmentExpression (COMMA assignmentExpression)*

    assignmentExpression        <-      unaryExpression assignmentOperator assignmentExpression / conditionalExpression

    assignmentOperator          <-      EQU / STAREQU / DIVEQU / MODEQU / PLUSEQU / MINUSEQU / LEFTEQU /
                                        RIGHTEQU / ANDEQU / HATEQU / OREQU

                                       

-- Lexical Elements 

    StringLiteral               <-      '"' (!'"' .)* '"' Spacing

    Spacing                     <-      ( LongComment / LineComment / Pragma / %nl)*

    COMMENT                     <-      LongComment / LineComment

    LongComment                 <-      "/*" (!"*/" .)* "*/"

    LineComment                 <-      "//" (!%nl .)* 

    Pragma                      <-      "#" (!%nl .)*

-- Identifiers 

    Identifier                  <-      !Keyword IdNondigit IdChar* Spacing

    IdChar                      <-      [a-z] / [A-Z] / [0-9]

    IdNondigit                  <-      [a-z] / [A-Z] 

-- Constants 

    Constant                    <-      FloatConstant / IntegerConstant / EnumerationConstant / CharacterConstant

    FloatConstant               <-      DecimalFloatConstant Spacing

    DecimalFloatConstant        <-      Fraction / [0-9]+ 

    Fraction                    <-      [0-9]* "." [0-9]+ / [0-9]+ "."  

    IntegerConstant             <-      DecimalConstant Spacing

    DecimalConstant             <-      [1-9][0-9]*

    EnumerationConstant         <-      Identifier

    CharacterConstant           <-      "L"? "'" Char* "'" Spacing

    Char                        <-      [a-z] / [A-Z] / !"'" .

--Keywords

    Keyword                     <-      ( "auto" / "break" / "case" / "char" / "const" / "continue" / "default" / "double" /
                                        "do" / "else" / "enum" / "extern" / "float" / "for" / "goto" / "if" / "int" /
                                        "inline" / "long" / "register" / "restrict" / "return" / "short" / "signed" /
                                        "sizeof" / "static" / "struct" / "switch" / "typedef" / "union" / "unsigned" /
                                        "void" / "volatile" / "while" / "_Bool" / "_Complex" / "_Imaginary" / "_stdcall" /
                                        "__declspec" / "__attribute__" )  !IdChar


    AUTO      <- "auto"             !IdChar Spacing 
    BREAK     <- "break"            !IdChar Spacing 
    CASE      <- "case"             !IdChar Spacing 
    CHAR      <- "char"             !IdChar Spacing
    CONST     <- "const"            !IdChar Spacing
    CONTINUE  <- "continue"         !IdChar Spacing
    DEFAULT   <- "default"          !IdChar Spacing
    DOUBLE    <- "double"           !IdChar Spacing
    DO        <- "do"               !IdChar Spacing
    ELSE      <- "else"             !IdChar Spacing
    ENUM      <- "enum"             !IdChar Spacing
    EXTERN    <- "extern"           !IdChar Spacing
    FLOAT     <- "float"            !IdChar Spacing
    FOR       <- "for"              !IdChar Spacing
    GOTO      <- "goto"             !IdChar Spacing
    IF        <- "if"               !IdChar Spacing
    INT       <- "int"              !IdChar Spacing
    INLINE    <- "inline"           !IdChar Spacing
    LONG      <- "long"             !IdChar Spacing
    REGISTER  <- "register"         !IdChar Spacing
    RESTRICT  <- "restrict"         !IdChar Spacing
    RETURN    <- "return"           !IdChar Spacing
    SHORT     <- "short"            !IdChar Spacing
    SIGNED    <- "signed"           !IdChar Spacing
    SIZEOF    <- "sizeof"           !IdChar Spacing
    STATIC    <- "static"           !IdChar Spacing
    STRUCT    <- "struct"           !IdChar Spacing
    SWITCH    <- "switch"           !IdChar Spacing
    TYPEDEF   <- "typedef"          !IdChar Spacing
    UNION     <- "union"            !IdChar Spacing
    UNSIGNED  <- "unsigned"         !IdChar Spacing
    VOID      <- "void"             !IdChar Spacing
    VOLATILE  <- "volatile"         !IdChar Spacing
    WHILE     <- "while"            !IdChar Spacing
    BOOL      <- "_Bool"            !IdChar Spacing
    COMPLEX   <- "_Complex"         !IdChar Spacing
    STDCALL   <- "_stdcall"         !IdChar Spacing
    DECLSPEC  <- "__declspec"       !IdChar Spacing
    ATTRIBUTE <- "__attribute__"    !IdChar Spacing

-- Punctuators

    LBRK       <-  "["              Spacing
    RBRK       <-  "]"              Spacing
    LPAR       <-  "("              Spacing
    RPAR       <-  ")"              Spacing
    LWING      <-  "{"              Spacing
    RWING      <-  "}"              Spacing
    DOT        <-  "."              Spacing
    PTR        <-  "->"             Spacing
    INC        <-  "++"             Spacing
    DEC        <-  "--"             Spacing
    AND        <-  "&"  !"&"        Spacing
    STAR       <-  "*"  !"="        Spacing
    PLUS       <-  "+"  !"+="       Spacing
    MINUS      <-  "-"  !"\-=>"     Spacing
    TILDA      <-  "~"              Spacing
    BANG       <-  "!"  !"="        Spacing
    DIV        <-  "/"  !"="        Spacing 
    MOD        <-  "%"  !"=>"       Spacing
    LEFT       <-  "<<" !"="        Spacing
    RIGHT      <-  ">>" !"="        Spacing
    LT         <-  "<"  !"="        Spacing
    GT         <-  ">"  !"="        Spacing
    LE         <-  "<="             Spacing
    GE         <-  ">="             Spacing
    EQUEQU     <-  "=="             Spacing
    BANGEQU    <-  "!="             Spacing
    HAT        <-  "^"  !"="        Spacing
    OR         <-  "|"  ![=|]       Spacing
    ANDAND     <-  "&&"             Spacing
    OROR       <-  "||"             Spacing
    QUERY      <-  "?"              Spacing
    COLON      <-  ":"  !">"        Spacing
    SEMI       <-  ";"              Spacing
    ELLIPSIS   <-  "..."            Spacing
    EQU        <-  "="  !"="        Spacing
    STAREQU    <-  "*="             Spacing
    DIVEQU     <-  "/="             Spacing
    MODEQU     <-  "%="             Spacing
    PLUSEQU    <-  "+="             Spacing
    MINUSEQU   <-  "-="             Spacing
    LEFTEQU    <-  "<<="            Spacing
    RIGHTEQU   <-  ">>="            Spacing
    ANDEQU     <-  "&="             Spacing
    HATEQU     <-  "^="             Spacing
    OREQU      <-  "|="             Spacing
    COMMA      <-  ","              Spacing

    EOT        <-  !.                                                                          

]]  


g, lab, pos = m.match(parser)
print(g, lab, pos)

--gerar o parser
local p = coder.makeg(g)        

local dir = lfs.currentdir() .. '/yes/'
util.testYes(dir, 'c', p)

dir = lfs.currentdir() .. '/yes2/'
util.testYes(dir, 'c', p)

dir = lfs.currentdir() .. '/no/'
util.testNo(dir, 'c', p)

dir = lfs.currentdir() .. '/no-strict/'
util.testNo(dir, 'c', p)
