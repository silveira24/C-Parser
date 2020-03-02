local m = require 'pegparser.parser'
local coder = require 'pegparser.coder'
local util = require'pegparser.util'

local parser = [[
    
    --parser                      <-      statement+ EOF^ErrEOF

-- EXTERNAL DEFINITIONS 
    
    translationUnit             <-      Spacing externalDeclaration+ EOT

    externalDeclaration         <-      functionDefinition / declaration 

    functionDefinition          <-      declarationSpecifiers declarator declarationList? compoundStatemet

    declarationList             <-      declaration+


-- STATEMENTS

    statement                   <-      labeledStatement / compoundStatemet / expressionStatement / selectionStatement /
                                        iterationStatement / jumpStatement

    labeledStatement            <-      Identifier COLON statement / 
                                        CASE constantExpression COLON statement /
                                        DEFAULT COLON statement

    compoundStatemet            <-      LWING ( declaration / statement )* RWING

    expressionStatement         <-      expression? SEMI

    selectionStatement          <-      IF LPAR^ErrIfLPar expression RPAR^ErrIfRPar statement ( ELSE statement)? /
                                        SWITCH LPAR^ErrSwitchLPar expression RPAR^ErrSwitchRPar statement

    iterationStatement          <-      WHILE LPAR^ErrWhileLPar expression RPAR^ErrWhileRPar statement /
                                        DO statement WHILE^ErrWhileLPar LPAR expression RPAR^ErrWhileRPar SEMI /
                                        FOR LPAR^ErrForLPar expression? SEMI expression? SEMI expression? RPAR^ErrForRPar statement /
                                        FOR LPAR^ErrForLPar declaration expression? SEMI expression? RPAR^ErrForRPar statement

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

    structOrUnionSpecifier      <-      structOrUnion ( Identifier? LWING structDeclaration+ RWING / Identifier ) 

    enumSpecifier               <-      ENUM ( Identifier? LWING enumeratorList COMMA? RWING / Identifier )

    enumeratorList              <-      enumerator (COMMA enumerator)*

    enumerator                  <-      EnumerationConstant (EQU constantExpression)?  

    structOrUnion               <-      STRUCT / UNION 

    structDeclaration           <-      specifierQualifierList structDeclaratorList SEMI

    structDeclaratorList        <-      structDeclarator (COMMA structDeclarator)*

    structDeclarator            <-      declarator? COLON constantExpression / declarator    

    declarator                  <-      pointer? directDeclarator

    pointer                     <-      ( STAR typeQualifier* )+

    directDeclarator            <-      ( Identifier / LPAR declarator RPAR )
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

    declarationSpecifiers       <-      (( storageClassSpecifier / typeQualifier / functionSpecifier )*
                                        typedefName ( storageClassSpecifier / typeQualifier / functionSpecifier )*) /
                                        ( storageClassSpecifier / typeSpecifier / typeQualifier /
                                        functionSpecifier )+  

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

    StringLiteral               <-      '"' (!'"' .)* '"' / "'" (!"'" .)* "'"

    EOF                         <-      !.

    Spacing                     <-      ( LongComment / LineComment / Pragma / %nl)*

    LongComment                 <-      "\*" (!"*\" .)* "*\"

    LineComment                 <-      "//" (!%nl .)* 

    Pragma                      <-      "#" (!%nl .)*

-- Identifiers 

    Identifier                  <-      !Keyword IdNondigit IdChar*

    IdChar                      <-      [a-z] / [A-Z] / [0-9]

    IdNondigit                  <-      [a-z] / [A-Z] 

-- Constants 

    Constant                    <-      FloatConstant / IntegerConstant / EnumerationConstant / CharacterConstant

    FloatConstant               <-      DecimalFloatConstant

    DecimalFloatConstant        <-      Fraction / [0-9]+ 

    Fraction                    <-      [0-9]* "." [0-9]+ / [0-9]+ "."  

    IntegerConstant             <-      DecimalConstant

    DecimalConstant             <-      [1-9][0-9]*

    EnumerationConstant         <-      Identifier

    CharacterConstant           <-      "L"? "'" Char* "'"

    Char                        <-      [a-z] / [A-Z]

--Keywords

    Keyword                     <-      ( "auto" / "break" / "case" / "char" / "const" / "continue" / "default" / "double" /
                                        "do" / "else" / "enum" / "extern" / "float" / "for" / "goto" / "if" / "int" /
                                        "inline" / "long" / "register" / "restrict" / "return" / "short" / "signed" /
                                        "sizeof" / "static" / "struct" / "switch" / "typedef" / "union" / "unsigned" /
                                        "void" / "volatile" / "while" / "_Bool" / "_Complex" / "_Imaginary" / "_stdcall" /
                                        "__declspec" / "__attribute__" )  !IdChar


    AUTO      <- "auto"     
    BREAK     <- "break"    
    CASE      <- "case"     
    CHAR      <- "char"     
    CONST     <- "const"    
    CONTINUE  <- "continue" 
    DEFAULT   <- "default"  
    DOUBLE    <- "double"   
    DO        <- "do"       
    ELSE      <- "else"     
    ENUM      <- "enum"     
    EXTERN    <- "extern"   
    FLOAT     <- "float"    
    FOR       <- "for"      
    GOTO      <- "goto"     
    IF        <- "if"       
    INT       <- "int"      
    INLINE    <- "inline"   
    LONG      <- "long"     
    REGISTER  <- "register" 
    RESTRICT  <- "restrict" 
    RETURN    <- "return"   
    SHORT     <- "short"    
    SIGNED    <- "signed"   
    SIZEOF    <- "sizeof"   
    STATIC    <- "static"   
    STRUCT    <- "struct"   
    SWITCH    <- "switch"   
    TYPEDEF   <- "typedef"  
    UNION     <- "union"    
    UNSIGNED  <- "unsigned" 
    VOID      <- "void"     
    VOLATILE  <- "volatile" 
    WHILE     <- "while"    
    BOOL      <- "_Bool"    
    COMPLEX   <- "_Complex" 
    STDCALL   <- "_stdcall" 
    DECLSPEC  <- "__declspec"
    ATTRIBUTE <- "__attribute__"

-- Punctuators

    LBRK       <-  "["         
    RBRK       <-  "]"         
    LPAR       <-  "("         
    RPAR       <-  ")"         
    LWING      <-  "{"         
    RWING      <-  "}"         
    DOT        <-  "."         
    PTR        <-  "->"        
    INC        <-  "++"        
    DEC        <-  "--"        
    AND        <-  "&"  !"&" 
    STAR       <-  "*"  !"="   
    PLUS       <-  "+"  !"+="  
    MINUS      <-  "-"  !"\-=>"
    TILDA      <-  "~"         
    BANG       <-  "!"  !"="   
    DIV        <-  "/"  !"="   
    MOD        <-  "%"  !"=>"  
    LEFT       <-  "<<" !"="   
    RIGHT      <-  ">>" !"="   
    LT         <-  "<"  !"="   
    GT         <-  ">"  !"="   
    LE         <-  "<="        
    GE         <-  ">="        
    EQUEQU     <-  "=="        
    BANGEQU    <-  "!="        
    HAT        <-  "^"  !"="   
    OR         <-  "|"  !"="   
    ANDAND     <-  "&&"        
    OROR       <-  "||"        
    QUERY      <-  "?"         
    COLON      <-  ":"  !">"   
    SEMI       <-  ";"         
    ELLIPSIS   <-  "..."       
    EQU        <-  "="  !"="   
    STAREQU    <-  "*="        
    DIVEQU     <-  "/="        
    MODEQU     <-  "%="        
    PLUSEQU    <-  "+="        
    MINUSEQU   <-  "-="        
    LEFTEQU    <-  "<<="       
    RIGHTEQU   <-  ">>="       
    ANDEQU     <-  "&="        
    HATEQU     <-  "^="        
    OREQU      <-  "|="        
    COMMA      <-  ","         

    EOT        <-  !.                                                                          

]]  


g, lab, pos = m.match(parser)
print(g, lab, pos)

--gerar o parser
local p = coder.makeg(g)        

local dir = lfs.currentdir() .. '/yes/'
util.testYes(dir, 'c', p)

dir = lfs.currentdir() .. '/no/'
util.testNo(dir, 'c', p)

print("\nStrict")
dir = lfs.currentdir() .. '/no-strict/'
util.testNo(dir, 'c', p, 'strict')
