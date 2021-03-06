/*
 [The "BSD licence"]
 Copyright (c) 2013 Sam Harwell
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** C 2011 grammar built from the C11 Spec */
grammar C;

@lexer::members {
  public static final int WHITESPACE = 1;
  public static final int COMMENT = 2;
  public static final int CPP = 2;
}

@parser::header {
import java.util.*;
}

@parser::members {
boolean typeSpecifierCanBeTypedefName = true;
Set<String> typedefs = new HashSet<String>(); // only concerned with membership.
{
  typedefs.add("__builtin_va_list");
}
boolean isTypedefName() { return typedefs.contains(getCurrentToken().getText()); }
}

// have `constant` as a parser rule (not lexer rule) so that
// we can easily distinguish what kind of constant some token is.
constant
    :   IntegerConstant      # constInteger
    |   FloatingConstant     # constFloat
    |   CharacterConstant    # constChar
    ;

primaryExpression
    :   Identifier                                                 # primaryId
    |   constant                                                   # primaryConst
    |   StringLiteral+                                             # primaryString
    |   '(' expression ')'                                         # primaryParen
    |   genericSelection                                           # primaryGeneric
    |   '__extension__'? '(' compoundStatement ')' // Blocks (GCC extension)
                                                                   # primaryBlock
    |   '__builtin_va_arg' '(' unaryExpression ',' typeName ')'    # primaryVaArg
    |   '__builtin_offsetof' '(' typeName ',' unaryExpression ')'  # primaryOffset
    ;

genericSelection
    :   '_Generic' '(' assignmentExpression ',' genericAssocList ')'
    ;

genericAssocList
    :   genericAssociation
    |   genericAssocList ',' genericAssociation
    ;

genericAssociation
    :   typeName ':' assignmentExpression
    |   'default' ':' assignmentExpression
    ;

postfixExpression
    :   primaryExpression                                            # postfixFallthrough
    |   postfixExpression '[' expression ']'                         # postfixArray
    |   postfixExpression '(' argumentExpressionList? ')'            # postfixCall
    |   postfixExpression '.' Identifier                             # postfixStruct
    |   postfixExpression '->' Identifier                            # postfixPtrStruct
    |   postfixExpression '++'                                       # postfixIncr
    |   postfixExpression '--'                                       # postfixIncr
    |   '(' typeName ')' '{' initializerList '}'                     # postfixCompoundLiteral
    |   '(' typeName ')' '{' initializerList ',' '}'                 # postfixCompoundLiteral
    |   '__extension__' '(' typeName ')' '{' initializerList '}'     # postfixCompoundLiteral
    |   '__extension__' '(' typeName ')' '{' initializerList ',' '}' # postfixCompoundLiteral
    ;

argumentExpressionList
    :   assignmentExpression
    |   argumentExpressionList ',' assignmentExpression
    ;

unaryExpression
    :   postfixExpression             # unaryFallthrough
    |   '++' unaryExpression          # unaryIncr
    |   '--' unaryExpression          # unaryIncr
    |   unaryOperator castExpression  # unaryOpExpr
    |   'sizeof' unaryExpression      # unarySizeofExpr
    |   'sizeof' '(' typeName ')'     # unarySizeofType
    |   '_Alignof' '(' typeName ')'   # unaryAlignof
    // GCC extension address of label
    |   '&&' Identifier               # unaryGCCExtension
    ;

unaryOperator
    :   '&' | '*' | '+' | '-' | '~' | '!'
    ;

castExpression
    :   unaryExpression                                  # castFallthrough
    |   {typeSpecifierCanBeTypedefName = true;}
        '(' typeName ')' castExpression                  # castExpr
    |   '__extension__' '(' typeName ')' castExpression  # castExpr
    ;

multiplicativeExpression
    :   castExpression                                   # multFallthrough
    |   multiplicativeExpression '*' castExpression      # multExpr
    |   multiplicativeExpression '/' castExpression      # multExpr
    |   multiplicativeExpression '%' castExpression      # multExpr
    ;

additiveExpression
    :   multiplicativeExpression                         # addFallthrough
    |   additiveExpression '+' multiplicativeExpression  # addExpr
    |   additiveExpression '-' multiplicativeExpression  # addExpr
    ;

shiftExpression
    :   additiveExpression                               # shiftFallthrough
    |   shiftExpression '<<' additiveExpression          # shiftExpr
    |   shiftExpression '>>' additiveExpression          # shiftExpr
    ;

relationalExpression
    :   shiftExpression                                  # relFallthrough
    |   relationalExpression '<' shiftExpression         # relExpr
    |   relationalExpression '>' shiftExpression         # relExpr
    |   relationalExpression '<=' shiftExpression        # relExpr
    |   relationalExpression '>=' shiftExpression        # relExpr
    ;

equalityExpression
    :   relationalExpression                             # eqFallthrough
    |   equalityExpression '==' relationalExpression     # eqExpr
    |   equalityExpression '!=' relationalExpression     # eqExpr
    ;

andExpression
    :   equalityExpression                               # andFallthrough
    |   andExpression '&' equalityExpression             # andExpr
    ;

exclusiveOrExpression
    :   andExpression                                    # xorFallthrough
    |   exclusiveOrExpression '^' andExpression          # xorExpr
    ;

inclusiveOrExpression
    :   exclusiveOrExpression                            # orFallthrough
    |   inclusiveOrExpression '|' exclusiveOrExpression  # orExpr
    ;

logicalAndExpression
    :   inclusiveOrExpression                            # logAndFallthrough
    |   logicalAndExpression '&&' inclusiveOrExpression  # logAndExpr
    ;

logicalOrExpression
    :   logicalAndExpression                             # logOrFallthrough
    |   logicalOrExpression '||' logicalAndExpression    # logOrExpr
    ;

conditionalExpression
    :   logicalOrExpression ('?' expression ':' conditionalExpression)?
    ;

assignmentExpression
    :   conditionalExpression                                   # assgFallthrough
    |   unaryExpression assignmentOperator assignmentExpression # assgExpr
    ;

assignmentOperator
    :   '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
    ;

expression
    :   assignmentExpression                              # exprFallthrough
    |   expression ',' assignmentExpression               # commaExpr
    ;

constantExpression
    :   conditionalExpression
    ;

declaration
locals [boolean isTypedef = false,
        boolean hasTagOrEnumMembers = false]
    :   declarationSpecifiers maybeInitDeclaratorList ';' {typeSpecifierCanBeTypedefName = true;}
    |   staticAssertDeclaration
    ;

// Use this, because I'm not sure we could make use of
// the semantic predicate {}? otherwise.
maybeInitDeclaratorList
  : {!$declaration::hasTagOrEnumMembers}? initDeclaratorList
  | initDeclaratorList?
  ;

// C99 S6.7(2) says that:
//  "A declaration shall declare at least a declarator (other than the
//  parameters of a function or the members of a structure or union), a tag, or
//  the members of an enumeration."
// This helps us disambiguate between typedefName and Identifier of declarator.
declarationSpecifiers
    :   declarationSpecifier+
    ;

declarationSpecifiers2
    :   declarationSpecifier+
    ;

declarationSpecifier
    :   storageClassSpecifier
    |   typeSpecifier
    |   typeQualifier
    |   functionSpecifier
    |   alignmentSpecifier
    ;

initDeclaratorList
    :   initDeclarator
    |   initDeclaratorList ',' initDeclarator
    ;

initDeclarator
    :   declarator
    |   declarator '=' initializer
    ;

storageClassSpecifier
    :   'typedef' {
        $declaration::isTypedef = true;
    }
    |   'extern'
    |   'static'
    |   '_Thread_local'
    |   'auto'
    |   'register'
    ;

typeSpecifier
    :   ('void'
    |   'char'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    |   'signed'
    |   'unsigned'
    |   '_Bool'
    |   '_Complex'
    |   '__m128'
    |   '__m128d'
    |   '__m128i')             { typeSpecifierCanBeTypedefName = false; } # typeSpecifierPrimitive
    |   '__extension__' '(' ('__m128' | '__m128d' | '__m128i') ')'
                               { typeSpecifierCanBeTypedefName = false; } # typeSpecifierExtension
    |   atomicTypeSpecifier    { typeSpecifierCanBeTypedefName = false; } # typeSpecifierAtomic
    |   structOrUnionSpecifier { typeSpecifierCanBeTypedefName = false; } # typeSpecifierStructOrUnion
    |   enumSpecifier          { typeSpecifierCanBeTypedefName = false; } # typeSpecifierEnum
    |   {isTypedefName() && typeSpecifierCanBeTypedefName}? typedefName
                               { typeSpecifierCanBeTypedefName = false; } # typeSpecifierTypedef
    |   '__typeof__' '(' constantExpression ')' /* GCC extension */
                               { typeSpecifierCanBeTypedefName = false; } # typeSpecifierTypeof
    ;

structOrUnionSpecifier
    :   structOrUnion Identifier? '{' structDeclarationList '}' {
        try {
            // TODO: By right, this is only true if identifier isn't null.
            $declaration::hasTagOrEnumMembers = true;
        } catch (NullPointerException ex) {
            // Silently ignore this
        }
    }
    |   structOrUnion Identifier {
        try {
            $declaration::hasTagOrEnumMembers = true;
        } catch (NullPointerException ex) {
            // Silently ignore this
        }
    }
    ;

structOrUnion
    :   'struct'
    |   'union'
    ;

structDeclarationList
    :   structDeclaration
    |   structDeclarationList structDeclaration
    ;

structDeclaration
    :   {typeSpecifierCanBeTypedefName = true;} specifierQualifierList structDeclaratorList? ';'
    |   staticAssertDeclaration
    ;

specifierQualifierList
    :   typeSpecifier specifierQualifierList?
    |   typeQualifier specifierQualifierList?
    ;

structDeclaratorList
    :   structDeclarator
    |   structDeclaratorList ',' structDeclarator
    ;

structDeclarator
    :   declarator
    |   declarator? ':' constantExpression
    ;

enumSpecifier
    :   'enum' Identifier? '{' enumeratorList ','? '}' {
        try {
            $declaration::hasTagOrEnumMembers = true;
        } catch (NullPointerException ex) {
            // Silently ignore this
        }
    }
    |   'enum' Identifier
    ;

enumeratorList
    :   enumerator
    |   enumeratorList ',' enumerator
    ;

enumerator
    :   enumerationConstant
    |   enumerationConstant '=' constantExpression
    ;

enumerationConstant
    :   Identifier
    ;

atomicTypeSpecifier
    :   '_Atomic' '(' typeName ')'
    ;

typeQualifier
    :   'const'
    |   'restrict'
    |   'volatile'
    |   '_Atomic'
    |   '__restrict'
    ;

functionSpecifier
    :   ('inline'
    |   '_Noreturn'
    |   '__inline__' // GCC extension
    |   '__stdcall')
    |   gccAttributeSpecifier
    |   '__declspec' '(' Identifier ')'
    ;

alignmentSpecifier
    :   '_Alignas' '(' typeName ')'
    |   '_Alignas' '(' constantExpression ')'
    ;

declarator
    :   pointer? directDeclarator gccDeclaratorExtension*
    ;

// Apologies if the labels are misnomers.
directDeclarator
    :   Identifier {
        try {
            if ($declaration::isTypedef) {
                typedefs.add($text);
            }
        } catch (NullPointerException ex) {
            // Silently ignore this
        }
    }                                                                             # declaredIdentifier
    |   '(' declarator ')'                                                        # declaredParentheses
    |   directDeclarator '[' typeQualifierList? assignmentExpression? ']'         # declaredArray
    |   directDeclarator '[' 'static' typeQualifierList? assignmentExpression ']' # declaredArray
    |   directDeclarator '[' typeQualifierList 'static' assignmentExpression ']'  # declaredArray
    |   directDeclarator '[' typeQualifierList? '*' ']'                           # declaredArray
    |   directDeclarator '(' parameterTypeList ')'                                # declaredFunctionPrototype
    |   directDeclarator '(' identifierList? ')'                                  # declaredFunctionDefinition
    ;

gccDeclaratorExtension
    :   ('__asm' | '__asm__') '(' StringLiteral+ ')'
    |   gccAttributeSpecifier
    ;

gccAttributeSpecifier
    :   '__attribute__' '(' '(' gccAttributeList ')' ')'
    ;

gccAttributeList
    :   gccAttribute (',' gccAttribute)*
    |   // empty
    ;

gccAttribute
    :   ~(',' | '(' | ')') // relaxed def for "identifier or reserved word"
        ('(' argumentExpressionList? ')')?
    |   // empty
    ;

nestedParenthesesBlock
    :   (   ~('(' | ')')
        |   '(' nestedParenthesesBlock ')'
        )*
    ;

pointer
    :   '*' typeQualifierList?
    |   '*' typeQualifierList? pointer
    |   '^' typeQualifierList? // Blocks language extension
    |   '^' typeQualifierList? pointer // Blocks language extension
    ;

typeQualifierList
    :   typeQualifier
    |   typeQualifierList typeQualifier
    ;

parameterTypeList
    :   {typeSpecifierCanBeTypedefName = true;} parameterList
    |   {typeSpecifierCanBeTypedefName = true;} parameterList ',' '...'
    ;

parameterList
    :   parameterDeclaration
    |   parameterList ',' parameterDeclaration
    ;

parameterDeclaration
    :   declarationSpecifiers declarator {typeSpecifierCanBeTypedefName = true;}
    |   declarationSpecifiers2 abstractDeclarator? {typeSpecifierCanBeTypedefName = true;}
    ;

identifierList
    :   Identifier
    |   identifierList ',' Identifier
    ;

typeName
    :   specifierQualifierList abstractDeclarator?
    ;

abstractDeclarator
    :   pointer
    |   pointer? directAbstractDeclarator gccDeclaratorExtension*
    ;

// Apologies if the labels are misnomers.
// Wish the labs could be more concise, though
directAbstractDeclarator
    :   '(' abstractDeclarator ')' gccDeclaratorExtension*                                # abstractDeclaredParentheses
    |   '[' typeQualifierList? assignmentExpression? ']'                                  # abstractDeclaredArray
    |   '[' 'static' typeQualifierList? assignmentExpression ']'                          # abstractDeclaredArray
    |   '[' typeQualifierList 'static' assignmentExpression ']'                           # abstractDeclaredArray
    |   '[' '*' ']'                                                                       # abstractDeclaredArray
    |   '(' parameterTypeList? ')' gccDeclaratorExtension*                                # abstractDeclaredFunctionPrototype
    |   directAbstractDeclarator '[' typeQualifierList? assignmentExpression? ']'         # abstractDeclaredArray
    |   directAbstractDeclarator '[' 'static' typeQualifierList? assignmentExpression ']' # abstractDeclaredArray
    |   directAbstractDeclarator '[' typeQualifierList 'static' assignmentExpression ']'  # abstractDeclaredArray
    |   directAbstractDeclarator '[' '*' ']'                                              # abstractDeclaredArray
    |   directAbstractDeclarator '(' parameterTypeList? ')' gccDeclaratorExtension*       # abstractDeclaredFunctionPrototype
    ;

typedefName
    :   Identifier
    ;

initializer
    :   assignmentExpression           # initializerAssgExpr
    |   '{' initializerList '}'        # initializerInitList
    |   '{' initializerList ',' '}'    # initializerInitList
    ;

initializerList
    :   designation? initializer
    |   initializerList ',' designation? initializer
    ;

designation
    :   designatorList '='
    ;

designatorList
    :   designator
    |   designatorList designator
    ;

designator
    :   '[' constantExpression ']'     # designatorIndex
    |   '.' Identifier                 # designatorMember
    ;

staticAssertDeclaration
    :   '_Static_assert' '(' constantExpression ',' StringLiteral+ ')' ';'
    ;

statement
    :   labeledStatement
    |   compoundStatement
    |   expressionStatement
    |   selectionStatement
    |   iterationStatement
    |   jumpStatement
    |   ('__asm' | '__asm__') ('volatile' | '__volatile__') '(' (logicalOrExpression (',' logicalOrExpression)*)? (':' (logicalOrExpression (',' logicalOrExpression)*)?)* ')' ';'
    ;

labeledStatement
    :   Identifier ':' statement
    |   'case' constantExpression ':' statement
    |   'default' ':' statement
    ;

compoundStatement
    :   '{' blockItemList? '}'
    ;

blockItemList
    :   blockItem
    |   blockItemList blockItem
    ;

// BlockItem's rule alternatives switched, because `x;` (expression stmt)
// would be parsed as a declaration.
// S*x; should be declaration, *not* expression.
blockItem
    :   declaration
    |   statement
    ;

expressionStatement
    :   expression? ';'
    ;

selectionStatement
    :   'if' '(' expression ')' statement ('else' statement)?
    |   'switch' '(' expression ')' statement
    ;

iterationStatement
    :   'while' '(' expression ')' statement
    |   'do' statement 'while' '(' expression ')' ';'
    |   'for' '(' expression? ';' expression? ';' expression? ')' statement
    |   'for' '(' declaration expression? ';' expression? ')' statement
    ;

jumpStatement
    :   'goto' Identifier ';'
    |   'continue' ';'
    |   'break' ';'
    |   'return' expression? ';'
    |   'goto' unaryExpression ';' // GCC extension
    ;

compilationUnit
    :   translationUnit? EOF
    ;

// For testing type inference, it's convenient to have a
// translationUnit + expression to type infer on.
typeInferenceFixture
    :   translationUnit? expression EOF
    ;

translationUnit
    :   externalDeclaration
    |   translationUnit externalDeclaration
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    |   ';' // stray ;
    ;

functionDefinition
    :   declarationSpecifiers? declarator declarationList? compoundStatement
    ;

declarationList
    :   declaration
    |   declarationList declaration
    ;

Auto : 'auto';
Break : 'break';
Case : 'case';
Char : 'char';
Const : 'const';
Continue : 'continue';
Default : 'default';
Do : 'do';
Double : 'double';
Else : 'else';
Enum : 'enum';
Extern : 'extern';
Float : 'float';
For : 'for';
Goto : 'goto';
If : 'if';
Inline : 'inline';
Int : 'int';
Long : 'long';
Register : 'register';
Restrict : 'restrict';
Return : 'return';
Short : 'short';
Signed : 'signed';
Sizeof : 'sizeof';
Static : 'static';
Struct : 'struct';
Switch : 'switch';
Typedef : 'typedef';
Union : 'union';
Unsigned : 'unsigned';
Void : 'void';
Volatile : 'volatile';
While : 'while';

Alignas : '_Alignas';
Alignof : '_Alignof';
Atomic : '_Atomic';
Bool : '_Bool';
Complex : '_Complex';
Generic : '_Generic';
Imaginary : '_Imaginary';
Noreturn : '_Noreturn';
StaticAssert : '_Static_assert';
ThreadLocal : '_Thread_local';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Less : '<';
LessEqual : '<=';
Greater : '>';
GreaterEqual : '>=';
LeftShift : '<<';
RightShift : '>>';

Plus : '+';
PlusPlus : '++';
Minus : '-';
MinusMinus : '--';
Star : '*';
Div : '/';
Mod : '%';

And : '&';
Or : '|';
AndAnd : '&&';
OrOr : '||';
Caret : '^';
Not : '!';
Tilde : '~';

Question : '?';
Colon : ':';
Semi : ';';
Comma : ',';

Assign : '=';
// '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
StarAssign : '*=';
DivAssign : '/=';
ModAssign : '%=';
PlusAssign : '+=';
MinusAssign : '-=';
LeftShiftAssign : '<<=';
RightShiftAssign : '>>=';
AndAssign : '&=';
XorAssign : '^=';
OrAssign : '|=';

Equal : '==';
NotEqual : '!=';

Arrow : '->';
Dot : '.';
Ellipsis : '...';

Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    |   UniversalCharacterName
    //|   // other implementation-defined characters...
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
UniversalCharacterName
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

IntegerConstant
    :   DecimalConstant IntegerSuffix?
    |   OctalConstant IntegerSuffix?
    |   HexadecimalConstant IntegerSuffix?
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

fragment
DecimalFloatingConstant
    :   FractionalConstant ExponentPart? FloatingSuffix?
    |   DigitSequence ExponentPart FloatingSuffix?
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix HexadecimalFractionalConstant BinaryExponentPart FloatingSuffix?
    |   HexadecimalPrefix HexadecimalDigitSequence BinaryExponentPart FloatingSuffix?
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;

fragment
ExponentPart
    :   'e' Sign? DigitSequence
    |   'E' Sign? DigitSequence
    ;

fragment
Sign
    :   '+' | '-'
    ;

fragment
DigitSequence
    :   Digit+
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
BinaryExponentPart
    :   'p' Sign? DigitSequence
    |   'P' Sign? DigitSequence
    ;

fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
FloatingSuffix
    :   'f' | 'l' | 'F' | 'L'
    ;

CharacterConstant
    :   '\'' CCharSequence '\''
    |   'L\'' CCharSequence '\''
    |   'u\'' CCharSequence '\''
    |   'U\'' CCharSequence '\''
    ;

fragment
CCharSequence
    :   CChar+
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UniversalCharacterName
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\]
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' OctalDigit OctalDigit OctalDigit
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;

StringLiteral
    :   EncodingPrefix? '"' SCharSequence? '"'
    ;

fragment
EncodingPrefix
    :   'u8'
    |   'u'
    |   'U'
    |   'L'
    ;

fragment
SCharSequence
    :   SChar+
    ;

fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    ;

PreprocessorDirective
    : '#' ~[\r\n]*
      -> channel(CPP)
    ;

LineDirective
    :   '#' Whitespace? DecimalConstant Whitespace? StringLiteral ~[\r\n]*
        -> skip
    ;

PragmaDirective
    :   '#' Whitespace? 'pragma' Whitespace ~[\r\n]*
        -> skip
    ;

Whitespace
    :   [ \t]+
        -> channel(WHITESPACE)
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> channel(WHITESPACE)
    ;

BlockComment
    :   '/*' .*? '*/'
        -> channel(COMMENT)
    ;

LineComment
    :   '//' ~[\r\n]*
        -> channel(COMMENT)
    ;
