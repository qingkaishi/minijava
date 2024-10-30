/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr, Sam Harwell
 Copyright (c) 2017 Ivan Kochurkin (upgrade to Java 8)
 Copyright (c) 2021 Michał Lorek (upgrade to Java 11)
 Copyright (c) 2022 Michał Lorek (upgrade to Java 17)
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

parser grammar MiniJavaParser;

options {
    tokenVocab = MiniJavaLexer;
}

compilationUnit
    : (classDeclaration | methodDeclaration | ';')* EOF
    ;

classDeclaration
    : CLASS identifier (EXTENDS typeType)? classBody
    ;

classBody
    : '{' classBodyDeclaration* '}'
    ;

classBodyDeclaration
    : ';'
    | methodDeclaration
    | fieldDeclaration
    | constructorDeclaration
    ;

methodDeclaration
    : (typeType | VOID) identifier formalParameters methodBody = block
    ;

constructorDeclaration
    : identifier formalParameters constructorBody = block
    ;

fieldDeclaration
    : typeType variableDeclarator ';'
    ;

variableDeclarator
    : identifier ('=' variableInitializer)?
    ;

variableInitializer
    : arrayInitializer
    | expression
    ;

arrayInitializer
    : '{' (variableInitializer (',' variableInitializer)* ','?)? '}'
    ;

formalParameters
    : '(' formalParameterList? ')'
    ;

formalParameterList
    : formalParameter (',' formalParameter)*
    ;

formalParameter
    : typeType identifier
    ;

literal
    : DECIMAL_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOL_LITERAL
    | NULL_LITERAL
    ;

block
    : '{' blockStatement* '}'
    ;

blockStatement
    : localVariableDeclaration ';'
    | statement
    ;

localVariableDeclaration
    : VAR identifier '=' expression
    | typeType variableDeclarator
    ;

identifier
    : IDENTIFIER
    | MODULE
    | OPEN
    | REQUIRES
    | EXPORTS
    | OPENS
    | TO
    | USES
    | PROVIDES
    | WITH
    | TRANSITIVE
    | YIELD
    | SEALED
    | PERMITS
    | RECORD
    | VAR
    ;

typeIdentifier // Identifiers that are not restricted for type declarations
    : IDENTIFIER
    | MODULE
    | OPEN
    | REQUIRES
    | EXPORTS
    | OPENS
    | TO
    | USES
    | PROVIDES
    | WITH
    | TRANSITIVE
    | SEALED
    | PERMITS
    | RECORD
    ;

statement
    : block
    | IF parExpression statement (ELSE statement)?
    | FOR '(' forControl ')' statement
    | WHILE parExpression statement
    | RETURN expression? ';'
    | BREAK ';'
    | CONTINUE ';'
    | SEMI
    | statementExpression = expression ';'
    ;

parExpression
    : '(' expression ')'
    ;

forControl
    : forInit? ';' expression? ';' forUpdate = expressionList?
    ;

forInit
    : localVariableDeclaration
    | expressionList
    ;

expressionList
    : expression (',' expression)*
    ;

expression
    // Expression order in accordance with https://introcs.cs.princeton.edu/java/11precedence/
    // Level 16, Primary, array and member access
    : primary
    | expression '[' expression ']'
    | expression bop = '.' (
        identifier
        | methodCall
    )
    // Method calls and method references are part of primary, and hence level 16 precedence
    | methodCall

    // Level 15 Post-increment/decrement operators
    | expression postfix = ('++' | '--')

    // Level 14, Unary operators
    | prefix = ('+' | '-' | '++' | '--' | '~' | 'not') expression

    // Level 13 Cast and object creation
    | '(' typeType ')' expression
    | NEW creator

    // Level 12 to 1, Remaining operators
    | expression bop = ('*' | '/' | '%') expression           // Level 12, Multiplicative operators
    | expression bop = ('+' | '-') expression                 // Level 11, Additive operators
    | expression ('<' '<' | '>' '>' '>' | '>' '>') expression // Level 10, Shift operators
    | expression bop = ('<=' | '>=' | '>' | '<') expression   // Level 9, Relational operators
    | expression bop = INSTANCEOF (typeType)
    | expression bop = ('==' | '!=') expression                      // Level 8, Equality Operators
    | expression bop = '&' expression                                // Level 7, Bitwise AND
    | expression bop = '^' expression                                // Level 6, Bitwise XOR
    | expression bop = '|' expression                                // Level 5, Bitwise OR
    | expression bop = 'and' expression                               // Level 4, Logic AND
    | expression bop = 'or' expression                               // Level 3, Logic OR
    | <assoc = right> expression bop = '?' expression ':' expression // Level 2, Ternary
    // Level 1, Assignment
    | <assoc = right> expression bop = (
        '='
        | '+='
        | '-='
        | '*='
        | '/='
        | '&='
        | '|='
        | '^='
        | '>>='
        | '>>>='
        | '<<='
        | '%='
    ) expression
    ;

primary
    : '(' expression ')'
    | THIS
    | SUPER
    | literal
    | identifier
    ;

methodCall
    : (identifier | THIS | SUPER) arguments
    ;

creator
    : createdName classCreatorRest
    | createdName arrayCreatorRest
    ;

createdName
    : typeIdentifier
    | primitiveType
    ;

arrayCreatorRest
    : ('[' ']')+ arrayInitializer
    | ('[' expression ']')+ ('[' ']')*
    ;

classCreatorRest
    : arguments
    ;

typeType
    : (typeIdentifier | primitiveType) ('[' ']')*
    ;

primitiveType
    : BOOLEAN
    | CHAR
    | INT
    | STRING
    ;

arguments
    : '(' expressionList? ')'
    ;
