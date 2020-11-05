grammar Graphql;
// import GraphqlSDL, GraphqlOperation, GraphqlCommon;

@header {
    package graphql.parser.antlr;
}

@lexer::members {
    public boolean isDigit(int c) {
        return c >= '0' && c <= '9';
    }
    public boolean isNameStart(int c) {
        return '_' == c ||
          (c >= 'A' && c <= 'Z') ||
          (c >= 'a' && c <= 'z');
    }
    public boolean isDot(int c) {
        return '.' == c;
    }
}


document : definition+;

definition:
operationDefinition |
fragmentDefinition |
typeSystemDefinition |
typeSystemExtension
;

//// GraphqlSDL
// grammar GraphqlSDL;
// import GraphqlCommon;

typeSystemDefinition:
schemaDefinition |
typeDefinition |
directiveDefinition
;

typeSystemExtension :
schemaExtension |
typeExtension
;

schemaDefinition : description? SCHEMA directives? '{' operationTypeDefinition+ '}';

schemaExtension :
    EXTEND SCHEMA directives? '{' operationTypeDefinition+ '}' |
    EXTEND SCHEMA directives+
;

operationTypeDefinition : description? operationType ':' typeName;

typeDefinition:
scalarTypeDefinition |
objectTypeDefinition |
interfaceTypeDefinition |
unionTypeDefinition |
enumTypeDefinition |
inputObjectTypeDefinition
;

//
// type extensions dont get "description" strings according to spec
// https://github.com/facebook/graphql/blob/master/spec/Appendix%20B%20--%20Grammar%20Summary.md
//

typeExtension :
    objectTypeExtensionDefinition |
    interfaceTypeExtensionDefinition |
    unionTypeExtensionDefinition |
    scalarTypeExtensionDefinition |
    enumTypeExtensionDefinition |
    inputObjectTypeExtensionDefinition
;

emptyParentheses : '{' '}';

scalarTypeDefinition : description? SCALAR name directives?;

scalarTypeExtensionDefinition : EXTEND SCALAR name directives;

objectTypeDefinition : description? TYPE name implementsInterfaces? directives? fieldsDefinition?;

objectTypeExtensionDefinition :
    EXTEND TYPE name implementsInterfaces? directives? extensionFieldsDefinition |
    EXTEND TYPE name implementsInterfaces? directives emptyParentheses? |
    EXTEND TYPE name implementsInterfaces
;

implementsInterfaces :
    IMPLEMENTS '&'? typeName+ |
    implementsInterfaces '&' typeName ;

fieldsDefinition : '{' fieldDefinition* '}';

extensionFieldsDefinition : '{' fieldDefinition+ '}';

fieldDefinition : description? name argumentsDefinition? ':' type directives?;

argumentsDefinition : '(' inputValueDefinition+ ')';

inputValueDefinition : description? name ':' type defaultValue? directives?;

interfaceTypeDefinition : description? INTERFACE name implementsInterfaces? directives? fieldsDefinition?;

interfaceTypeExtensionDefinition :
    EXTEND INTERFACE name implementsInterfaces? directives? extensionFieldsDefinition |
    EXTEND INTERFACE name implementsInterfaces? directives emptyParentheses? |
    EXTEND INTERFACE name implementsInterfaces
;


unionTypeDefinition : description? UNION name directives? unionMembership?;

unionTypeExtensionDefinition :
    EXTEND UNION name directives? unionMembership |
    EXTEND UNION name directives
;

unionMembership : '=' unionMembers;

unionMembers:
'|'? typeName |
unionMembers '|' typeName
;

enumTypeDefinition : description? ENUM name directives? enumValueDefinitions?;

enumTypeExtensionDefinition :
    EXTEND ENUM name directives? extensionEnumValueDefinitions |
    EXTEND ENUM name directives emptyParentheses?
;

enumValueDefinitions : '{' enumValueDefinition* '}';

extensionEnumValueDefinitions : '{' enumValueDefinition+ '}';

enumValueDefinition : description? enumValue directives?;


inputObjectTypeDefinition : description? INPUT name directives? inputObjectValueDefinitions?;

inputObjectTypeExtensionDefinition :
    EXTEND INPUT name directives? extensionInputObjectValueDefinitions |
    EXTEND INPUT name directives emptyParentheses?
;

inputObjectValueDefinitions : '{' inputValueDefinition* '}';

extensionInputObjectValueDefinitions : '{' inputValueDefinition+ '}';


directiveDefinition : description? DIRECTIVE '@' name argumentsDefinition? 'on' directiveLocations;

directiveLocation : name;

directiveLocations :
directiveLocation |
directiveLocations '|' directiveLocation
;

//// GraphqlOperation
// grammar GraphqlOperation;
// import GraphqlCommon;

operationDefinition:
selectionSet |
operationType  name? variableDefinitions? directives? selectionSet;

variableDefinitions : '(' variableDefinition+ ')';

variableDefinition : variable ':' type defaultValue? directives?;


selectionSet :  '{' selection+ '}';

selection :
field |
fragmentSpread |
inlineFragment;

field : alias? name arguments? directives? selectionSet?;

alias : name ':';



fragmentSpread : '...' fragmentName directives?;

inlineFragment : '...' typeCondition? directives? selectionSet;

fragmentDefinition : FRAGMENT fragmentName typeCondition directives? selectionSet;


typeCondition : ON_KEYWORD typeName;

////grammar GraphqlCommon;


operationType : SUBSCRIPTION | MUTATION | QUERY;

description : StringValue;

enumValue : enumValueName ;


arrayValue: '[' value* ']';

arrayValueWithVariable: '[' valueWithVariable* ']';



objectValue: '{' objectField* '}';
objectValueWithVariable: '{' objectFieldWithVariable* '}';
objectField : name ':' value;
objectFieldWithVariable : name ':' valueWithVariable;


directives : directive+;

directive :'@' name arguments?;


arguments : '(' argument+ ')';

argument : name ':' valueWithVariable;

baseName: NAME | FRAGMENT | QUERY | MUTATION | SUBSCRIPTION | SCHEMA | SCALAR | TYPE | INTERFACE | IMPLEMENTS | ENUM | UNION | INPUT | EXTEND | DIRECTIVE;
fragmentName: baseName | BooleanValue | NullValue;
enumValueName: baseName | ON_KEYWORD;

name: baseName | BooleanValue | NullValue | ON_KEYWORD;

value :
StringValue |
IntValue |
FloatValue |
BooleanValue |
NullValue |
enumValue |
arrayValue |
objectValue;


valueWithVariable :
variable |
StringValue |
IntValue |
FloatValue |
BooleanValue |
NullValue |
enumValue |
arrayValueWithVariable |
objectValueWithVariable;


variable : '$' name;

defaultValue : '=' value;

type : typeName | listType | nonNullType;

typeName : name;
listType : '[' type ']';
nonNullType: typeName '!' | listType '!';


BooleanValue: 'true' | 'false';

NullValue: 'null';

FRAGMENT: 'fragment';
QUERY: 'query';
MUTATION: 'mutation';
SUBSCRIPTION: 'subscription';
SCHEMA: 'schema';
SCALAR: 'scalar';
TYPE: 'type';
INTERFACE: 'interface';
IMPLEMENTS: 'implements';
ENUM: 'enum';
UNION: 'union';
INPUT: 'input';
EXTEND: 'extend';
DIRECTIVE: 'directive';
ON_KEYWORD: 'on';
NAME: [_A-Za-z][_0-9A-Za-z]*;



// Int Value
IntValue :  IntegerPart { !isDigit(_input.LA(1)) && !isDot(_input.LA(1)) && !isNameStart(_input.LA(1))  }?;
fragment IntegerPart : NegativeSign? '0' | NegativeSign? NonZeroDigit Digit*;
fragment NegativeSign : '-';
fragment NonZeroDigit: '1'..'9';

// Float Value
FloatValue : ((IntegerPart FractionalPart ExponentPart) { !isDigit(_input.LA(1)) && !isDot(_input.LA(1)) && !isNameStart(_input.LA(1))  }?) |
    ((IntegerPart FractionalPart ) { !isDigit(_input.LA(1)) && !isDot(_input.LA(1)) && !isNameStart(_input.LA(1))  }?) |
    ((IntegerPart ExponentPart) { !isDigit(_input.LA(1)) && !isDot(_input.LA(1)) && !isNameStart(_input.LA(1))  }?);
fragment FractionalPart: '.' Digit+;
fragment ExponentPart :  ExponentIndicator Sign? Digit+;
fragment ExponentIndicator: 'e' | 'E';
fragment Sign: '+'|'-';
fragment Digit : '0'..'9';

// StringValue
StringValue:
'""'  { _input.LA(1) != '"'}? |
'"' StringCharacter+ '"' |
'"""' BlockStringCharacter*? '"""';

fragment BlockStringCharacter:
'\\"""'|
ExtendedSourceCharacter;

fragment StringCharacter:
([\u0009\u0020\u0021] | [\u0023-\u005b] | [\u005d-\u{10FFFF}]) |  // this is SoureCharacter without '"' and '\'
'\\u' EscapedUnicode  |
'\\' EscapedCharacter;

fragment EscapedCharacter :  ["\\/bfnrt];
fragment EscapedUnicode : Hex Hex Hex Hex;
fragment Hex : [0-9a-fA-F];


// this is currently not covered by the spec because we allow all unicode chars
// u0009 = \t Horizontal tab
// u000a = \n line feed
// u000d = \r carriage return
// u0020 = space
fragment ExtendedSourceCharacter :[\u0009\u000A\u000D\u0020-\u{10FFFF}];
fragment ExtendedSourceCharacterWithoutLineFeed :[\u0009\u0020-\u{10FFFF}];

// this is the spec definition
// fragment SourceCharacter :[\u0009\u000A\u000D\u0020-\uFFFF];


Comment: '#' ExtendedSourceCharacterWithoutLineFeed* -> channel(2);

LF: [\n] -> channel(3);
CR: [\r] -> channel(3);
LineTerminator: [\u2028\u2029] -> channel(3);

Space : [\u0020] -> channel(3);
Tab : [\u0009] -> channel(3);
Comma : ',' -> channel(3);
UnicodeBOM : [\ufeff] -> channel(3);