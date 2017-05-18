package graphql_clj;

import clojure.lang.IObj;
import clojure.lang.IMeta;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;
import clojure.lang.PersistentVector;
import clojure.lang.Symbol;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;

public class Parser {
    private static final int TOKEN_EOF = -1;
    private static final int TOKEN_INTEGER = 1;
    private static final int TOKEN_FLOAT = 2;
    private static final int TOKEN_STRING = 3;
    private static final int TOKEN_IDENT = 4;
    private static final int TOKEN_ELLIPSIS = 5;

    private static final Keyword ALIAS = Keyword.intern("alias");
    private static final Keyword ARGUMENT = Keyword.intern("argument");
    private static final Keyword ARGUMENTS = Keyword.intern("arguments");
    private static final Keyword ARGUMENT_DEFINITION = Keyword.intern("argument-definition");
    private static final Keyword BASIC_TYPE = Keyword.intern("basic-type");
    private static final Keyword BOOLEAN_VALUE = Keyword.intern("boolean-value");
    private static final Keyword COLUMN = Keyword.intern("column");
    private static final Keyword CONSTANTS = Keyword.intern("constants");
    private static final Keyword DEFAULT_VALUE = Keyword.intern("default-value");
    private static final Keyword DIRECTIVE = Keyword.intern("directive");
    private static final Keyword DIRECTIVES = Keyword.intern("directives");
    private static final Keyword DIRECTIVE_DEFINITION = Keyword.intern("directive-definition");
    private static final Keyword END = Keyword.intern("end");
    private static final Keyword ENUM_VALUE = Keyword.intern("enum-value");
    private static final Keyword ENUM_CONSTANT = Keyword.intern("enum-constant");
    private static final Keyword ENUM_DEFINITION = Keyword.intern("enum-definition");
    private static final Keyword EXTEND_TYPE_DEFINITION = Keyword.intern("extend-type-definition");
    private static final Keyword FIELDS = Keyword.intern("fields");
    private static final Keyword FLOAT_VALUE = Keyword.intern("float-value");
    private static final Keyword FRAGMENT_DEFINITION = Keyword.intern("fragment-definition");
    private static final Keyword FRAGMENT_SPREAD = Keyword.intern("fragment-spread");
    private static final Keyword IMAGE = Keyword.intern("image");
    private static final Keyword IMPLEMENTS = Keyword.intern("implements");
    private static final Keyword INDEX = Keyword.intern("index");
    private static final Keyword INLINE_FRAGMENT = Keyword.intern("inline-fragment");
    private static final Keyword INNER_TYPE = Keyword.intern("inner-type");
    private static final Keyword INPUT_DEFINITION = Keyword.intern("input-definition");
    private static final Keyword INTERFACE_DEFINITION = Keyword.intern("interface-definition");
    private static final Keyword INT_VALUE = Keyword.intern("int-value");
    private static final Keyword LINE = Keyword.intern("line");
    private static final Keyword LIST_TYPE = Keyword.intern("list-type");
    private static final Keyword LIST_VALUE = Keyword.intern("list-value");
    private static final Keyword MEMBERS = Keyword.intern("members");
    private static final Keyword MUTATION = Keyword.intern("mutation");
    private static final Keyword NAME = Keyword.intern("name");
    private static final Keyword NULL_VALUE = Keyword.intern("null-value");
    private static final Keyword OBJECT_FIELD = Keyword.intern("object-field");
    private static final Keyword OBJECT_VALUE = Keyword.intern("object-value");
    private static final Keyword ON = Keyword.intern("on");
    private static final Keyword QUERY = Keyword.intern("query");
    private static final Keyword QUERY_DEFINITION = Keyword.intern("query-definition");
    private static final Keyword REQUIRED = Keyword.intern("required");
    private static final Keyword SCALAR_DEFINITION = Keyword.intern("scalar-definition");
    private static final Keyword SCHEMA = Keyword.intern("schema");
    private static final Keyword SCHEMA_DEFINITION = Keyword.intern("schema-definition");
    private static final Keyword SELECTION_FIELD = Keyword.intern("selection-field");
    private static final Keyword SELECTION_SET = Keyword.intern("selection-set");
    private static final Keyword START = Keyword.intern("start");
    private static final Keyword STRING_VALUE = Keyword.intern("string-value");
    private static final Keyword SUBSCRIPTION = Keyword.intern("subscription");
    private static final Keyword TAG = Keyword.intern("tag");
    private static final Keyword TYPE = Keyword.intern("type");
    private static final Keyword TYPE_DEFINITION = Keyword.intern("type-definition");
    private static final Keyword TYPE_FIELD = Keyword.intern("type-field");
    private static final Keyword TYPE_SYSTEM_DEFINITIONS = Keyword.intern("type-system-definitions");
    private static final Keyword UNION_DEFINITION = Keyword.intern("union-definition");
    private static final Keyword VALUE = Keyword.intern("value");
    private static final Keyword VALUES = Keyword.intern("values");
    private static final Keyword VARIABLE_DEFINITION = Keyword.intern("variable-definition");
    private static final Keyword VARIABLE_DEFINITIONS = Keyword.intern("variable-definitions");
    private static final Keyword VARIABLE_REFERENCE = Keyword.intern("variable-reference");

    private final String _input;
    private int _limit;
    private int _index;
    private int _line;
    private int _lineStart;

    private int _token;
    private IObj _startLocation;
    private String _image;
    private final StringBuilder _stringImage = new StringBuilder();
    
    public Parser(String input) {
        _line = 1;
        _lineStart = -1;
        _input = input;
        _limit = input.length();
        // populate the first token
        next();
    }

    static PersistentArrayMap map(Object ... args) {
        return new PersistentArrayMap(args);
    }

    private static IObj node(IPersistentMap meta, Object ... kvpairs) {
        return new PersistentArrayMap(meta, kvpairs);
    }

    private static IObj nodeWithLoc(Object start, Object end, Object ... kvpairs) {
        return node(map(START, start, END, end), kvpairs);
    }


    IObj location(int index) {
        return map(
            INDEX, index,
            LINE, _line,
            COLUMN, index - _lineStart);
    }

    ParseException tokenError(String msg) {
        throw new ParseException(location(_index), msg);
    }

    static boolean isDigit(char ch) {
        return '0' <= ch && ch <= '9';
    }

    void next() {
        nextImpl();
        // System.out.println("TOKEN: "+tokenDescription());
    }

    private void nextImpl() {
        for (char ch ; _index < _limit ; ++_index) {
            switch (ch = _input.charAt(_index)) {
            case ' ':
            case ',':
            case '\ufeff': // Byte Order Mark
                continue;

            case '\t':
                // TODO: handle tab's effect on column numbers
                // best test for this is schema-type-with-ignorables.
                continue;
                
            case '\r':
                if (_index + 1 < _limit && '\n' == _input.charAt(_index+1))
                    _index++;
                // fall through
            case '\n':
                _line++;
                _lineStart = _index;
                continue;
            case '#':
                while (++_index < _limit) {
                    ch = _input.charAt(_index);
                    
                    if (ch == '\n' || ch == '\r') {
                        _index--; // process CR or LF again
                        break;
                    }
                }
                continue;
            }

            int tokenStart = _index++;

            _image = null;
            _startLocation = null;

            switch (ch) {
            case '-':
                if (!(_index < _limit && isDigit(_input.charAt(_index))))
                    throw tokenError("expected digit after '-'");

                // consume the '-', and ...
                _index++;
                // ...fall through
            case '0':
                if (ch == '0' // a bit of hackery since we can fall through from '-' case
                    && _index < _limit && isDigit(_input.charAt(_index)))
                    throw tokenError("0-prefixed numbers are not allowed");
                // fall through
            case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                _startLocation = location(tokenStart);

                // the token is an integer until we see a '.' or an 'e'
                _token = TOKEN_INTEGER;

                while (_index < _limit && isDigit(ch = _input.charAt(_index)))
                    _index++;

                if (ch == '.') {
                    _token = TOKEN_FLOAT;
                    _index++;
                    if (!(_index < _limit && isDigit(ch = _input.charAt(_index))))
                        throw tokenError("expected digit after '.'");
                    _index++;
                    while (_index < _limit && isDigit(ch = _input.charAt(_index)))
                        _index++;
                }
                
                if (ch == 'e' || ch == 'E') {
                    _token = TOKEN_FLOAT;
                    _index++;
                    if (!(_index < _limit))
                        throw tokenError("expected '+', '-', or digit");
                    ch = _input.charAt(_index);
                    if ('+' == ch || '-' == ch) {
                        if (!(_index++ < _limit))
                            throw tokenError("expected digit");
                        ch = _input.charAt(_index);
                    }
                    if (!isDigit(ch))
                        throw tokenError("expected digit");
                    _index++;
                    while (_index < _limit && isDigit(_input.charAt(_index)))
                        _index++;
                }
                
                _image = _input.substring(tokenStart, _index);
                return;

            case '"':
                _startLocation = location(tokenStart);
                _stringImage.setLength(0);
                while (_index < _limit) {
                    if ('\"' == (ch = _input.charAt(_index++))) {
                        _image = _input.substring(tokenStart+1, _index-1);
                        _token = TOKEN_STRING;
                        return;
                    } else if (ch == '\\') {
                        if (_index >= _limit)
                            throw tokenError("unterminated string");
                        switch (ch = _input.charAt(_index++)) {
                        case '\"':
                        case '\\':
                        case '/': // TODO: double check, why is this here?
                            break;

                        case 'b': ch = '\b'; break;
                        case 'f': ch = '\f'; break;
                        case 'r': ch = '\r'; break;
                        case 't': ch = '\t'; break;
                        case 'n': ch = '\n'; break;

                        case 'u':
                            ch = 0;
                            for (int j = 0 ; j<4 ; ++j) {
                                if (_index >= _limit)
                                    throw tokenError("unterminated string");
                                char x = _input.charAt(_index++);
                                ch <<= 4;
                                if ('0' <= x && x <= '9') {
                                    ch |= x - '0';
                                } else if ('a' <= x && x <= 'f') {
                                    ch |= x - ('a' - 10);
                                } else if ('A' <= x && x <= 'F') {
                                    ch |= x - ('A' - 10);
                                } else {
                                    throw tokenError("invalid escape sequence");
                                }
                            }
                            break;
                        default:
                            throw tokenError("invalid escape sequence");
                        }
                    } else if (ch < ' ' && ch != '\t') {
                        throw tokenError("invalid character in string");
                    }
                    _stringImage.append(ch);
                }

                throw tokenError("unterminated string");

            case '_':
            case 'A': case 'B': case 'C': case 'D': case 'E':
            case 'F': case 'G': case 'H': case 'I': case 'J':
            case 'K': case 'L': case 'M': case 'N': case 'O':
            case 'P': case 'Q': case 'R': case 'S': case 'T':
            case 'U': case 'V': case 'W': case 'X': case 'Y':
            case 'Z':
            case 'a': case 'b': case 'c': case 'd': case 'e':
            case 'f': case 'g': case 'h': case 'i': case 'j':
            case 'k': case 'l': case 'm': case 'n': case 'o':
            case 'p': case 'q': case 'r': case 's': case 't':
            case 'u': case 'v': case 'w': case 'x': case 'y':
            case 'z':
                _startLocation = location(tokenStart);
                for ( ; _index < _limit ; _index++) {
                    ch = _input.charAt(_index);
                    if (!('a' <= ch && ch <= 'z' ||
                          'A' <= ch && ch <= 'Z' ||
                          '0' <= ch && ch <= '9' ||
                          ch == '_'))
                        break;
                }
                _image = _input.substring(tokenStart, _index);
                _token = TOKEN_IDENT;
                return;

            case '.':
                _startLocation = location(tokenStart);
                if (_index >= _limit)
                    throw tokenError("expected '...' or missing 0 prefix");
                if (_input.charAt(_index++) != '.')
                    throw tokenError("expected '...' or missing 0 prefix");
                if (_index >= _limit)
                    throw tokenError("expected '...'");
                if (_input.charAt(_index++) != '.')
                    throw tokenError("expected '...'");
                _token = TOKEN_ELLIPSIS;
                return;

            case '@':
            case '{':
            case '(':
            case '[':
            case '$':
                // don't waste time setting the start location for
                // tokens that do not start productions.  these 5
                // tokens can start productions, the next 7 cannot.
                _startLocation = location(tokenStart);
                // fall through
            case ']':
            case '!':
            case ':':
            case '}':
            case ')':
            case '|':
            case '=':
                _token = ch;
                return;

            default:
                throw tokenError(String.format("invalid character '%c'", ch));
            }
        }
        _token = TOKEN_EOF;
        return;
    }

    private String tokenDescription() {
        return tokenDescription(_token);
    }

    private String tokenDescription(int kind) {
        switch (kind) {
        case TOKEN_EOF:
            return "end of file";
        case TOKEN_INTEGER:
        case TOKEN_FLOAT:
        case TOKEN_STRING:
            return "'" + _image + "'";
        case TOKEN_IDENT:
            return _image;
        case TOKEN_ELLIPSIS:
            return "'...'";
        default:
            return new String(new char[] { '\'', (char)kind, '\'' });
        }
    }

    private void expect(int kind) {
        if (_token != kind)
            throw new ParseException(
                _startLocation,
                "Expected "+tokenDescription(kind)+
                ", found "+tokenDescription());
    }

    private void consume(int kind) {
        expect(kind);
        next();
    }

    private Symbol parseName() {
        if (TOKEN_IDENT != _token)
            throw new ParseException(
                _startLocation,
                "Expected name, found "+tokenDescription());
        
        Symbol name = (Symbol)Symbol.intern(_image)
            .withMeta(map(START, _startLocation, END, location(_index)));
        next();
        return name;
    }

    private IObj parseBasicType() {
        Symbol name = parseName();
        return node(name.meta(), TAG, BASIC_TYPE, NAME, name);
    }

    private IObj parseTypeRef() {
        if ('[' == _token) {
            IObj start = _startLocation;
            next();
            IObj inner = parseTypeRef();
            IObj end = location(_index);
            consume(']');
            if ('!' != _token) {
                return nodeWithLoc(
                    start, end,
                    TAG, LIST_TYPE,
                    INNER_TYPE, inner);
            } else {
                end = location(_index);
                next(); // '!'
                return nodeWithLoc(
                    start, end,
                    TAG, LIST_TYPE,
                    INNER_TYPE, inner,
                    REQUIRED, true);
            }
        } else {
            Symbol name = parseName();
            Object end = name.meta().valAt(END);
            if ('!' != _token) {
                return nodeWithLoc(
                    name.meta().valAt(START), end,
                    TAG, BASIC_TYPE,
                    NAME, name);
            } else {
                end = location(_index);
                next();
                return nodeWithLoc(
                    name.meta().valAt(START), end,
                    TAG, BASIC_TYPE,
                    NAME, name,
                    REQUIRED, true);
            }
        }
    }

    private IObj parseVec(int startToken, int endToken, Supplier<Object> itemParser) {
        Object start = _startLocation;
        consume(startToken);
        List<Object> list = new ArrayList();
        while (_token != endToken) {
            list.add(itemParser.get());
        }
        Object end = location(_index);
        next();
        return PersistentVector.create(list).withMeta(
            map(START, start, END, end));
    }

    IObj parseArgumentDefinition() {
        Object[] map = new Object[8];
        int i = 0;
        map[i++] = TAG;
        map[i++] = ARGUMENT_DEFINITION;
        Symbol name = parseName();
        map[i++] = NAME;
        map[i++] = name;
        consume(':');
        IObj type = parseTypeRef();
        map[i++] = TYPE;
        map[i++] = type;

        if ('=' == _token) {
            next();
            IObj defVal = parseValue();
            map[i++] = DEFAULT_VALUE;
            map[i++] = defVal;
        }

        return nodeWithLoc(
            name.meta().valAt(START),
            ((IObj)map[i-1]).meta().valAt(END),
            Arrays.copyOf(map, i));
    }

    // package private to avoid compiler-generated private accesor
    // with lambda usage.
    IObj parseTypeField() {
        Object[] map = new Object[8];
        int i = 0;
        map[i++] = TAG;
        map[i++] = TYPE_FIELD;
        
        Symbol name = parseName();
        map[i++] = NAME;
        map[i++] = name;

        if ('(' == _token) {
            map[i++] = ARGUMENTS;
            map[i++] = parseVec('(', ')', this::parseArgumentDefinition).withMeta(null);
        }

        consume(':');

        IObj type = parseTypeRef();
        map[i++] = TYPE;
        map[i++] = type;

        return nodeWithLoc(
            name.meta().valAt(START),
            type.meta().valAt(END),
            Arrays.copyOf(map, i));
        
        // Object arguments = ('(' == _token)
        //     ? parseVec('(', ')', this::parseArgumentDefinition)
        //     : null;
        // consume(':');
        // IObj type = parseTypeRef();
        // return nodeWithLoc(
        //     name.meta().valAt(START),
        //     type.meta().valAt(END),
        //     TAG, TYPE_FIELD,
        //     NAME, name,
        //     ARGUMENTS, arguments, 
        //     TYPE, type);
    }

    IObj parseTypeDefinition(IObj start, Keyword tag) {
        Object[] map = new Object[8];
        int i = 0;
        map[i++] = TAG;
        map[i++] = tag;
        
        next(); // "type"
        Symbol name = parseName();
        map[i++] = NAME;
        map[i++] = name;
        
        if (TOKEN_IDENT == _token && "implements".equals(_image)) {
            next();
            List<IObj> impls = new ArrayList<>();
            do {
                impls.add(parseBasicType());
            } while (TOKEN_IDENT == _token);

            map[i++] = IMPLEMENTS;
            map[i++] = PersistentVector.create(impls);
        }

        IObj fields = parseVec('{', '}', this::parseTypeField);
        Object end = fields.meta().valAt(END);
        map[i++] = FIELDS;
        map[i++] = fields.withMeta(null);  // TODO: remove withMeta(null)

        return nodeWithLoc(
            start, end,
            Arrays.copyOf(map, i));
    }

    IObj parseInterfaceDefinition() {
        IObj start = _startLocation;
        next();
        Symbol name = parseName();
        IObj fields = parseVec('{', '}', this::parseTypeField);

        return nodeWithLoc(
            start, fields.meta().valAt(END),
            TAG, INTERFACE_DEFINITION,
            NAME, name,
            FIELDS, fields.withMeta(null)); // TODO: remove withMeta(null)
    }

    IObj parseInputDefinition() {
        IObj start = _startLocation;
        next(); // "input"
        Symbol name = parseName();
        IObj fields = parseVec('{', '}', this::parseTypeField);
        return nodeWithLoc(
            start, fields.meta().valAt(END),
            TAG, INPUT_DEFINITION,
            NAME, name,
            FIELDS, fields.withMeta(null));
    }

    IObj parseUnionDefinition() {
        IObj start = _startLocation;
        next(); // "union"
        Symbol name = parseName();
        consume('=');
        List<Object> members = new ArrayList<>();
        IObj lastType = parseBasicType();
        members.add(lastType);
        while ('|' == _token) {
            next();
            members.add(lastType = parseBasicType());
        }
        return nodeWithLoc(
            start, lastType.meta().valAt(END),
            TAG, UNION_DEFINITION,
            NAME, name,
            MEMBERS, PersistentVector.create(members));
    }

    Keyword parseSchemaTag() {
        if (TOKEN_IDENT == _token) {
            switch (_image) {
            case "query":
                return QUERY;
            case "mutation":
                return MUTATION;
            case "subscription":
                return SUBSCRIPTION;
            }
        }
        throw new ParseException(
            _startLocation,
            "expected 'query', 'mutation', or 'subscription'.  Found "+
            tokenDescription());
    }

    IObj parseSchemaType() {
        IObj start = _startLocation;
        Keyword tag = parseSchemaTag();
        next(); // consume the tag, the parseSchemaType does not
        consume(':');
        Symbol name = parseName();
        return nodeWithLoc(
            start, name.meta().valAt(END),
            TAG, tag,
            NAME, name);
    }

    IObj parseSchemaDefinition() {
        IObj start = _startLocation;
        next(); // "schema"
        IObj members = parseVec('{', '}', this::parseSchemaType);
        return nodeWithLoc(
            start, members.meta().valAt(END),
            TAG, SCHEMA_DEFINITION,
            MEMBERS, members.withMeta(null)); // TODO: remove withMeta(null)
    }

    IObj parseEnumConstant() {
        Symbol name = parseName();
        IObj directives = parseDirectives();
        if (directives == null) {
            return node(
                name.meta(),
                TAG, ENUM_CONSTANT,
                NAME, name);
        } else {
            return nodeWithLoc(
                name.meta().valAt(START), directives.meta().valAt(END),
                TAG, ENUM_CONSTANT,
                NAME, name,
                DIRECTIVES, directives.withMeta(null)); // TODO: remove withMeta(null)
        }
    }

    IObj parseEnumDefinition() {
        IObj start = _startLocation;
        next(); // "enum"
        Symbol name = parseName();
        IObj constants = parseVec('{', '}', this::parseEnumConstant);
        return nodeWithLoc(
            start, constants.meta().valAt(END),
            TAG, ENUM_DEFINITION,
            NAME, name,
            CONSTANTS, constants.withMeta(null)); // TODO: remove withMeta(null)
    }

    IObj parseTypeConditionOpt() {
        if (TOKEN_IDENT != _token || !"on".equals(_image))
            return null;
        
        IObj start = _startLocation;
        next();
        IObj type = parseBasicType();

        // TODO: for backwards compatilbity with the AST, the
        // type-condition's "on" is the start location.
        return type.withMeta(type.meta().assoc(START, start));
    }

    IObj parseDirectiveDefinition() {
        Object[] map = new Object[6];
        int i = 0;
        IObj start = _startLocation;
        next(); // "directive"
        expect('@');
        map[i++] = TAG;
        map[i++] = DIRECTIVE_DEFINITION;

        Symbol name = parseName();
        map[i++] = NAME;
        map[i++] = name;
        
        IObj on = parseTypeConditionOpt();
        if (on != null) {
            map[i++] = ON;
            map[i++] = on;
        }

        return nodeWithLoc(
            start, (on == null ? name : on).meta().valAt(END),
            Arrays.copyOf(map, i));
    }

    IObj parseExtendTypeDefinition() {
        IObj start = _startLocation;
        next(); // "extend"
        return parseTypeDefinition(start, EXTEND_TYPE_DEFINITION);
    }

    IObj parseScalarDefinition() {
        IObj start = _startLocation;
        next(); // "scalar"
        Symbol name = parseName();
        return nodeWithLoc(
            start, name.meta().valAt(END),
            TAG, SCALAR_DEFINITION,
            NAME, name);
    }

    IObj parseTypeSystemDefinition() {
        if (TOKEN_IDENT == _token) {
            switch (_image) {
            case "type":
                return parseTypeDefinition(_startLocation, TYPE_DEFINITION);
            case "interface":
                return parseInterfaceDefinition();
            case "union":
                return parseUnionDefinition();
            case "schema":
                return parseSchemaDefinition();
            case "enum":
                return parseEnumDefinition();
            case "input":
                return parseInputDefinition();
            case "directive":
                return parseDirectiveDefinition();
            case "extend":
                return parseExtendTypeDefinition();
            case "scalar":
                return parseScalarDefinition();
            }
        }

        throw new ParseException(
            _startLocation,
            "Expected 'type', 'interface', 'union', 'schema', 'enum', 'input', "+
            "'directive', 'extend', or 'scalar'.  Found "+tokenDescription());
    }

    private PersistentVector parseTypeSystemDefinitions() {
        if (_token == TOKEN_EOF){
            return PersistentVector.EMPTY;
        }
        List<IObj> defs = new ArrayList<>();
        do {
            defs.add(parseTypeSystemDefinition());
        } while (_token != TOKEN_EOF);
        
        return PersistentVector.create(defs);
    }

    public IObj parseSchema() {
        PersistentVector tsd = parseTypeSystemDefinitions();
        Object start;
        Object end;
        return map(
            TAG, SCHEMA,
            TYPE_SYSTEM_DEFINITIONS, tsd);
    }

    private IObj parseSelection() {
        if (TOKEN_IDENT == _token) {
            int i = 0;
            Object[] map = new Object[12];
            map[i++] = TAG;
            map[i++] = SELECTION_FIELD;
            
            Symbol name = parseName();
            Object start = name.meta().valAt(START);
            Symbol alias = null;
            if (':' == _token) {
                next();
                alias = name;
                name = parseName();
                map[i++] = ALIAS;
                map[i++] = alias;
            }
            map[i++] = NAME;
            map[i++] = name;

            Object end = name.meta().valAt(END);

            IObj arguments = null;
            if ('(' == _token) {
                arguments = parseVec('(', ')', this::parseArgument);
                end = arguments.meta().valAt(END);
                map[i++] = ARGUMENTS;
                map[i++] = arguments.withMeta(null);
            }

            IObj directives = parseDirectives();
            if (directives != null) {
                end = directives.meta().valAt(END);
                map[i++] = DIRECTIVES;
                map[i++] = directives;
            }

            IObj sset = null;
            if ('{' == _token) {
                sset = parseSelectionSet();
                end = sset.meta().valAt(END);
                map[i++] = SELECTION_SET;
                map[i++] = sset.withMeta(null);
            }
            return nodeWithLoc(start, end, Arrays.copyOf(map, i));
            // return nodeWithLoc(
            //     start, end,
            //     TAG, SELECTION_FIELD,
            //     NAME, name,
            //     ALIAS, alias,
            //     ARGUMENTS, arguments,
            //     DIRECTIVES, directives,
            //     SELECTION_SET, sset);
        } else if (TOKEN_ELLIPSIS == _token) {
            int i = 0;
            Object[] map = new Object[8];
            map[i++] = TAG;
            i++; // added later
            
            IObj start = _startLocation;
            next();
            IObj on = null;
            if (TOKEN_IDENT == _token) {
                if ("on".equals(_image)) {
                    // TODO: convert this if block to parseTypeCondition
                    // note: that this is the correct way to populate `on`
                    on = parseTypeConditionOpt();
                    map[i++] = ON;
                    map[i++] = on;
                } else {
                    Symbol name = parseName();
                    map[i++] = NAME;
                    map[i++] = name;
                    
                    IObj directives = parseDirectives();
                    if (directives != null) {
                        map[i++] = DIRECTIVES;
                        map[i++] = directives;
                    }
                    map[1] = FRAGMENT_SPREAD;
                    
                    return nodeWithLoc(
                        start, (directives != null ? directives : name).meta().valAt(END),
                        Arrays.copyOf(map, i));
                        // TAG, FRAGMENT_SPREAD,
                        // NAME, name,
                        // DIRECTIVES, directives);
                }
            }

            map[1] = INLINE_FRAGMENT;
            
            IObj directives = parseDirectives();
            if (directives != null) {
                map[i++] = DIRECTIVES;
                map[i++] = directives;
            }
            
            IObj sset = parseSelectionSet();
            map[i++] = SELECTION_SET;
            map[i++] = sset.withMeta(null);

            return nodeWithLoc(start, sset.meta().valAt(END), Arrays.copyOf(map, i));
            // return nodeWithLoc(
            //     start, sset.meta().valAt(END),
            //     TAG, INLINE_FRAGMENT,
            //     ON, on,
            //     SELECTION_SET, sset);
        } else {
            throw new ParseException(
                _startLocation,
                "Expected field name or '...'.  Found "+tokenDescription());
        }
    }

    private IObj parseSelectionSet() {
        return parseVec('{', '}', this::parseSelection);
    }

    private IObj parseAnonymousQuery() {
        IObj sset = parseSelectionSet();
        return node(
            sset.meta(),
            TAG, SELECTION_SET,
            SELECTION_SET, sset.withMeta(null)); // TODO: remove withMeta(null)
    }

    private IObj parseObjectField() {
        Symbol name = parseName();
        consume(':');
        IObj value = parseValue();
        return nodeWithLoc(
            name.meta().valAt(START),
            value.meta().valAt(END),
            TAG, OBJECT_FIELD,
            NAME, name,
            VALUE, value);
    }

    private IObj parseVarRef() {
        IObj start = _startLocation;
        next(); // '$'
        Symbol name = parseName();
        return nodeWithLoc(
            start, name.meta().valAt(END),
            TAG, VARIABLE_REFERENCE,
            NAME, name);
    }

    private IObj parseObjectValue() {
        IObj fields = parseVec('{', '}', this::parseObjectField);
        return node(
            fields.meta(),
            TAG, OBJECT_VALUE,
            FIELDS, fields.withMeta(null)); // TODO: remove withMeta(null)
    }

    private IObj parseListValue() {
        IObj values = parseVec('[', ']', this::parseValue);
        return node(
            values.meta(),
            TAG, LIST_VALUE,
            VALUES, values.withMeta(null)); // TODO: remove withMeta(null)
    }
    
    private IObj parseValue() {
        Keyword tag;
        Object value;
        
        switch (_token) {
        case TOKEN_INTEGER:
            tag = INT_VALUE;
            value = Long.parseLong(_image, 10);
            break;
        case TOKEN_FLOAT:
            tag = FLOAT_VALUE;
            value = new Double(_image);
            break;
        case TOKEN_STRING:
            tag = STRING_VALUE;
            value = _stringImage.toString();
            break;
        case TOKEN_IDENT:
            switch (_image) {
            case "null":
                tag = NULL_VALUE;
                value = null;
                break;
            case "true":
                tag = BOOLEAN_VALUE;
                value = Boolean.TRUE;
                break;
            case "false":
                tag = BOOLEAN_VALUE;
                value = Boolean.FALSE;
                break;
            default:
                tag = ENUM_VALUE;
                value = Symbol.intern(_image);
                break;
            }
            break;
        case '$':
            return parseVarRef();
        case '[':
            return parseListValue();
        case '{':
            return parseObjectValue();
        default:
            throw new ParseException(
                _startLocation,
                "expected value, found "+tokenDescription());
        }

        IObj start = _startLocation;
        IObj end = location(_index);
        String image = _image;
        next();
        return nodeWithLoc(
            start, end,
            TAG, tag,
            IMAGE, image,
            VALUE, value);
    }

    private IObj parseVariableDefinition() {
        IObj start = _startLocation;
        consume('$');
        Symbol name = parseName();
        consume(':');
        IObj type = parseTypeRef();

        if ('=' != _token) {
            return nodeWithLoc(
                start, type.meta().valAt(END),
                TAG, VARIABLE_DEFINITION,
                NAME, name,
                TYPE, type);
        } else {
            next();
            IObj defVal = parseValue();
            return nodeWithLoc(
                start, defVal.meta().valAt(END),
                TAG, VARIABLE_DEFINITION,
                NAME, name,
                TYPE, type,
                DEFAULT_VALUE, defVal);
        }
    }

    private IObj parseArgument() {
        Symbol name = parseName();
        consume(':');
        IObj value = parseValue();
        return nodeWithLoc(
            name.meta().valAt(START),
            value.meta().valAt(END),
            TAG, ARGUMENT,
            NAME, name,
            VALUE, value);
    }

    private IObj parseDirectives() {
        if ('@' != _token)
            return null;

        IObj firstStart = _startLocation;
        Object lastEnd;
        List<Object> directives = new ArrayList<>();
        Object[] map = new Object[6];
        map[0] = TAG;
        map[1] = DIRECTIVE;
        do {
            IObj start = _startLocation;
            next(); // '@'
            Symbol name = parseName();
            map[2] = NAME;
            map[3] = name;

            int i = 4;
            
            Object end;
            IObj arguments = null;
            if ('(' == _token) {
                arguments = parseVec('(', ')', this::parseArgument);
                end = arguments.meta().valAt(END);
                map[i++] = ARGUMENTS;
                map[i++] = arguments.withMeta(null); // TODO: remove withMeta(null)
            } else {
                end = name.meta().valAt(END);
            }

            directives.add(
                nodeWithLoc(
                    start, lastEnd = end,
                    Arrays.copyOf(map, i)));
        } while ('@' == _token);

        return PersistentVector.create(directives)
            .withMeta(map(START, firstStart, END, lastEnd));
    }

    private IObj parseOperationDefinition(Keyword tag) {
        Object[] map = new Object[10];
        int i = 0;
        map[i++] = TAG;
        map[i++] = tag;
        
        IObj start = _startLocation;
        next(); // "query" or "mutation"
        Symbol name = null;
        if (TOKEN_IDENT == _token) {
            name = parseName();
            map[i++] = NAME;
            map[i++] = name;
        }
        boolean metaBug = false;
        // optional
        IObj varDefs = null;
        if ('(' == _token) {
            varDefs = parseVec('(', ')', this::parseVariableDefinition);
            map[i++] = VARIABLE_DEFINITIONS;
            map[i++] = varDefs.withMeta(null); // TODO: remove withMeta(null)
            metaBug = true;
        }
        
        IObj directives = parseDirectives();
        if (directives != null) {
            map[i++] = DIRECTIVES;
            map[i++] = directives;
            metaBug = true;
        }
        
        IObj sset = parseSelectionSet();
        map[i++] = SELECTION_SET;
        map[i++] = sset.withMeta(null); // TODO: remove withMeta(null)
    
        return nodeWithLoc(
            start, sset.meta().valAt(END),
            Arrays.copyOf(map, i));
    }

    private IObj parseFragmentDefinition() {
        Object[] map = new Object[10];
        int i = 0;
        map[i++] = TAG;
        map[i++] = FRAGMENT_DEFINITION;
        
        IObj start = _startLocation;
        next(); // "fragment"
        Symbol name = parseName();
        map[i++] = NAME;
        map[i++] = name;
        
        IObj on = parseTypeConditionOpt();
        if (on != null) {
            map[i++] = ON;
            map[i++] = on;
        }
        
        IObj directives = parseDirectives();
        if (directives != null) {
            map[i++] = DIRECTIVES;
            map[i++] = directives;
        }
        
        IObj sset = parseSelectionSet();
        map[i++] = SELECTION_SET;
        map[i++] = sset.withMeta(null);
        return nodeWithLoc(
            start, sset.meta().valAt(END),
            Arrays.copyOf(map, i));
    }

    private IObj parseQueryElement() {
        if ('{' == _token) {
            return parseAnonymousQuery();
        } else if (TOKEN_IDENT == _token) {
            switch (_image) {
            case "query":
                return parseOperationDefinition(QUERY_DEFINITION);
            case "mutation":
                return parseOperationDefinition(MUTATION);
            case "fragment":
                return parseFragmentDefinition();
            }
        }
        throw new ParseException(
            _startLocation,
            "Expected '{', 'query', 'mutation', or 'fragment'.  Found "+tokenDescription());
    }

    public IObj parseQueryDocument() {
        if (_token == TOKEN_EOF) {
            return PersistentVector.EMPTY.withMeta(
                map(START, _startLocation, END, location(_index)));
        }
        
        List<IObj> queries = new ArrayList<>();
        do {
            queries.add(parseQueryElement());
        } while (_token != TOKEN_EOF);
        
        return PersistentVector.create(queries).withMeta(
            map(START, map(INDEX, 0, LINE, 1, COLUMN, 1), // queries.get(0).meta().valAt(START),
                END, location(_index)));
    }
}
