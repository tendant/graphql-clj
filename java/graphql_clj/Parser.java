package graphql_clj;

import clojure.lang.IMeta;
import clojure.lang.IObj;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentArrayMap;
import clojure.lang.PersistentVector;
import clojure.lang.Symbol;
import java.util.Arrays;
import java.util.function.Supplier;

public class Parser {
    private static final int TOKEN_EOF = -1;
    private static final int TOKEN_INTEGER = 1;
    private static final int TOKEN_FLOAT = 2;
    private static final int TOKEN_STRING = 3;
    private static final int TOKEN_IDENT = 4;
    private static final int TOKEN_ELLIPSIS = 5;

    private static final char BYTE_ORDER_MARK = '\ufeff';

    private static final int STATE_NEGATIVE = -1;
    private static final int STATE_ZERO = 0;
    private static final int STATE_INTEGER = 1;
    private static final int STATE_DOT = 2;
    private static final int STATE_E = 3;

    private static final Keyword ALIAS = Keyword.intern("alias");
    private static final Keyword ARGUMENT = Keyword.intern("argument");
    private static final Keyword ARGUMENTS = Keyword.intern("arguments");
    private static final Keyword ARGUMENT_DEFINITION = Keyword.intern("argument-definition");
    private static final Keyword BASIC_TYPE = Keyword.intern("basic-type");
    private static final Keyword BOOLEAN_VALUE = Keyword.intern("boolean-value");
    private static final Keyword CONSTANTS = Keyword.intern("constants");
    private static final Keyword DEFAULT_VALUE = Keyword.intern("default-value");
    private static final Keyword DIRECTIVE = Keyword.intern("directive");
    private static final Keyword DIRECTIVES = Keyword.intern("directives");
    private static final Keyword DIRECTIVE_DEFINITION = Keyword.intern("directive-definition");
    private static final Keyword DOC = Keyword.intern("doc");
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
    private static final Keyword INLINE_FRAGMENT = Keyword.intern("inline-fragment");
    private static final Keyword INNER_TYPE = Keyword.intern("inner-type");
    private static final Keyword INPUT_DEFINITION = Keyword.intern("input-definition");
    private static final Keyword INTERFACE_DEFINITION = Keyword.intern("interface-definition");
    private static final Keyword INT_VALUE = Keyword.intern("int-value");
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
    private int _tokenStart;
    private Location _startLocation;
    private String _image;
    private final StringBuilder _stringValue = new StringBuilder();
    private int _docStart;
    private int _docEnd;

    private Object[] _stack = new Object[64];
    private int _stackTop;

    public Parser(String input) {
        _line = 1;
        _lineStart = -1;
        _input = input;
        _limit = input.length();
        // populate the first token
        next();
    }

    private void ensureCapacity() {
        if (_stackTop >= _stack.length)
            _stack = Arrays.copyOf(_stack, _stackTop*2);
    }

    private void push(Keyword key, Object value) {
        ensureCapacity();
        _stack[_stackTop++] = key;
        _stack[_stackTop++] = value;
    }

    private void push(Object value) {
        ensureCapacity();
        _stack[_stackTop++] = value;
    }

    private Object[] pop(int topIndex) {
        final int botIndex = _stackTop;
        return Arrays.copyOfRange(_stack, _stackTop = topIndex, botIndex);
    }

    private PersistentVector popVec(int topIndex) {
        final int botIndex = _stackTop;
        return (PersistentVector)LazilyPersistentVector.createOwning(
            Arrays.copyOfRange(_stack, _stackTop = topIndex, botIndex));
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

    Location location(int index) {
        return new Location(_line, index - _lineStart, index);
    }
    
    Location startLocation() {
        if (_startLocation == null)
            _startLocation = location(_tokenStart);
        return _startLocation;
    }

    ParseException tokenError(int index, String msg) {
        throw new ParseException(location(index), msg);
    }

    ParseException expectedError(String expected) {
        throw new ParseException(
            startLocation(),
            "Expected "+expected+".  Found "+tokenDescription());
    }

    static boolean isDigit(char ch) {
        return '0' <= ch && ch <= '9';
    }

    private void next() {
        _token = nextImpl();
    }

    void tabAdjust(int tabIndex) {
        // TODO: update _lineStart to make subsequent column
        // computation correct.
    }

    private void documentComment() {
        if (_docStart >= 0)
            push(DOC, _input.substring(_docStart, _docEnd));
    }

    private int nextImpl() {
        char ch;
        int i;
        int state;

        _tokenStart = _index;
        _startLocation = null;
        // clear out the document comment from the previous token.
        // use -2 to indicate we have not seen a new line.  We have a
        // special case for the first charater in the input, that is
        // assumed to follow a newline.
        _docStart = _tokenStart == 0 ? -1 : -2;
        
    outer:
        for (;; _tokenStart++) {
            if (_tokenStart >= _limit) {
                _index = _tokenStart;
                return TOKEN_EOF;
            }

            switch (ch = _input.charAt(_tokenStart)) {
            case '\t':
                tabAdjust(_tokenStart);
                // fall through
            case ' ':
            case ',':
            case BYTE_ORDER_MARK:
                continue;
            case '\r':
                if (_tokenStart+1 < _limit && '\n' == _input.charAt(_tokenStart+1))
                    _tokenStart++;
                // fall through
            case '\n':
                _line++;
                _lineStart = _tokenStart;
                // seen a newline, if there's a comment on the
                // following line, it can start a document comment.
                // This also clears out the previous document comment
                // if there was one, since we want our document
                // comments to be contiguous.
                _docStart = -1;
                continue;
            case '#':
                if (_docStart == -1)
                    _docStart = _tokenStart;
            comment:
                while (++_tokenStart < _limit) {
                    switch (_input.charAt(_docEnd = _tokenStart)) {
                    case '\r':
                        if (_tokenStart+1 < _limit && '\n' == _input.charAt(_tokenStart+1))
                            _tokenStart++;
                        // fall through
                    case '\n':
                        _line++;
                        _lineStart = _tokenStart;
                        if (_docStart < 0) {
                            // this is not a document comment, but the
                            // next comment could be.
                            _docStart = -1;
                            continue outer;
                        }

                        while (++_tokenStart < _limit) {
                            switch (ch = _input.charAt(_tokenStart)) {
                            case '\t':
                                tabAdjust(_tokenStart);
                                // fall through
                            case ' ':
                            case ',':
                            case BYTE_ORDER_MARK:
                                continue;
                            case '#':
                                continue comment;
                            default:
                                // process the character again
                                --_tokenStart;
                                continue outer;
                            }
                        }
                        continue outer;
                    }
                }
                _index = _tokenStart;
                return TOKEN_EOF;

            case '@':
            case '{':
            case '(':
            case '[':
            case '$':
            case ']':
            case '!':
            case ':':
            case '}':
            case ')':
            case '|':
            case '=':
                _index = _tokenStart + 1;
                return ch;

            case '.':
                if (_tokenStart + 2 < _limit
                    && '.' == _input.charAt(_tokenStart + 1)
                    && '.' == _input.charAt(_tokenStart + 2)) {
                    _index = _tokenStart + 3;
                    return TOKEN_ELLIPSIS;
                } else {
                    throw tokenError(
                        _tokenStart,
                        "invalid character sequence, did you mean '...'?");
                }

            case '"':
                // need to track start location explicitly on strings,
                // since their contents can make
                // location(_startLocaiton) become invalid (once \t is
                // handled)
                _startLocation = location(_tokenStart);
                _stringValue.setLength(0);
                for (i=_tokenStart+1 ; i<_limit ; ) {
                    if ('"' == (ch = _input.charAt(i++))) {
                        _image = _input.substring(_tokenStart, _index = i);
                        return TOKEN_STRING;
                    } else if ('\\' == ch) {
                        if (i >= _limit)
                            throw tokenError(i, "unterminated string");
                        switch (ch = _input.charAt(i++)) {
                        case '\"': case '\\': case '/':
                            break;
                        case 'b': ch = '\b'; break;
                        case 'f': ch = '\f'; break;
                        case 'r': ch = '\r'; break;
                        case 't': ch = '\t'; break;
                        case 'n': ch = '\n'; break;

                        case 'u':
                            if (i+4 >= _limit)
                                throw tokenError(i, "unterminated string");
                            for (int n=i+4 ; i<n ; ) {
                                char x = _input.charAt(i++);
                                if ('0' <= x && x <= '9') {
                                    ch = (char)((ch << 4) | (x - '0'));
                                } else if ('A' <= (x &= ~0x20) && x <= 'F') {
                                    // if x is a letter, then x & ~0x20 will convert
                                    // lowercase to uppercase, and leave uppercase alone
                                    ch = (char)((ch << 4) | (x - ('A' - 10)));
                                } else {
                                    throw tokenError(i, "invalid hex escape");
                                }
                            }
                            break;

                        default:
                            throw tokenError(i, "invalid escape sequence");
                        }
                    } else if (ch < ' ') {
                        if (ch == '\t') {
                            tabAdjust(i-1);
                        } else {
                            throw tokenError(i, "invalid character in string");
                        }
                    }
                    _stringValue.append(ch);
                }
                throw tokenError(_limit, "unterminated string");
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
                for (i=_tokenStart ; ++i<_limit ; ) {
                    ch = _input.charAt(i);
                    if (!('a' <= ch && ch <= 'z' ||
                          'A' <= ch && ch <= 'Z' ||
                          '0' <= ch && ch <= '9' || ch == '_'))
                        break;
                }
                _image = _input.substring(_tokenStart, _index = i);
                return TOKEN_IDENT;

            case '-':
                state = STATE_NEGATIVE;
                break;
            case '0':
                state = STATE_ZERO;
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                state = STATE_INTEGER;
                break;
            default:
                throw tokenError(_tokenStart, String.format("invalid character U+%04x", (int)ch));
            } // switch on char

        stateLoop:
            for (i=_tokenStart+1 ;; ++i) {
                switch (state) {
                case STATE_ZERO:
                    if (i < _limit) {
                        ch = _input.charAt(i);
                        if ('.' == ch) {
                            state = STATE_DOT;
                            continue;
                        } else if ('e' == ch || 'E' == ch) {
                            state = STATE_E;
                            continue;
                        } else if ('0' <= ch && ch <= '9') {
                            throw tokenError(i, "zero-prefixed numbers are not allowed");
                        }
                    }
                    _image = _input.substring(_tokenStart, _index = i);
                    return TOKEN_INTEGER;

                case STATE_NEGATIVE:
                    if (i >= _limit)
                        throw tokenError(i, "expected digit after '-'");

                    ch = _input.charAt(i);
                    if ('1' <= ch && ch <= '9') {
                        state = STATE_INTEGER;
                        // fall through
                    } else if ('0' == ch) {
                        state = STATE_ZERO;
                        continue stateLoop;
                    } else {
                        throw tokenError(i, "expected digit after '-'");
                    }

                case STATE_INTEGER:
                    for ( ; i < _limit ; ++i) {
                        ch = _input.charAt(i);
                        if ('0' <= ch && ch <= '9') {
                            continue;
                        } else if ('.' == ch) {
                            state = STATE_DOT;
                        } else if ('e' == ch || 'E' == ch) {
                            state = STATE_E;
                        } else {
                            break;
                        }
                        continue stateLoop;
                    }
                    _image = _input.substring(_tokenStart, _index = i);
                    return TOKEN_INTEGER;
                case STATE_DOT:
                    if (!((i < _limit) && '0' <= (ch = _input.charAt(i)) && ch <= '9'))
                        throw tokenError(i, "expected digit after '.'");
                    while (++i < _limit) {
                        ch = _input.charAt(i);
                        if ('0' <= ch && ch <= '9')
                            continue;
                        if ('e' == ch || 'E' == ch) {
                            state = STATE_E;
                            continue stateLoop;
                        }
                        break;
                    }
                    _image = _input.substring(_tokenStart, _index = i);
                    return TOKEN_FLOAT;

                case STATE_E:
                    if (i >= _limit)
                        throw tokenError(i, "expected digit after 'e'");
                    if ('+' == (ch = _input.charAt(i)) || '-' == ch) {
                        if (++i >= _limit)
                            throw tokenError(i, "expected digit after 'e'");
                        ch = _input.charAt(i);
                    }
                    if (!('0' <= ch && ch <= '9'))
                        throw tokenError(i, "expected digit after 'e'");
                    while (++i < _limit) {
                        ch = _input.charAt(i);
                        if (!('0' <= ch && ch <= '9'))
                            break;
                    }
                    _image = _input.substring(_tokenStart, _index = i);
                    return TOKEN_FLOAT;

                default:
                    throw new AssertionError("bad state: "+state);
                }
            } // stateLoop

        } // outer: for

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
            throw expectedError(tokenDescription(kind));
    }

    private void consume(int kind) {
        expect(kind);
        next();
    }

    private Symbol parseName() {
        if (TOKEN_IDENT != _token)
            throw expectedError("name");

        Symbol name = (Symbol)Symbol.intern(_image)
            .withMeta(map(START, startLocation(), END, location(_index)));
        next();
        return name;
    }

    private IObj parseBasicType() {
        Symbol name = parseName();
        return node(name.meta(), TAG, BASIC_TYPE, NAME, name);
    }

    private IObj parseTypeRef() {
        int topIndex = _stackTop;
        Location start = startLocation();
        Location end;
        if ('[' == _token) {
            next();
            push(TAG, LIST_TYPE);
            push(INNER_TYPE, parseTypeRef());
            end = location(_index);
            consume(']');
        } else if (TOKEN_IDENT == _token) {
            push(TAG, BASIC_TYPE);
            Symbol name = parseName();
            push(NAME, name);
            end = (Location)name.meta().valAt(END);
        } else {
            throw expectedError("'[' or type name");
        }
        if ('!' == _token) {
            end = location(_index);
            next();
            push(REQUIRED, true);
        }
        return nodeWithLoc(start, end, pop(topIndex));
    }

    private IObj parseVec(int startToken, int endToken, Supplier<Object> itemParser) {
        Location start = startLocation();
        consume(startToken);
        int topIndex = _stackTop;
        while (_token != endToken) {
            push(itemParser.get());
        }
        Object end = location(_index);
        next();
        return popVec(topIndex).withMeta(map(START, start, END, end));
    }

    IObj parseArgumentDefinition() {
        int topIndex = _stackTop;
        push(TAG, ARGUMENT_DEFINITION);
        Symbol name = parseName();
        push(NAME, name);
        consume(':');
        IObj lastObj = parseTypeRef();
        push(TYPE, lastObj);

        if ('=' == _token) {
            next();
            push(DEFAULT_VALUE, lastObj = parseValue());
        }

        return nodeWithLoc(
            name.meta().valAt(START),
            lastObj.meta().valAt(END),
            pop(topIndex));
    }

    // package private to avoid compiler-generated private accesor
    // with lambda usage.
    IObj parseTypeField() {
        int topIndex = _stackTop;
        push(TAG, TYPE_FIELD);
        documentComment();

        Symbol name = parseName();
        push(NAME, name);
        if ('(' == _token)
            push(ARGUMENTS, parseVec('(', ')', this::parseArgumentDefinition).withMeta(null));

        consume(':');

        IObj type = parseTypeRef();
        push(TYPE, type);

        return nodeWithLoc(
            name.meta().valAt(START),
            type.meta().valAt(END),
            pop(topIndex));
    }

    IObj parseTypeDefinition(IObj start, Keyword tag) {
        int topIndex = _stackTop;
        push(TAG, tag);
        documentComment();
        next(); // "type"
        Symbol name = parseName();
        push(NAME, name);

        if (TOKEN_IDENT == _token && "implements".equals(_image)) {
            next();
            int vecStart = _stackTop;
            do {
                push(parseBasicType());
            } while (TOKEN_IDENT == _token);
            push(IMPLEMENTS, popVec(vecStart));
        }

        IObj fields = parseVec('{', '}', this::parseTypeField);
        Object end = fields.meta().valAt(END);
        push(FIELDS, fields.withMeta(null));

        return nodeWithLoc(start, end, pop(topIndex));
    }

    IObj parseInterfaceDefinition() {
        int topIndex = _stackTop;
        Location start = startLocation();
        push(TAG, INTERFACE_DEFINITION);
        documentComment();
        next();

        push(NAME, parseName());
        IObj fields = parseVec('{', '}', this::parseTypeField);
        push(FIELDS, fields.withMeta(null));

        return nodeWithLoc(
            start, fields.meta().valAt(END),
            pop(topIndex));
    }

    IObj parseInputDefinition() {
        int topIndex = _stackTop;
        Location start = startLocation();
        push(TAG, INPUT_DEFINITION);
        documentComment();
        next(); // "input"
        push(NAME, parseName());
        IObj fields = parseVec('{', '}', this::parseTypeField);
        push(FIELDS, fields.withMeta(null));

        return nodeWithLoc(
            start, fields.meta().valAt(END),
            pop(topIndex));
    }

    IObj parseUnionDefinition() {
        int topIndex = _stackTop;
        Location start = startLocation();
        push(TAG, UNION_DEFINITION);
        documentComment();
        next(); // "union"
        push(NAME, parseName());
        consume('=');
        int vecStart = _stackTop;
        IObj lastType = parseBasicType();
        push(lastType);
        while ('|' == _token) {
            next();
            push(lastType = parseBasicType());
        }
        push(MEMBERS, popVec(vecStart));
        return nodeWithLoc(
            start, lastType.meta().valAt(END),
            pop(topIndex));
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
        throw expectedError("'query', 'mutation', or 'subscription'");
    }

    IObj parseSchemaType() {
        Location start = startLocation();
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
        Location start = startLocation();
        next(); // "schema"
        IObj members = parseVec('{', '}', this::parseSchemaType);
        return nodeWithLoc(
            start, members.meta().valAt(END),
            TAG, SCHEMA_DEFINITION,
            MEMBERS, members.withMeta(null)); // TODO: remove withMeta(null)
    }

    IObj parseEnumConstant() {
        int topIndex = _stackTop;
        push(TAG, ENUM_CONSTANT);
        documentComment();
        Symbol name = parseName();
        push(NAME, name);
        IObj directives = parseDirectives();
        IPersistentMap meta = name.meta();
        if (directives != null) {
            push(DIRECTIVES, directives.withMeta(null));
            meta = map(START, name.meta().valAt(START),
                       END, directives.meta().valAt(END));
        }

        return node(meta, pop(topIndex));
    }

    IObj parseEnumDefinition() {
        int topIndex = _stackTop;
        Location start = startLocation();
        push(TAG, ENUM_DEFINITION);
        documentComment();
        next(); // "enum"
        push(NAME, parseName());
        IObj constants = parseVec('{', '}', this::parseEnumConstant);
        push(CONSTANTS, constants.withMeta(null));

        return nodeWithLoc(
            start, constants.meta().valAt(END),
            pop(topIndex));
    }

    IObj parseTypeConditionOpt() {
        if (TOKEN_IDENT != _token || !"on".equals(_image))
            return null;

        Location start = startLocation();
        next();
        IObj type = parseBasicType();

        // TODO: for backwards compatilbity with the AST, the
        // type-condition's "on" is the start location.
        return type.withMeta(type.meta().assoc(START, start));
    }

    IObj parseDirectiveDefinition() {
        int topIndex = _stackTop;
        Location start = startLocation();
        next(); // "directive"
        expect('@');
        push(TAG, DIRECTIVE_DEFINITION);

        Symbol name = parseName();
        push(NAME, name);

        IObj on = parseTypeConditionOpt();
        if (on != null)
            push(ON, on);

        return nodeWithLoc(
            start, (on == null ? name : on).meta().valAt(END),
            pop(topIndex));
    }

    IObj parseExtendTypeDefinition() {
        Location start = startLocation();
        next(); // "extend"
        return parseTypeDefinition(start, EXTEND_TYPE_DEFINITION);
    }

    IObj parseScalarDefinition() {
        Location start = startLocation();
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
                return parseTypeDefinition(startLocation(), TYPE_DEFINITION);
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

        throw expectedError("'type', 'interface', 'union', 'schema', 'enum', 'input', "+
                         "'directive', 'extend', or 'scalar'");
    }

    private PersistentVector parseTypeSystemDefinitions() {
        while (_token != TOKEN_EOF)
            push(parseTypeSystemDefinition());
        return popVec(0);
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
            int topIndex = _stackTop;
            push(TAG, SELECTION_FIELD);

            Symbol name = parseName();
            Object start = name.meta().valAt(START);
            Symbol alias = null;
            if (':' == _token) {
                next();
                alias = name;
                name = parseName();
                push(ALIAS, alias);
            }
            push(NAME, name);

            Object end = name.meta().valAt(END);

            IObj arguments = null;
            if ('(' == _token) {
                arguments = parseVec('(', ')', this::parseArgument);
                end = arguments.meta().valAt(END);
                push(ARGUMENTS, arguments.withMeta(null));
            }

            IObj directives = parseDirectives();
            if (directives != null) {
                end = directives.meta().valAt(END);
                push(DIRECTIVES, directives);
            }

            IObj sset = null;
            if ('{' == _token) {
                sset = parseSelectionSet();
                end = sset.meta().valAt(END);
                push(SELECTION_SET, sset.withMeta(null));
            }
            return nodeWithLoc(start, end, pop(topIndex));
        } else if (TOKEN_ELLIPSIS == _token) {
            int topIndex = _stackTop;
            push(TAG, null); // null replaced later

            Location start = startLocation();
            next();
            IObj on = null;
            if (TOKEN_IDENT == _token) {
                if ("on".equals(_image)) {
                    // TODO: convert this if block to parseTypeCondition
                    // note: that this is the correct way to populate `on`
                    on = parseTypeConditionOpt();
                    push(ON, on);
                } else {
                    Symbol name = parseName();
                    push(NAME, name);

                    IObj directives = parseDirectives();
                    if (directives != null)
                        push(DIRECTIVES, directives);

                    _stack[topIndex+1] = FRAGMENT_SPREAD;

                    return nodeWithLoc(
                        start, (directives != null ? directives : name).meta().valAt(END),
                        pop(topIndex));
                }
            }

            _stack[topIndex+1] = INLINE_FRAGMENT;

            IObj directives = parseDirectives();
            if (directives != null)
                push(DIRECTIVES, directives);

            IObj sset = parseSelectionSet();
            push(SELECTION_SET, sset.withMeta(null));

            return nodeWithLoc(start, sset.meta().valAt(END), pop(topIndex));
        } else {
            throw expectedError("field name or '...'");
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
        Location start = startLocation();
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

    // exposed for testing
    public IObj parseValue() {
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
            value = _stringValue.toString();
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
            throw expectedError("value");
        }

        Location start = startLocation();
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
        Location start = startLocation();
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

        Location firstStart = startLocation();
        Object lastEnd;
        int vecStart = _stackTop;
        do {
            int topIndex = _stackTop;
            push(TAG, DIRECTIVE);
            Location start = startLocation();
            next(); // '@'
            Symbol name = parseName();
            push(NAME, name);

            Object end;
            IObj arguments = null;
            if ('(' == _token) {
                arguments = parseVec('(', ')', this::parseArgument);
                end = arguments.meta().valAt(END);
                push(ARGUMENTS, arguments.withMeta(null));
            } else {
                end = name.meta().valAt(END);
            }

            push(nodeWithLoc(start, lastEnd = end, pop(topIndex)));
        } while ('@' == _token);

        return popVec(vecStart).withMeta(map(START, firstStart, END, lastEnd));
    }

    private IObj parseOperationDefinition(Keyword tag) {
        int topIndex = _stackTop;
        int i = 0;
        push(TAG, tag);

        Location start = startLocation();
        next(); // "query" or "mutation"
        Symbol name = null;
        if (TOKEN_IDENT == _token) {
            name = parseName();
            push(NAME, name);
        }
        // optional
        IObj varDefs = null;
        if ('(' == _token) {
            varDefs = parseVec('(', ')', this::parseVariableDefinition);
            push(VARIABLE_DEFINITIONS, varDefs.withMeta(null));
        }

        IObj directives = parseDirectives();
        if (directives != null)
            push(DIRECTIVES, directives);

        IObj sset = parseSelectionSet();
        push(SELECTION_SET, sset.withMeta(null));

        return nodeWithLoc(start, sset.meta().valAt(END), pop(topIndex));
    }

    private IObj parseFragmentDefinition() {
        final int topIndex = _stackTop;
        push(TAG, FRAGMENT_DEFINITION);
        Location start = startLocation();
        next(); // "fragment"
        Symbol name = parseName();
        push(NAME, name);

        IObj on = parseTypeConditionOpt();
        if (on != null)
            push(ON, on);

        IObj directives = parseDirectives();
        if (directives != null)
            push(DIRECTIVES, directives);

        IObj sset = parseSelectionSet();
        push(SELECTION_SET, sset.withMeta(null));
        return nodeWithLoc(
            start, sset.meta().valAt(END),
            pop(topIndex));
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
        throw expectedError("'{', 'query', 'mutation', or 'fragment'");
    }

    public IObj parseQueryDocument() {
        while (_token != TOKEN_EOF)
            push(parseQueryElement());

        return popVec(0).withMeta(
            map(START, new Location(1, 1, 0),
                END, location(_index)));
    }
}
