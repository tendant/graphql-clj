package graphql_clj;

import clojure.lang.IObj;

public class ParseException extends RuntimeException {
    final IObj _location;
    
    public ParseException(IObj location, String msg) {
        super(msg);
        _location = location;
    }

    public IObj location() {
        return _location;
    }
}
