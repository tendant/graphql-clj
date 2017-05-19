package graphql_clj;

import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.Arrays;
import java.util.Collections;

/**
 * Location is a replacement for using a PersistentMap implementation
 * directly.  Location values are rarely referenced in practice, since
 * they are primarily used in error messages that should be found and
 * fixed.  This delays construction of Integer boxes when the location
 * values is not referenced.  This speeds up parsing in some
 * experiments by 30%.
 */
public class Location extends AbstractRecord {
    private static final Keyword LINE = Keyword.intern("line");
    private static final Keyword COLUMN = Keyword.intern("column");
    private static final Keyword INDEX = Keyword.intern("index");

    private static final Set<Keyword> KEY_SET = Collections.unmodifiableSet(
        new TreeSet<>(Arrays.asList(LINE, COLUMN, INDEX)));

    private final int _line;
    private final int _column;
    private final int _index;
    
    public Location(int line, int column, int index) {
        _line = line;
        _column = column;
        _index = index;
    }

    public int getIndex() {
        return _index;
    }

    @Override
    public Set<Keyword> keySet() {
        return KEY_SET;
    }

    @Override
    protected PersistentArrayMap toMap(IPersistentMap meta) {
        return new PersistentArrayMap(
            meta, new Object[] {
                LINE, _line,
                COLUMN, _column,
                INDEX, _index
            });
    }

    @Override
    public Object valAt(Object key, Object notFound) {
        if (LINE == key) {
            return _line;
        } else if (COLUMN == key) {
            return _column;
        } else if (INDEX == key) {
            return _index;
        } else {
            return notFound;
        }
    }
}

/*
______________________________________________________________________
BEFORE adding Location

Java: "Elapsed time: 49.532082 msecs"
Java: "Elapsed time: 4.228468 msecs"
Java: "Elapsed time: 3.962028 msecs"
Java: "Elapsed time: 3.675353 msecs"
Java: "Elapsed time: 6.498892 msecs"
Java: "Elapsed time: 3.791992 msecs"
Java: "Elapsed time: 3.101611 msecs"
Java: "Elapsed time: 3.314961 msecs"
Java: "Elapsed time: 3.168092 msecs"
Java: "Elapsed time: 3.188221 msecs"

______________________________________________________________________
AFTER adding Location

Java: "Elapsed time: 48.400449 msecs"
Java: "Elapsed time: 3.650806 msecs"
Java: "Elapsed time: 3.104886 msecs"
Java: "Elapsed time: 2.725937 msecs"
Java: "Elapsed time: 5.226762 msecs"
Java: "Elapsed time: 2.905882 msecs"
Java: "Elapsed time: 2.478088 msecs"
Java: "Elapsed time: 2.783301 msecs"
Java: "Elapsed time: 2.498166 msecs"
Java: "Elapsed time: 2.464393 msecs"

Java: "Elapsed time: 48.960953 msecs"
Java: "Elapsed time: 3.629612 msecs"
Java: "Elapsed time: 2.967359 msecs"
Java: "Elapsed time: 2.589153 msecs"
Java: "Elapsed time: 2.598267 msecs"
Java: "Elapsed time: 2.762551 msecs"
Java: "Elapsed time: 2.821136 msecs"
Java: "Elapsed time: 2.425323 msecs"
Java: "Elapsed time: 2.358258 msecs"
Java: "Elapsed time: 2.349141 msecs"



*/
