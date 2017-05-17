package graphql_clj;

import clojure.lang.Associative;
import clojure.lang.IEditableCollection;
import clojure.lang.IFn;
import clojure.lang.IKVReduce;
import clojure.lang.ILookup;
import clojure.lang.IMapEntry;
import clojure.lang.IMapIterable;
import clojure.lang.IObj;
import clojure.lang.IPersistentCollection;
import clojure.lang.APersistentMap;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.ITransientCollection;
import clojure.lang.Keyword;
import clojure.lang.PersistentArrayMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.Arrays;

/**
 * Abstract parent class of typed java objects meant to be used
 * natively in clojure.  The main advantage of using this class is
 * that it can save a lot of time for objects that are unlikely to
 * modified, or have primitive members that are unlikely to be
 * referenced (and thus can remain unboxed).
 */
public abstract class AbstractRecord extends APersistentMap
    implements IObj, IEditableCollection, IMapIterable, IKVReduce
{
    /**
     * Cached persistent map created ondemand when the record is
     * modified.
     */
    private volatile PersistentArrayMap _map;

    private PersistentArrayMap toMap() {
        // Since we're using an unsynchornized cache object, we may
        // end up with more than one copy before we settle on a single
        // one that is reused.
        PersistentArrayMap map = _map; // volatile read
        if (map == null) {
            map = toMap(null);
            _map = map; // volatile write
        }
        return map;
    }

    /**
     * Subclasses override to return an unmodifiable set of Keywords.
     */
    public abstract Set<Keyword> keySet();

    /**
     * Subclasses override to return an extensible persistent map that
     * is used when the object is "modified".
     */
    protected abstract PersistentArrayMap toMap(IPersistentMap meta);

    /**
     * Subclasses override to implement ILookup.
     */
    public abstract Object valAt(Object key, Object notFound);
    
    @Override
    public IPersistentMap meta() {
        return null;
    }

    @Override
    public IObj withMeta(IPersistentMap meta) {
        return (null == meta) ? this : toMap(meta);
    }
    
    @Override
    public Object valAt(Object key) {
        return valAt(key, null);
    }

    @Override
    public IPersistentMap without(Object key) {
        return toMap().without(key);
    }

    @Override
    public IPersistentMap assoc(Object key, Object val) {
        return toMap().assoc(key, val);
    }

    @Override
    public IPersistentMap assocEx(Object key, Object val) {
        return toMap().assocEx(key, val);
    }

    @Override
    public Iterator iterator() {
        return toMap().iterator();
    }

    @Override
    public IMapEntry entryAt(Object key) {
        return toMap().entryAt(key);
    }

    @Override
    public IPersistentCollection empty() {
        return toMap().empty();
    }

    @Override
    public ISeq seq() {
        return toMap().seq();
    }

    @Override
    public ITransientCollection asTransient() {
        return toMap().asTransient();
    }

    @Override
    public Iterator keyIterator() {
        return toMap().keyIterator();
    }

    @Override
    public Iterator valIterator() {
        return toMap().valIterator();
    }

    @Override
    public Object kvreduce(IFn f, Object init) {
        return toMap().kvreduce(f, init);
    }

    @Override
    public boolean containsKey(Object key) {
        return keySet().contains(key);
    }

    @Override
    public int count() {
        return keySet().size();
    }
}
