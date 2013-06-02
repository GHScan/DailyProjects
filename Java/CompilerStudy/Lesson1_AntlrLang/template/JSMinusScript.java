import java.util.*;

public class JSMinusScript {
    static HashMap<String, Object> s_globals;

    static Object print(Object args) {
        for (Object arg : (ArrayList)args) System.out.print(arg + "\t");
        return null;
    }
    static Object println(Object args) {
        print(args);
        System.out.println("");
        return null;
    }
    static Object clock() {
        return System.currentTimeMillis() / 1000.0;
    }
    static Object random() {
        return (int)(Math.random() * (1 << 24));
    }
    @SuppressWarnings("unchecked")
    static Object push(Object a, Object v) {
        ((ArrayList)a).add(v);
        return a;
    }
    static Object format(Object fmt, Object args) {
        return String.format((String)fmt, ((ArrayList)args).toArray());
    }
    static boolean __equals(Object a, Object b) {
        if (a == null) return a == b;
        else return a.equals(b);
    }
    static boolean __nequals(Object a, Object b) {
        return !__equals(a, b);
    }
}
