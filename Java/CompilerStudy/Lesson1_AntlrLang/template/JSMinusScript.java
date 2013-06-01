import java.util.*;

public class JSMinusScript {
    static HashMap<String, Object> s_globals;

    static Object print(ArrayList args) {
        for (Object arg : args) System.out.println(arg);
        return null;
    }
    static Object println(ArrayList args) {
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
    static Object push(ArrayList a, Object v) {
        a.add(v);
        return a;
    }
    static Object format(String fmt, ArrayList args) {
        return String.format(fmt, args.toArray());
    }
}
