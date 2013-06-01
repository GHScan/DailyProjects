import java.util.*;
import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
 
public class Main {
    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            System.out.println("Usage : JSMinus file");
            return;
        }

        ANTLRFileStream input = new ANTLRFileStream(args[0]);
        JSMinusLexer lexer = new JSMinusLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        JSMinusParser parser = new JSMinusParser(tokens);
        CodeEmitor emitor = new CodeEmitor(parser.program());
        emitor.save();
        emitor.run();
    }
}
