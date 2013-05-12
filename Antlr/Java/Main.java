import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;
 
public class Main {
    public static void main(String[] args) throws Exception {
        ANTLRInputStream input = new ANTLRInputStream(System.in);
        ExprLexer lexer = new ExprLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        ExprParser parser = new ExprParser(tokens);
        CommonTreeNodeStream astStream = new CommonTreeNodeStream(parser.prog().getTree());
        new TreeExpr(astStream).prog();
    }
}
