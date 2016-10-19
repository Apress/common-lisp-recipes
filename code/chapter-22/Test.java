import org.armedbear.lisp.*;

public class Test {
  public static void main(String arg[]) {
    Interpreter interpreter = Interpreter.createInstance();
    interpreter.eval("(load \"hello.lisp\")");
    Function hello = (Function) Packages.findPackage("CL-USER")
                                        .findAccessibleSymbol("HELLO")
                                        .getSymbolFunction();
    LispObject result = hello.execute(new SimpleString("ABCL"));
    System.out.print(result.princToString());
  }
}
