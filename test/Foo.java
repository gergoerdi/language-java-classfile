import java.io.*;

public class Foo extends Object implements Comparable<Foo>
{
    public class Bar
    {
        protected class Quux
        {
        }
    }

    interface IBaz
    {
        class Blargh
        {
        };
        
        int foo ();
    }

    IBaz getBaz ()
    {
        return new IBaz() {
            public int foo () { return 1;};
        };
    }
    
    Bar getBar ()
    {
        return null;
    }
    
    static final int x = 42;

    public int doFoo () throws IOException
    {
        return 0;
    }
    
    public int compareTo (Foo x)
    {
        return 0;
    }
}
