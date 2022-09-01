using System;
using System.Collections.Generic;
using System.Xml.XPath;

public abstract class Expr
{
    public abstract override string ToString();
    public abstract int eval(Dictionary<string, int> env);

    public abstract Expr simplify();
}

public class CstI : Expr
{
    public int value { get; set; }

    public override string ToString()
    {
        return value.ToString();
    }

    public override int eval(Dictionary<string, int> env)
    {
        return value;
    }

    public override Expr simplify()
    {
        return new CstI(value);
    }

    public CstI(int i)
    {
        value = i;
    }
}

public class Var : Expr
{
    private string value;

    public override string ToString()
    {
        return value;
    }

    public override int eval(Dictionary<string, int> env)
    {
        try
        {
            return env[value];
        }
        catch (Exception e)
        {
            throw e;
        }
    }

    public override Expr simplify()
    {
        return new Var(value);
    }

    public Var(string i)
    {
        value = i;
    }
}

public abstract class Binop : Expr   
{
    
}

public class Add : Binop
{
    private Tuple<Expr, Expr> value;
    public Add(Expr r, Expr l)
    {
        value = new Tuple<Expr, Expr>(r, l);
    }

    public override string ToString()
    {
        return value.Item1 + " + " + value.Item2;
    }

    public override int eval(Dictionary<string, int> env)
    {
        return value.Item1.eval(env) + value.Item2.eval(env);
    }

    public override Expr simplify()
    {
        var left = value.Item1.simplify();
        var right = value.Item2.simplify();
        
        if (left is CstI i)
        {
            if (i.value == 0)
            {
                return right;
            }
           
        } 
        else if (right is CstI {value: 0})
        {
            return left;
        }
        return new Add(value.Item1, value.Item2);
    }
}

public class Sub : Binop
{
    private Tuple<Expr, Expr> value;
    public Sub(Expr r, Expr l)
    {
        value = new Tuple<Expr, Expr>(r, l);
    }

    public override string ToString()
    {
        return value.Item1 + " - " + value.Item2;
    }

    public override int eval(Dictionary<string, int> env)
    {
        return value.Item1.eval(env) - value.Item2.eval(env);
    }

    public override Expr simplify()
    {
        var left = value.Item1.simplify();
        var right = value.Item2.simplify();
        if (right is CstI il && left is CstI r)
        {
            if (il == r)
            {
                return new CstI(0);
            }
        }
        else if (right is CstI i)
        {
            if (i.value == 0)
            {
                return left;
            }
           
        } 

        return new Sub(left, right);
        
    }
}

public class Mul : Binop
{
    private Tuple<Expr, Expr> value;
    public Mul(Expr r, Expr l)
    {
        value = new Tuple<Expr, Expr>(r, l);
    }

    public override string ToString()
    {
        return value.Item1 + " * " + value.Item2;
    }

    public override int eval(Dictionary<string, int> env)
    {
        return value.Item1.eval(env) * value.Item2.eval(env);
    }

    public override Expr simplify()
    {
        
        if (value.Item1.Equals(new CstI(1)))
        {
            return value.Item2;
        } 
        else if (value.Item2.Equals(new CstI(1)))
        {
            return value.Item1;
        }
        else if (value.Item1.Equals(new CstI(0)) || value.Item2.Equals(new CstI(0)))
        {
            return new CstI(0);
        }
        else
        {
            return new Mul(value.Item1, value.Item2);
        }
    }
}