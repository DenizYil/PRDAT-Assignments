using System;
using System.Collections.Generic;

//Put into C# Interactive environment and run Main() to test :)
void Main()
{
    Expr e = new Add(new CstI(17), new Var("z"));
    Expr e1 = new Mul(new CstI(17), new Var("z"));
    Expr e2 = new Sub(new CstI(17), new Var("z"));
    Expr e3 = new Add(new CstI(17), new CstI(22832472));
    Expr e4 = new Add(new CstI(0), new CstI(22832472));
    Console.WriteLine(e.ToString());
    Console.WriteLine(e1.ToString());
    Console.WriteLine(e2.ToString());
    Console.WriteLine(e3.ToString());
}

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
        
        var left = value.Item1.simplify();
        var right = value.Item2.simplify();
        
        if (left is CstI i)
        {
            if (i.value == 1)
            {
                return right;
            }
            else if (i.value == 0)
            {
                return new CstI(0);
            }
        } 
        
        if (right is CstI rl)
        {
            if (rl.value == 1)
            {
                return left;
            }
            else if (rl.value == 0)
            {
                return new CstI(0);
            }
        }
        return new Mul(left, right);
        
    }
}