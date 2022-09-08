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
    public abstract int Eval(Dictionary<string, int> env);
    public abstract Expr Simplify();
}

public class CstI : Expr
{
    public int Value { get; init; }

    public CstI(int i)
    {
        Value = i;
    }

    public override string ToString()
    {
        return Value.ToString();
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return Value;
    }

    public override Expr Simplify()
    {
        return new CstI(Value);
    }
}

public class Var : Expr
{
    private readonly string _value;

    public Var(string i)
    {
        _value = i;
    }

    public override string ToString()
    {
        return _value;
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return env[_value];
    }

    public override Expr Simplify()
    {
        return new Var(_value);
    }
}

public abstract class Binop : Expr
{
}

public class Add : Binop
{
    private readonly Tuple<Expr, Expr> _value;

    public Add(Expr r, Expr l)
    {
        _value = new Tuple<Expr, Expr>(r, l);
    }

    public override string ToString()
    {
        return $"{_value.Item1} + {_value.Item2}";
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return _value.Item1.Eval(env) + _value.Item2.Eval(env);
    }

    public override Expr Simplify()
    {
        var left = _value.Item1.Simplify();
        var right = _value.Item2.Simplify();

        if (left is CstI {Value: 0})
        {
            return right;
        }
        
        if (right is CstI {Value: 0})
        {
            return left;
        }

        return new Add(left, right);
    }
}

public class Sub : Binop
{
    private readonly Tuple<Expr, Expr> _value;

    public Sub(Expr r, Expr l)
    {
        _value = new Tuple<Expr, Expr>(r, l);
    }

    public override string ToString()
    {
        return $"{_value.Item1} - {_value.Item2}";
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return _value.Item1.Eval(env) - _value.Item2.Eval(env);
    }

    public override Expr Simplify()
    {
        var left = _value.Item1.Simplify();
        var right = _value.Item2.Simplify();

        if (right is CstI il && left is CstI r)
        {
            if (il == r)
            {
                return new CstI(0);
            }
        }
        else if (right is CstI {Value: 0})
        {
            return left;
        }

        return new Sub(left, right);
    }
}

public class Mul : Binop
{
    private readonly Tuple<Expr, Expr> _value;

    public Mul(Expr r, Expr l)
    {
        _value = new Tuple<Expr, Expr>(r, l);
    }

    public override string ToString()
    {
        return $"{_value.Item1} * {_value.Item2}";
    }

    public override int Eval(Dictionary<string, int> env)
    {
        return _value.Item1.Eval(env) * _value.Item2.Eval(env);
    }

    public override Expr Simplify()
    {
        var left = _value.Item1.Simplify();
        var right = _value.Item2.Simplify();

        if (left is CstI i)
        {
            switch (i.Value)
            {
                case 1:
                    return right;
                case 0:
                    return new CstI(0);
            }
        }

        if (right is CstI rl)
        {
            switch (rl.Value)
            {
                case 1:
                    return left;
                case 0:
                    return new CstI(0);
            }
        }

        return new Mul(left, right);
    }
}