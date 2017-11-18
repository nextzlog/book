# Scalaで学ぶプログラミング言語の作り方
## programming language fava/elva

- fava is a small functional programming language based on lambda calculus
- elva is a tiny LISP interpretor naturally implemented on Scala ecosystem

## details

- see http://pafelog.net/fava.pdf

## build

`$ gradle build`

## usage

```
$ java -jar build/libs/fava.jar
which language do you use?
[1]: fava
[2]: elva
select: 1
fava$
```

## fava example

### lambda calculus

```
fava$ ((x)=>(y)=>3*x+7*y)(2)(3)
27
```

### Church encoding

```
fava$ ((x)=>(y)=>x(y)((x)=>(y)=>y))((x)=>(y)=>x)((x)=>(y)=>y)(true)(false)
false
fava$ ((x)=>(y)=>x((x)=>(y)=>x)(y))((x)=>(y)=>x)((x)=>(y)=>y)(true)(false)
true
fava$ ((a)=>(b)=>(f)=>(x)=>a(f)(b(f)(x)))((f)=>(x)=>f(x))((f)=>(x)=>f(f(x)))((x)=>x+1)(0)
3
fava$ ((a)=>(b)=>(f)=>(x)=>a(b(f))(x))((f)=>(x)=>f(f(x)))((f)=>(x)=>f(f(x)))((x)=>x+1)(0)
4
```

### anonymous recursion

```
fava$ ((f)=>((x)=>f((y)=>x(x)(y)))((x)=>f((y)=>x(x)(y))))((f)=>(n)=>(n==0)?1:n*f(n-1))(10)
3628800
fava$ ((f)=>((x)=>f((y)=>x(x)(y)))((x)=>f((y)=>x(x)(y))))((f)=>(n)=>(n<2)?n:f(n-1)+f(n-2))(10)
55
```

## elva example

### name space

```
elva$ (set ’function-in-variable list)
<function>
elva$ (function-in-variable 1 2 3 4 5)
(1 2 3 4 5)
```

### lambda definition

```
elva$ (define-lambda fact (x) (if (eq x 1) x (* x (fact (- x 1)))))
(lambda (x) (if (eq x 1) x (* x (fact (- x 1)))))
```

### syntax definition

```
elva$ define-lambda
(syntax (name pars body) (list (quote setq) name (list (quote lambda) pars body)))
elva$ define-syntax
(syntax (name pars body) (list (quote setq) name (list (quote syntax) pars body)))
```
