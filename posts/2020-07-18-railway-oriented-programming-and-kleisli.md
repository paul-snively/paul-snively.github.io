---
title:  Railway-Oriented Programming and Kleisli
---

Recently, an article on [Railway-Oriented Programming in Scala](https://blog.pjam.me/posts/railway-oriented-programming-scala/) appeared and was [linked to](https://www.reddit.com/r/scala/comments/ho33fy/railway_oriented_programming_in_scala/) on the Scala subreddit. It, in turn, is based on [this post](https://fsharpforfunandprofit.com/rop/), which presents the concept in the context of the F# language. In the Scala subreddit thread, I observed that ROP is a great example of the power of typed functional programming, and is a reasonably well-known pattern that goes by a different (and, let’s be honest, less approachable) name in languages and/or libraries that offer the more general form. That’s what I’d like to talk about here. First, though, please go read the other articles. They’re very good treatments of and motivators for the concept, so I won’t recapitulate them. Rather, I’ll begin by discussing what the point of ROP is, how Scala offers language features making the more general approach practical, and how ROP fits into the bigger typed functional programming picture.

Back from reading the articles? Good.

## What’s the point?

So, what is ROP about? I’ll claim it’s about the following

1. ROP is about function composition.

I imagine most Scala programmers have some intuition that if you have something like:

```scala
def fun1(a: A): B
```

and

```scala
def fun2(b: B): C
```

you can, of course:

```scala
fun2(fun1(a))
```

and get a `C`. But maybe fewer Scala programmers know you can say the same thing in the following way:

```scala
val fun1: A => B = ???
val fun2: B => C = ???
(fun1 andThen fun2)(a)
```

As the Scala-based article notes, in Scala, methods and functions are different. Methods are defined with `def`. While most Scala developers have probably seen the `=>` syntax, it might not be obvious that `A => B` is syntactic sugar for [`Function1[A, B]`](https://www.scala-lang.org/api/current/scala/Function1.html). Whether you say `fun2(fun1(a))` or `(fun1 andThen fun2)(a)`, we call this “functional composition.” It’s what we’re trying to do when we do “functional programming.”

But there’s a hitch, isn’t there? As the articles point out, most useful functions in most domains have some kind of _effect_. And it turns out you have to be careful how and when effects in a function happen if you want functions to “make sense” when you compose them. You want:

```scala
f andThen g andThen h andThen i...
```

to “work the way you expect” no matter what `f`, `g`, `h`, `i`… do, if their types line up. So:

2. ROP is about taking effects into account.

You know what’s coming, right? Yes, I’m going to talk about monads.

Relax. You use monads in Scala _constantly_. In fact, you use monads literally every time you write a `for`-comprehension. The Scala-based article notes that porting the F# code for `Result` to Scala ends up yielding (no pun intended) something a lot like Scala’s [`Either`](https://www.scala-lang.org/api/current/scala/util/Either.html), which is often used to represent _either_ a success (`Right`, by convention, in Scala) or failure (`Left`). And Scala developers probably know you can use `Either` in `for`-comprehensions:

```scala
for {
  a <- Left[String, Int]("Something bad happened.")
  b <- Right[String, Int](42 + a)
} yield b
res0: Either[String, Int] = Left("Something bad happened.")
```

So `Either` is a monad, because it works in `for`-comprehensions. And it’s a monad that, again as the Scala-based article points out, is often used to model “operations that can fail,” like validating user input, or writing to a database. So I’ll make a third claim:

3. ROP is about taking failure into account.</li>

But there’s another hitch, isn’t there? We’ve lost the “railway” notion along the way. There’s something appealing about the simplicity of:

```scala
f andThen g andThen h andThen i...
```

But if:

```scala
val f: A => Either[String, B] = ???
val g: B => Either[String, C] = ???
val h: C => Either[String, D] = ???
val i: D => Either[String, E] = ???
```

then the “railway” version of their composition won’t even compile, because the arguments aren’t `Either`s. But the `for`-comprehension works:

```scala
def compose(a: A): Either[String, E] = for {
  b <- f(a)
  c <- g(b)
  d <- h(c)
  e <- i(d)
} yield e
```

The reason is that all `for`-comprehensions care about is that what’s to the right of the `<-` is a monad. Because we’re applying all of the functions to their arguments manually, we have a monad—an `Either`—on the right, so the comprehension works. Another important aspect of the reason it works is that monads have constructors, a `map` method, and a `flatMap` method that, together, obey certain laws, so `for`-comprehensions are just syntactic sugar for a chain of `flatMap`s followed by a `map` for the `yield` keyword. This point will be important later. The point is, it’d be really nice if there were some way to provide `andThen` for `Function1`s returning monads, especially monads we use to represent failure.

It turns out there is, in a library called [Cats](https://typelevel.org/cats/).

First, Cats helps us address the quandary I alluded to a moment ago: we want something that acts a lot like a `for`-comprehension, but we’d like to just compose functions with something like `andThen`, and this seems out of reach because an `A` is not an `Either[E, A]`. The key observation above is that `for`-comprehensions “know” they’re dealing with a monad: if the type you try to use doesn’t have `map` and `flatMap`, your `for`-comprehension won’t compile. Is there some more general way to let the compiler “know” a type `T` is a monad?

It turns out there is: we can say `Monad[T]`, thanks to Cats.

But there is, again, a hitch: `T` isn’t really a type, but a type _constructor_. To use another example, `Option` isn’t a type. It’s a type constructor that takes one argument, the type of its value, if it exists. So `Monad`, from Cats, is a type constructor that takes another type constructor as an argument, and the result is a type, _regardless of the type argument of the nested type constructor_. This is a feature that distinguishes Scala’s type system from F#’s type system, and is the reason this post exists. We call these [higher-kinded types](https://typelevel.org/blog/2016/08/21/hkts-moving-forward.html). Again, don’t panic: “kind” just means “type of a type,” so “higher-kinded type” just means “type of a type of a type,” and honestly, you’ll almost certainly never have to think about that phrase again, even if you use “higher-kinded types” every day, like I do.

## Higher-Kinded Types and Typeclasses

I’m not going to dwell on the formalities of higher-kinded types. The link above does a much better job of that than I could ever hope to. What I want to do instead is just lay down the intuition, _which you already have from `for`-comprehensions_, that a higher-kinded type just makes _explicit_ something about a type constructor that we already know _implicitly_. We know `Option` is a monad, because we can use it in a `for`-comprehension, which means an `Option[A]` has a constructor, `map`, and `flatMap` obeying the monad laws. Here’s the thing: do the constructor, `map`, and `flatMap` know anything at all about `A`? No. They don’t. The type `Monad[Option]`—and remember, this _is_ a type, not a type _constructor_, because it’s a higher-kinded type—captures this. `Option` is a `Monad` _for all_, as the logicians say, `A` in `Option[A]`. You may think, especially if you have a Java background, that this sounds like `Option` could _inherit_ from `Monad`, because I keep saying “`Option` is-a `Monad`.” And in traditional object-oriented programming, you’d be right. But Cats isn’t the standard library; `Option` _is_ in the standard library; and besides, the problems with modeling “is-a” relationships with implementation inheritance are well-known even to object-oriented programmers. So what we usually call this use-pattern of higher-kinded types, especially when they obey a set of algebraic laws, is a “typeclass,” a term that comes from the purely-functional programming language Haskell. I’m not going to dwell on their formalities either. I’m just going to say “typeclass” from now on, instead of the mouthful “higher-kinded type obeying a set of algebraic laws,” or, worse, “type of a type of a type obeying a set of algebraic laws.” And now maybe you can begin to see why some of the jargon exists.

## Can I Get a Witness?

Speaking of jargon, here’s some more: we’ve established that `Monad[Option]` is a perfectly good type, even though `Option`, by itself, is a type _constructor_. We’ve also established that `Option` doesn’t inherit from `Monad`. So what do we call a value of type `Monad[Option]`? I’m afraid we still call it an “instance” of the _typeclass_. Now I want to ask you to make a _big_ mental leap:

<p style="text-align: center;">
Read `Monad[Option]` as “`Option` implies `Monad`.”
</p>

In fact, you can think of _all_ types as propositions (that’s the Curry-Howard-Lambek correspondence, which I talked about [here](https://www.youtube.com/watch?v=dj7LcTAK8ow), but that’s out of scope for this post). But let’s stay focused on `Monad[Option]` for now. How do you know whether the proposition “`Option` implies `Monad`” is true or not? It’s true if, and only if, there is at least one value of that type. Now let me dive into how that works in Cats in practice. First, I’ll fire up Li Haoyi’s wonderful [Ammonite REPL](http://ammonite.io/#Ammonite-REPL) to noodle around in (as I have actually been doing all along):

```scala
psnively@oryx-pro:~|⇒  amm
Loading...
Welcome to the Ammonite Repl 2.1.4 (Scala 2.13.2 Java 11.0.7)
@  
```

One of the great things about the Ammonite REPL is its “magic imports” feature. It lets us import libraries directly from the usual repositories. Because I’m going to talk about effects more later, let me go ahead and import the cats-effect library:

```scala
@ import $ivy.`org.typelevel::cats-effect:2.1.4`
https://repo1.maven.org/maven2/org/typelevel/cats-effect_2.13/2.1.4/cats-effect_2.13-2.1.4.pom
  100.0% [##########] 2.5 KiB (4.0 KiB / s)
https://repo1.maven.org/maven2/org/typelevel/cats-effect_2.13/2.1.4/cats-effect_2.13-2.1.4-sources.jar
  100.0% [##########] 142.5 KiB (598.9 KiB / s)
https://repo1.maven.org/maven2/org/typelevel/cats-effect_2.13/2.1.4/cats-effect_2.13-2.1.4.jar
  100.0% [##########] 1.1 MiB (3.0 MiB / s)
import $ivy.$

@  
```

Now we import what we need from Cats. Usually, Cats developers do whole-package imports like this:

```scala
@ import cats._, implicits._
import cats._, implicits._
```

That gets us a lot of stuff that’s native to Cats, which is great. But we also want Cats typeclass instances for standard library types like `Option`. Those live in a different package:

```scala
@ import cats.instances._
import cats.instances._
```

When we want instances of a typeclass, we usually want them to be “implicitly” in scope, because we want, in a broad sense, to know that the proposition their type represents is true. The value of type `Monad[Option]`—and ideally, there is only one—is sometimes called “evidence” or a “witness.” In fact, you may already have seen code like:

```scala
def doSomething[F[_]](arg: Whatever)(implicit ev: Monad[F]) = {
  ...
}
```

and wondered what “`ev`” meant. It’s short for “evidence.” Here, we’re saying we don’t care what type _constructor_ `F` actually is, or what its type argument is, as long as there’s implicit _evidence_ it’s a `Monad`. Do we have that evidence for `Monad[Option]`?

```scala
@ implicitly[Monad[Option]]
res4: Monad[Option] = cats.instances.OptionInstances$$anon$1@5d3634c8
```

We do. Importing `cats.instances._` has brought a `Monad[Option]` implicitly in scope. OK, the compiler now knows `Option` implies `Monad`. So what? Well, types define legal operations on terms. So a _value_ (“term” is Programming Language Theory jargon I won’t dig into further here) of a type has certain legal operations. Remember, a `Monad[Option]` is not an `Option`; it defines operations _any_ `Monad` supports. What are some of them?

```scala
@ res4. 
*>                                asRight                           lift                              productL                          tuple3
<*                                compose                           map                               productLEval                      tuple4
<*>                               composeApply                      map10                             productR                          tuple5
ap                                composeContravariant              map11                             productREval                      tuple6
ap10                              composeContravariantMonoidal      map12                             pure                              tuple7
ap11                              composeFunctor                    map13                             raiseError                        tuple8
ap12                              flatMap                           map14                             replicateA                        tuple9
ap13                              flatTap                           map15                             rightIor                          tupleLeft
ap14                              flatten                           map16                             rightNec                          tupleRight
ap15                              fmap                              map17                             rightNel                          unit
ap16                              foreverM                          map18                             some                              unlessA
ap17                              fproduct                          map19                             tailRecM                          untilDefinedM
ap18                              ifA                               map2                              tell                              untilM
ap19                              ifF                               map20                             tuple10                           untilM_
ap2                               ifM                               map21                             tuple11                           unzip
ap20                              imap                              map22                             tuple12                           valid
ap21                              invalid                           map2Eval                          tuple13                           validNec
ap22                              invalidNec                        map3                              tuple14                           validNel
ap3                               invalidNel                        map4                              tuple15                           void
ap4                               iterateForeverM                   map5                              tuple16                           whenA
ap5                               iterateUntil                      map6                              tuple17                           whileM
ap6                               iterateUntilM                     map7                              tuple18                           whileM_
ap7                               iterateWhile                      map8                              tuple19                           widen
ap8                               iterateWhileM                     map9                              tuple2                            writer
ap9                               leftIor                           mproduct                          tuple20
as                                leftNec                           point                             tuple21
asLeft                            leftNel                           product                           tuple22
@ res4. 
```

As you can see, there’s a _lot_ you can do with a `Monad`. But for all that, there’s no `andThen`. And that makes sense. A `Monad[Option]` isn’t a `Function1[A, Option[B]]`, and here we can see the problem pretty clearly. We need some type that, if possible, represents a monadic function like our `val f: A => Either[String, B]`. But first, let’s deal with another hitch: `Option` takes one type argument, but `Either` takes two. Also, `Either` explicitly represents failure with one of its type arguments. Is there a typeclass that accounts for that?

It turns out there is: `MonadError`. `MonadError` takes two type arguments, a type _constructor_ taking _one_ type argument, and a type representing error values. You see the problem: you can’t just use `Either` as the first type argument for `MonadError` because it takes _two_ type arguments. It’s significant, though, that you want to use the _same type_ for the `Left` of the `Either` and the second type argument of `MonadError`. You want to be able to say:

```scala
MonadError[Either[Throwable, *], Throwable]
```

where we’re using the JVM’s pervasive `Throwable` to represent errors, but the `Right` `Either` type can be anything, _and this is a complete type_, just like `Monad[Option]` is a complete type in spite of not knowing what the (unwritten) type argument of `Option` is.

It turns out, we can do this, with a compiler plugin called “kind-projector,” which we can use in the Ammonite REPL like this:

```scala
@ import $plugin.$ivy.`org.typelevel:::kind-projector:0.11.0` 
import $plugin.$
```

Now we can ask: “Does `Either[Throwable, *]` imply `MonadError`?”

```scala
@ implicitly[MonadError[Either[Throwable, *], Throwable]] 
res6: MonadError[Either[Throwable, β$0$], Throwable] = cats.instances.EitherInstances$$anon$2@724e483f
```

Indeed it does.

OK. We’ve managed to convince the compiler that `Option` implies `Monad` and that `Either` implies `MonadError`, and we’ve seen that `Monad` offers a _lot_ of functionality. By the way, `MonadError` offers even _more_ functionality, and all `MonadError`s are `Monad`s. We still haven’t seen a representation of “monadic function,” though.

## Kleisli

Yeah, it’s named after the Swiss mathematician who worked it out. Sorry about that. Because it’s a complete data type, not a typeclass or instance of a typeclass for a type in the standard library, it lives in yet another package:

```scala
@ import cats.data.Kleisli 
import cats.data.Kleisli
```

A `Kleisli[M, A, B]`, sometimes called a “Kleisli triple” in the literature, represents an `A => M[B]`.

Pretty underwhelming, right?

By now, though, you may be able to hazard a guess as to where this is going. Because if `Kleisli` can find _evidence_ that `M` implies `Monad` or `MonadError`, might `Kleisli` be able to offer a _lot_ of functionality, like `Monad` and `MonadError` do?

The easiest way to construct a `Kleisli` is just to literally apply it to an `A => M[B]`:

```scala
@ import scala.util.Try 
import scala.util.Try

@ Kleisli { (s: String) => Try { s.toInt }.toEither }
res13: Kleisli[Either[Throwable, B], String, Int] = Kleisli(ammonite.$sess.cmd13$$$Lambda$2649/0x0000000840b94840@47b8e2)
```

I’m using `Try` here to turn the possible failure—a thrown exception—into an `Either` with the `Throwable` on the `Left`, which we already know implies `MonadError`. And this gives us a perfectly good `Kleisli`.

```scala
@ res13("foo") 
res14: Either[Throwable, Int] = Left(java.lang.NumberFormatException: For input string: "foo")
```

Exactly as we’d hope, passing a `String` that isn’t an `Int` gives us the appropriate `Left`.

```scala
@ res13("42") 
res15: Either[Throwable, Int] = Right(42)
```

Exactly as we’d hope, passing a `String` that _is_ a valid `Int` gives us the appropriate `Right`.

Boy, that’s a long-winded way to write `def stringToInt(s: String): Either[Throwable, Int] = Try { s.toInt }.toEither`, isn’t it?

But hang on:

```scala
@ res13. 
&&&                         attemptT                    handleErrorWith             mapF                        pure                        tapWith
&>                          canEqual                    imap                        mapK                        raiseError                  tapWithF
***                         choice                      index                       merge                       recover                     tell
*>                          choose                      invalid                     mkString_                   recoverWith                 toReader
+++                         combine                     invalidNec                  mproduct                    redeem                      traverse
<&                          combineK                    invalidNel                  onError                     redeemWith                  tupleLeft
<*                          combineN                    isEmpty                     orElse                      reduceA                     tupleRight
<*>                         compose                     iterateForeverM             parFoldMapA                 reduceMapK                  typeClassInstance
<+>                         copy                        iterateUntil                parUnorderedFlatTraverse    reject                      unlessA
<<<                         cosequence                  iterateUntilM               parUnorderedTraverse        replicateA                  untilM
>>                          dimap                       iterateWhile                partitionBifold             right                       untilM_
>>=                         distribute                  iterateWhileM               partitionBifoldM            rightIor                    valid
>>>                         ensure                      left                        partitionEitherM            rightNec                    validNec
adaptErr                    ensureOr                    leftIor                     product                     rightNel                    validNel
adaptError                  first                       leftNec                     productArity                rightc                      void
andThen                     flatMap                     leftNel                     productElement              rmap                        whenA
ap                          flatMapF                    leftc                       productElementName          run                         whileM
ap2                         flatTap                     lift                        productElementNames         second                      whileM_
apply                       flatten                     lmap                        productIterator             self                        widen
as                          fmap                        local                       productL                    some                        writer
asLeft                      foldMapK                    lower                       productLEval                split                       |+|
asRight                     foreverM                    map                         productPrefix               sum                         |||
attempt                     fproduct                    map2                        productR                    tailRecM
attemptNarrow               handleError                 map2Eval                    productREval                tap
```

As we hoped, we get a _staggering_ amount of functionality on `Kleisli`, _some of it derived from the typeclass instances `M` has_:

```scala
@ implicitly[MonadError[Kleisli[Either[Throwable, *], String, *], Throwable]] 
res16: MonadError[Kleisli[Either[Throwable, β$0$], String, γ$1$], Throwable] = cats.data.KleisliInstances0_5$$anon$9@5d2d89a6
```

“`Either` implies `MonadError` implies (`Either` implies `Kleisli`) implies (`Kleisli` implies `MonadError`).”

But look: because `Kleisli` captures the notion of “monadic _function_,” we have `andThen`!

```scala
val getDivisor: String => Either[Throwable, Int] = s => Try { s.toInt }.toEither
val divide56By: Int    => Either[Throwable, Int] = i => Try { 56 / i }.toEither
val getDivisorK = Kleisli(getDivisor)
val divide56ByK = Kleisli(divide56By)

val divide56ByString = getDivisorK andThen divide56ByK

@ divide56ByString("3")
res27: Either[Throwable, Int] = Right(18)

@ divide56ByString("foo") 
res28: Either[Throwable, Int] = Left(java.lang.NumberFormatException: For input string: "foo")
```

`andThen` on `Kleisli` is traditionally spelled `>=>`, which is affectionately known as the “[fish operator](https://www.slideshare.net/pjschwarz/kleisli-composition).” This generalization of function composition, which can account for effects and failure, is called “Kleisli composition.”

So my claim is that “Railway-Oriented Programming” is Kleisli composition, and my suggestion is that you should use languages and libraries that make it explicit, if at all possible.

There’s much more to say, especially about effects and `Kleisli`’s many other aspects and its relationship to types and even category theory generally. But this post is already long, because I wanted to try to spell out some details about Cats and make some intuitions explicit. If you’re still reading, thanks for hanging out with me, and I hope you found something worthwhile here.
