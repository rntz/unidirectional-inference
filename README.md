I've had a thought about what seems like a very mild extension to bidirectional type inference. For now, I'm calling it "unidirectional type inference", for reasons I don't explain in this README, but I wouldn't be surprised if someone has already used that name for something else.

One way of presenting bidirectional type inference is that your term language looks completely untyped, except for a single "type ascription" form:

    terms M ::= x | M N | λx.M | (M : A) | ...
    types A ::= A -> B | ...

In this case λ cannot synthesize, so you often have to annotate your top-level λ-terms:

    (λx. x) : nat -> nat

But in practice, to infer the type of a λ, it's often enough only to know the type of its argument (not its return type), which suggests a different syntax:

    terms M ::= x | M N | λx:A. M | ...
    types A ::= A -> B | ...

Then we can write:

    λx:nat. x

I was wondering: Is there a principled way of seeing the latter syntax as a special-case of the former? It seems to me there might be, if we let ourselves ascribe "fuzzy types", which may have placeholder wildcards:

    terms       M ::= x | M N | λx.M | (M : A) | ...
    fuzzy types A ::= _ | A -> B | ...

(that "_" is to be read as a literal piece of object-level syntax, not meta-syntax.)

Then we can write:

    (λx.x) : nat -> _

Which tells the type checker that (λx.x) is a function taking a `nat` argument, but with unknown result type. This is morally equivalent to (λx:nat. x), but the "fuzzy type" formulation allows for slightly more generality. For example, we can annotate only the half of a sum type which we don't use:

    -- actual type (int + string), but the `int` can be inferred.
    in₁ 0 : _ + string

Or, we can combine multiple annotations, each of which gives only partial information:

    -- actual type (nat -> nat -> nat)
    (λx. λy. x) : (nat -> _) : (_ -> nat -> _)

I've implemented this idea in Haskell and Racket. It seems to work so far, but I haven't tried anything at all interesting (sum types, subtyping, polymorphism) yet, and I have no theory to back it up.
