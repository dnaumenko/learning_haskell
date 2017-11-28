# Make it func-y

* the Haskell compiler will infer the most polymorphic type that works
* if we want ot use function as argument, we should use parentheses in type declaration


# Key words
* parameter to values **binding** – happens when you apply function. 
* **shadowing** of variables — Haskell is *lexically* scoped that means 
that resolving of variable depends on the location of code and statements scope.
Shadowing describes the situation when you overwrite (or shadow) some variable 
by declaring variable with the same тфьу in inner scope.
* **pattern matching** – in Haskell, variable could be bound to a given function by a successful match
 which itself will depend on available patterns in function definition.
    * order of patterns matters
* **case expressions** – it's like a switch statement - you have a number of cases and return something
for each case. It differs from pattern matching, because case expressions is an expression
* **higher-order functions** - function that gets or return other functions
* **poinfree style** - point = function argument, it's a style of writing function without mentioning
arguments
     
# New syntax
* **anonymous functions** – function not bounded to identificator. E.g. \x -> x
* **newtype** – special case of data constructor that permits only one constructor
and only one field
* **guard block** definitions – it's a compact syntax for functions that allows 2 or more possible outcomes
depending on boolean conditions