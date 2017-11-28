General thoughts
* Haskell is good a checking your type assumptions and warn you if you miss something.
  * E.g. type-constrained polymorphism checks
* When you use concrete type instead of typeclasses, this concrete types implies all its typeclasses 
(i.e. instances of type-classes for this type)
* Imports from type-class definition is available everywhere once imported

Ord
* Make sure that your Ord instances agree with your Eq instances
