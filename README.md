Nick's Tiny Scheme Implementation
=================================

This is my implementation of a Scheme-like programming language in Haskell. It
is still rather new and doesn't have a lot of features, such as:

- Error handling. If you do something bad with the Scheme types, like try to
  apply an integer instead of a function, you will get a Haskell runtime
  exception. It will most likely be some incomprehensible pattern matching
  failure and you will hate me. A solution would be to add ErrorT to the
  monad transformer stack and explicitly throw errors when these cases come up.

- Builtins. So far only lambda and quote are supported, though adding new
  builtins should be really, really easy. Builtins take an argument list and
  produce an action in the interpreter's monad, so they can do any IO action
  and make changes to the bindings.

Feel free to hack on it. It's a pretty small, simple code base, which is very
much the point.
