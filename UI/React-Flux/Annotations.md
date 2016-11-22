# Annotations

We'll annotate here doubts and document the blockers

* The react-flux project need a simple example to compile. There are examples in
  the repo, but they are interwined with the structure of the project. Done.

* Depending on ghcjs-base as the original project did does not permit the
  compilation on ghc, twarting our tools. Instead, using jsaddle and
  jsaddle-dom, I can obtain a build both for ghc and ghcjs. Done.
  
* We could investigate the structure for node compilation. Postponed.

* I'm getting runtime errors that aren't caught by the types and are opaque. I
  don't like this.

  ``` uncaught exception in Haskell main thread: Invariant Violation:
  _registerComponent(...): Target container is not a DOM element. ```
  
  Ok the problem was that the string passed as a first argument to reactRender
  (in main) should be equal to the string in the html. Done.

