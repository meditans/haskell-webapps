# Annotations

We'll annotate here doubts and document the blockers

* The react-flux project need a simple example to compile. There are examples in
  the repo, but they are interwined with the structure of the project. Done.

* Depending on ghcjs-base as the original project did does not permit the
  compilation on ghc, twarting our tools. Instead, using jsaddle and
  jsaddle-dom, I can obtain a build both for ghc and ghcjs. Done.
  
* We could investigate the structure for node compilation. Postponed.
