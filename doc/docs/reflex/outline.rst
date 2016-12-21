.. _outline

An outline of the chapters
==========================

This tutorial will be a progressive installment on how to write more and more
complex reflex apps; Each chapter will have a companion repo that you can
install and use to learn the concepts we're presenting.

First Chapter: How to get started
------------------------------

Here we'll cover how to build, and minify an example app (commands, cabal flags,
etc). From the code perspective, the code is slightly more complex than the one
in the author's reflex tutorial, offering a first example of a more complex
interaction of signals.

Companion repo: `starterApp <https://github.com/meditans/haskell-webapps/tree/master/UI/ReflexFRP/starterApp>`_

Second Chapter: Client-Server structure and validations
----------------------------------------------------

Here we'll see how to write an application with a server and a client part,
doing a simple authentication of a form.

* How to organize a project with a common part shared between backend and
  frontend.

* A simple server, handling the requests for authentication and using wai to
  gzip the js he's sending.

* Servant integration: how to treat communication with server in the reflex
  network (and calculate the reflex functions directly from the API
  specification).

* A general take on validation, showing how to mix validations on the client and
  on the server side.

Companion repo: `mockLoginPage <https://github.com/meditans/haskell-webapps/tree/master/UI/ReflexFRP/mockLoginPage>`_, corresponding to the mockup `here <https://vacationlabs.github.io/haskell-webapps/ui-mockups/>`_.


Third Chapter: Large scale structure of the app, JSX templating
---------------------------------------------------------------

Here we'll show how to write a multi-page app complete with routing, jsx
templating, hiding of signals with EventWriter, and we'll share a simple case of
ffi binding.

* Descriving the problem we're solving with reflex-jsx and the solution
* Global app structuring
* Routing with servant-router and reflex-contrib-router
* An example of advanced widget creation (the list with "show more")

Companion repo: `mockUsersRoles <https://github.com/meditans/haskell-webapps/tree/mockUsersRoles/UI/ReflexFRP/mockUsersRoles>`_, corresponding to the mockup `here <https://vacationlabs.github.io/haskell-webapps/ui-mockups/role-edit.html>`_ and related.

Fourth Chapter: EventWriter and Global features
-----------------------------------------------

Here we'll demonstrate how to use EventWriter to hide parts of the signalling
infrastructure, leading to more readable code.

* EventWriter and the related advantages in the link structure
* The global interceptor-like feature

Companion repo: `mockUsersRoles <https://github.com/meditans/haskell-webapps/tree/mockUsersRoles/UI/ReflexFRP/mockUsersRoles>`_, corresponding to the mockup `here <https://vacationlabs.github.io/haskell-webapps/ui-mockups/role-edit.html>`_ and related (in another branch).

Fifth Chapter: Later
------------------------------------------------------------

Questions for later.

* FFI bindings
* Comments on Reflex Ecosystem
