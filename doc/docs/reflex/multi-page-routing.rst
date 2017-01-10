.. _multi_page_routing

Multi page routing
==================

Here we’ll show how to write a multi-page app complete with routing, jsx
templating, hiding of signals with EventWriter, and we’ll share a simple case of
ffi binding.

The accompanying repo is at TODO
Companion repo: mockUsersRoles, corresponding to the mockup here and related.

Describing the problem we’re solving with reflex-jsx and the solution
---------------------------------------------------------------------

Today app is a bit different from the previous one, in the sense that we got a
copious quantity of markup (generated with the bootstrap framework). We could
try to translate it manually, but this would be a pain;

Another thing to keep in mind is the translating solution forces designers to be
able to operate on the markup level.

Just to be clear, if you're designing your app from the ground up I still think
there is something to be gained from writing all the stuff manually, without
templating. (Expand point here)

My concern is also addressing another point I find somehow troublesome: the
monadic structure is used in reflex for both construction in the DOM and the
construction of the signals:

This leads to the following problem in my opinion: let's consider this example
code for a widget:

.. code-block:: haskell

  widget :: MonadWidget t m => m ()
  widget = do
    a <- firstThing
    b <- secondThing
    c <- f a b

As we can see, the structure of the signals here is quite simple.

Let's now say that we want to include ``a`` and ``b`` in a ``div`` for
presentation reasons: we would have to say:

We need also to present the values of ``a`` and ``b`` outside of the ``div``, to
continue the elaboration.

.. code-block:: haskell

  widget :: MonadWidget t m => m ()
  widget = do
    (a,b) <- el "div" $ do
      a <- firstThing
      b <- secondThing
      return (a,b) 
    c <- f a b

So we see that now we had to add a return to extract all the values we're
interested in extracting from the block, and binding them again.

Now, this might not seem a terrible problem when the structure is so simple, but
it is when you have to deal with more complicated structures, like:

TODO: Choose a code snippet that illustrates this problem and for which we have
a reflex-jsx translation

I'm presenting here a solution I wrote while working on this problem, based on the `reflex-jsx
<https://hackage.haskell.org/package/reflex-jsx-0.1.0.0>`_, and for comparison my fork is `here
<https://github.com/meditans/reflex-jsx/tree/returningValues>`_.

TODO: Talk about the working of the library, again choosing a simple example from the code.

Global app structuring
----------------------

Routing with servant-router and reflex-contrib-router
-----------------------------------------------------

An example of advanced widget creation (the list with “show more”)
------------------------------------------------------------------


