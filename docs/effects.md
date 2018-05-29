A function maps data from one form to another. Where do effects, like writing
something to a database, creating a file, showing a plot, or running an
application fit in?

For example, what is the type signature of a histogram function?

```
histogram :: [Num] -> ?   
```

We can delay visualization, keeping things in the pure realm a little longer,
by converting the vector of numbers to bins:


```
histogram :: [Num] -> [Rectangle]
```

While this conveniently decouples the histogram algorithm from the
visualization, it doesn't solve the problem. We still want to make an image.

We could allow the effect to run and return the filename

```
histogram :: [Num] -> Filename
```

Though `Filename` is fairly ad hoc. Is this a PDF? Or a PNG? Perhaps here
parameterized types would be worthwhile. Where `Path PDF` implies a filename
reference to the PDF is being passed (alternatively `Bytestream PDF`). Or
perhaps just `PDF`, where `PDF` is a complex representation of the PDF vector
object (see the 700 page reference manual) that will be converted into an
appropriate data structure (e.g. PDF object) in the target language.

```
histogram :: [Num] -> Path PDF
histogram :: [Num] -> Bytestream PDF
histogram :: [Num] -> PDF
```

But this still does not capture the fact that a file was created. More clearly,
imagine a robotic function that calculates a movement vector from the original
position, moves the robot, and then returns the new position. The signature
might be:

```
literallyMove :: Position -> Vector -> Position
```

But this does not capture the fact that the function is also causing an effect
(making the robot move).

In old Morloc, I have support for `Void` types, which roughly indicate IO. So
we could couple two function:

```
calculateMove :: Position -> Vector
moveRobot :: Vector -> Void
moveRobot $ calculateMove (1.1, 3.4)
```

Perhaps we can leave Void as Void, for the base type, then expound upon it
semantically. Perhaps with a prefix to indicate effect, e.g.:

```
-- using '!', I'd want to think more before settling
moveRobot :: Vector -> !MoveRobot
```

Now of course, in Haskell we would solve this with an IO monad. Do we want
monads? But we really can't pretend to be pure, since the functions are black
boxes. And even if we could be pure, would we want to be? Morloc needs to allow
effects. Otherwise Haskell would be about the only major language it could
support.

Conclusion: for now I lean towards specialized Void.
