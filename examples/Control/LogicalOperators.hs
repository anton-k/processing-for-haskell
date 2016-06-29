-- original code: https://processing.org/examples/logicaloperators.html

-- Logical Operators.
--
-- The logical operators for AND (&&) and OR (||) are used to combine simple relational statements 
-- into more complex expressions. The NOT (not) operator is used to negate a boolean statement.
import Graphics.Proc

main = runProc $ def { procSetup = setup, procDraw = draw }

width  = 640
height = 360

setup = do    
  size (width, height)
  testRef <- newPioRef True 
  return testRef

draw testRef  = do
  background (grey 126)

  forM_ [5, 10 .. height] $ \i -> do
    -- Logical AND
    stroke (grey 0)
    when (i > 35 && i < 100) $ do
      line (width/4, i) (width/2, i)
      writePioRef testRef False
      
    -- Logical OR
    stroke (grey 76)
    when (i <= 35 || i >= 100) $ do
      line (width/2, i) (width, i)
      writePioRef testRef True

    test <- readPioRef testRef

    -- Testing if a boolean value is "true"
    -- The expression "if(test)" is equivalent to "if (test == true)"
    when (test) $ do
      stroke (grey 0)
      point (width/3, i)

    -- Testing if a boolean value is "false"
    -- The expression "if(!test)" is equivalent to "if(test == false)"
    when (not test) $ do
      stroke (grey 255)
      point (width/4, i)

---------------------------------------------------
-- Side note
--
-- This example originally was intended to show the logical operators,
-- but in Haskell setting it provides a good example for using mutable variables in Haskell.
--
-- Being a purely functional language Haskell gives a way to mutable magic.
-- but it's done in more strict way. In Haskell we can create references that hold values
-- and then we can read and write values to the references.
--
-- The state of the program is the reference testRef that holds the boolean value.
-- We create references with function newPioRef:
--
-- > newPioRef :: a -> Pio (PioRef a)
--
-- We pass an initial value.
-- Then we can read values with the function
--
-- > readPioRef :: PioRef a -> Pio a
--
-- Also we can write the values to the reference:
--
-- > writePioRef :: PioRef a -> a -> Pio ()
--
-- There is a useful function for update of the value inside the reference:
--
-- > modifyPioRef :: PioRef a -> (a -> a) -> Pio ()
--
-- The names for the functions follow the standard established in the similar module `Data.IORef`.
--
-- Also example shows how we can create imperative if-statements without `else`-alternative.
-- The function `when` does the trick:
--
-- > when :: Monad m => Bool -> m () -> m ()
--
-- Here the m-type is automatically substituted with our main monad Pio.
-- We use do-notation to join many effectfull statements in the single expression.
--
-- > when (test) $ do
-- >   stmt1
-- >   stmt2


