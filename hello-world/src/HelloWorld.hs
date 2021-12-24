-- Write a function that returns the string "Hello, World!".

-- All modules begin with the keyword `module`
-- then the name of the module, title-cased
-- then a list of expressions to export wrapped in parenthesis
-- then the keyword `where`
-- This allows the module to be used in other modules using `import`
module HelloWorld (hello) where

-- The type definition for `hello`. It simply returns a String
hello :: String

-- The function declaration for `hello`. It returns the String "Hello, World!"
-- You can see how the declaration matches the type definition above.
hello = "Hello, World!"
