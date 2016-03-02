# Commander: A Haskell Library for parsing commands

This library aims to make it super easy to create nested commands with flags in the form of an object that is easy to traverse and run custom actions on, and easy to extend with handling for safe casting to values from their input strings.

The desired outcome is that we define our commands along the lines of:

```
cmds = commands $ do

    command "hello" $ do

        help "This is the hello command"

        command "nope" $ do
            help "This is the nope command."
            run $
                \(Flag a :: Flag '["c"] "count"  Int)
                 (Flag s :: Flag '["s"] "string" Char) ->
                  putStrLn ("nope! " ++ [s] ++ show a)

        command "wee" $ do
            help "This is the weeeee command."
            run $
                \(Flag a :: Flag '["c"] "count"  Int)
                 (Flag s :: Flag '["s"] "string" Char) ->
                  putStrLn ("weeee! " ++ [s] ++ show a)

    command "bye" $ do

        help "This is the bye command"

        command "woop" (return ())
```

And this leads to `cmds` being a nexted record of type `Command out` (where `out` is the output from the functions you provide to the `run` command), which can be traversed and interacted with.

The novelty of this approach (compared to others I have seen, which is a non exhaustive list!) is the use of the functions input parameter types to cast-from-string and inject the appropriate values into the function safely at runtime, as well as to generate documentation. This means that we only declare the flags and values each command requires in one place, and do not execute any of the output unless all parameters are fully satisfied. These functions are then safely hidden away inside an existential type, and so we don't need any other type level magic or handling to work with the output.

# Installation

1. Add this repository to your stack.yaml file under the packages folder, so we end up with somehting that looks a bit like:
   ```
   packages:
   - '.'
   - location:
       git: https://github.com/jsdw/hs-commander
       commit: abcdef123456789abcdef1234
   ```
2. run `stack install`.
3. import `Commander` into your library.

# Disclaimer

This project is still under heavy development and so will likely change drastically!