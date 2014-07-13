{-# LANGUAGE TemplateHaskell #-}

import Codeec.Contract

zero :: Int
zero = $(mkZeroIs 0)

main = return ()
