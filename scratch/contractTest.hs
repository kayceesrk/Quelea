{-# Language TemplateHaskell #-}

import Codeec.Contract
import Codeec.TH
import Contracts

main = print [ $(check "rmw" rmw),          -- Sticky
               $(check "simple" simple),    -- High
               $(check "tv" tv),            -- High
               $(check "tv2" tv2),          -- Un
               $(check "mw" mw),            -- High
               $(check "cyclic" cyclic),    -- Not well-typed
               $(check "lastEff" lastEff)   -- Not well-typed
               $(check "tv3" tv3)           -- Un
             ]

