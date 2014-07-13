{-# Language TemplateHaskell #-}

import Codeec.Contract
import Codeec.TH
import Contracts

main = print [ $(check "rmw" rmw),          -- Sticky
               $(check "simple" simple),    -- High
               $(check "tv" tv),            -- High
               $(check "tv2" tv2),          -- Fail -- too strong
               $(check "mw" mw),            -- High
               $(check "lastEff" lastEff),  -- Fail -- too strong
               $(check "tv3" tv3) ]         -- Un
