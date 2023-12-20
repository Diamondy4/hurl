module PyFCustom where

import Language.Haskell.TH.Quote
import PyF
import PyF.Internal.QQ

cFmt :: QuasiQuoter
cFmt =
    mkFormatter
        "cFmt"
        ( fmtConfig
            { delimiters = Just ('@', '!')
            }
        )
