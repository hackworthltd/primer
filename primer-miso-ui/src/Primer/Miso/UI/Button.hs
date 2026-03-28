{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Miso.UI.Button (
  Button (..),
  button_,
  buttonComponent,
) where

import Foreword

import Data.Data (Data)
import Miso qualified as M
import Miso.Html.Element qualified as H
import Miso.Html.Property qualified as P

data Button = Button {text :: M.MisoString} deriving stock (Eq, Show, Read, Data, Generic)

button_ :: M.Binding parent Button -> M.Component parent Button ()
button_ binding = buttonComponent{M.bindings = [binding]}

emptyButton :: Button
emptyButton = Button mempty

buttonComponent :: M.Component parent Button ()
buttonComponent = M.component emptyButton M.noop $ \model ->
  H.button_ [P.class_ "btn"] [M.text model.text]
