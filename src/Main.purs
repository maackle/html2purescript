module Main where

import Prelude (Unit, bind, discard, unit, void, ($))
import Web.HTML (HTMLElement)

import Component (component)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  log "main"
  run body

rerunUI :: HTMLElement -> Effect Unit
rerunUI body = launchAff_ do
  log "rerunUI"
  run body

run ::
  HTMLElement -> Aff Unit
run body = do
  log "run"
  void $ runUI component unit body
