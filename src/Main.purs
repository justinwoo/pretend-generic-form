module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List, intercalate, (:))
import Data.Monoid (mempty)
import Type.Prelude (class IsSymbol, Proxy(Proxy), RProxy(..), SProxy(SProxy), reflectSymbol)

data TextInput
data NumberInput
data CheckBox

foreign import kind Ordering
foreign import data OrderingNil :: Ordering
foreign import data OrderingCons :: Symbol -> Ordering -> Ordering
infixr 1 type OrderingCons as -

data OrderingProxy (o :: Ordering) = OrderingProxy

class BuildForm (xs :: Ordering) (spec :: # Type) where
  buildForm
    :: RProxy spec
    -> OrderingProxy xs
    -> List String -- could be anything else here

instance nilBuildForm :: BuildForm OrderingNil spec where
  buildForm _ _ = mempty

instance consBuildFrom ::
  ( IsSymbol label
  , RowCons label ty trash spec
  , RenderInput label ty
  , BuildForm tail spec
  ) => BuildForm (OrderingCons label tail) spec where
  buildForm spec _ = first : rest
    where
      first = renderInput (SProxy :: SProxy label) (Proxy :: Proxy ty)
      rest = buildForm spec (OrderingProxy :: OrderingProxy tail)

class RenderInput label ty where
  renderInput
    :: SProxy label
    -> Proxy ty
    -> String -- could be anything else here

instance textInputRenderInput ::
  ( IsSymbol label
  ) => RenderInput label TextInput where
  renderInput _ _ = label <> ": TextInput"
    where
      label = reflectSymbol (SProxy :: SProxy label)

instance numberInputRenderInput ::
  ( IsSymbol label
  ) => RenderInput label NumberInput where
  renderInput _ _ = label <> ": NumberInput"
    where
      label = reflectSymbol (SProxy :: SProxy label)

instance checkBoxRenderInput ::
  ( IsSymbol label
  ) => RenderInput label CheckBox where
  renderInput _ _ = label <> ": CheckBox"
    where
      label = reflectSymbol (SProxy :: SProxy label)

type MyFormSpec =
  ( apple :: TextInput
  , banana :: NumberInput
  , cherry :: CheckBox
  )

type MyFormOrdering
  = "cherry"
  - "apple"
  - "banana"
  - OrderingNil

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let
    form = buildForm
      (RProxy :: RProxy MyFormSpec)
      (OrderingProxy :: OrderingProxy MyFormOrdering)
    form' = intercalate "\n" form
  log form'
  -- cherry: CheckBox
  -- apple: TextInput
  -- banana: NumberInput

