module Dynamic.Import (toValue) where


import Dynamic.Syntax (Value(..), Expression(..))

import qualified Data.Aeson as Aes

import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S

import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Control.Applicative
import Control.Arrow
import Utilities ((|>))
import Data.Scientific (toRealFloat)

toValue :: String -> Maybe Value
toValue file = transform <$> Aes.decode (pack file)

transform :: Aes.Value -> Value
transform (Aes.Object hashMap) = hashMap
  |> HM.toList
  |> map (first unpack)
  |> map (second (ValueExpr . transform))
  |> M.fromList
  |> ObjectValue

transform (Aes.Array array) = array
  |> V.toList
  |> map (ValueExpr . transform)
  |> ArrayValue

transform (Aes.String text) = text
  |> unpack
  |> AtomicString

transform (Aes.Number scientific) = scientific
  |> toRealFloat
  |> AtomicNumber

transform (Aes.Bool bool) = AtomicBool bool
transform Aes.Null = Null

len :: Value -> Int
len value = case value of
  ObjectValue object -> object |> M.toList |> map snd |> calculateList
  BagValue bag -> bag |> S.toList |>  calculateList
  ArrayValue array -> array |> calculateList
  _ -> 1

  where calculateList = sum . (map (len . ValueExpr))