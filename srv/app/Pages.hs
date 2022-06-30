{-# LANGUAGE OverloadedStrings #-}

module Pages
  ( renderIndex
  ) where

import Data.Text.Lazy
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as G

type Block = (Html, H.Attribute)

currTemp :: (Show a) => Maybe a -> Block
currTemp temp = (H.div h, G.id "curr")
  where
    h =
      (H.span ! G.class_ "label" $ H.toHtml $ pack "current temperature") <>
      (H.span ! G.class_ "data" $ case temp of
          Just t -> H.toHtml $ (pack . show) t
          Nothing -> H.abbr ! G.title "No input has been received." $ H.toHtml $ pack "N/A")

recentTemps :: Block
recentTemps = (h, mempty)
  where
    h =
      H.details $
      (H.summary ! G.class_ "label" $ H.toHtml $ pack "recent temperatures") <>
      (H.ul ! G.class_ "templist label" $ H.li ! G.class_ "weak" $ H.toHtml $ pack "XX°")

renderBlock :: Block -> Html
renderBlock (h, attrs) = h ! attrs ! G.class_ "md block"

siteHeader :: Text -> Html
siteHeader t =
  H.head $
  H.title (H.toHtml t) <>
  H.link ! G.rel "stylesheet" ! G.href "/static/main.css"

siteMain :: [Block] -> Html
siteMain bs =
  H.body $
  H.div ! G.id "main" $
  H.div ! G.id "text" $ mconcat $ Prelude.map renderBlock bs

renderIndex :: (Show a) => Maybe a -> Text
renderIndex temp = renderHtml inner
  where
    inner =
      H.docTypeHtml $ siteHeader (pack "Vaccine Monitoring") <> siteMain blocks
    blocks = [currTemp temp, recentTemps] -- TODO: type-safe settings
