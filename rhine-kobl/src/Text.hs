module Text where

import Data.LruCache qualified as LruCache
import Font (FontCache)
import SDL qualified
import SDL.Font qualified
import SDL.Font qualified as SDL (Font)
import Prelude

data HAlign
    = Left
    | Right
    | HCentre

data VAlign
    = Top
    | Bottom
    | VCentre

drawText
    :: (MonadIO m)
    => SDL.Renderer
    -> SDL.Font
    -> TVar FontCache
    -> SDL.Font.Color
    -> Text
    -> HAlign
    -> VAlign
    -> SDL.Rectangle CInt
    -> m ()
drawText renderer font fontCache color text hAlign vAlign (view rectTuple -> (dstX', dstY', dstW, dstH)) = do
    texture <-
        lookupCache text >>= \case
            Nothing -> do
                surface <- SDL.Font.blended font color text
                texture <- SDL.createTextureFromSurface renderer surface
                insertCache text texture
                pure texture
            Just (texture, newCache) -> do
                replaceCache newCache
                pure texture
    SDL.TextureInfo{textureWidth = srcW, textureHeight = srcH} <-
        SDL.queryTexture texture
    let w = min srcW dstW
    let h = min srcH dstH
    let (srcX, dstX) =
            case hAlign of
                Text.Left -> (0, dstX')
                Text.Right -> (srcW - w, dstX' + dstW - w)
                HCentre -> ((srcW - w) `div` 2, dstX' + (dstW - w) `div` 2)
    let (srcY, dstY) =
            case vAlign of
                Top -> (0, dstY')
                Bottom -> (srcH - h, dstY' + dstH - h)
                VCentre -> ((srcH - h) `div` 2, dstY' + (dstH - h) `div` 2)
    let srcRect = rect srcX srcY w h
    let dstRect = rect dstX dstY w h
    SDL.copy renderer texture (Just srcRect) (Just dstRect)
  where
    withTVar :: TVar a -> (a -> b) -> STM b
    withTVar tvar f = f <$> readTVar tvar
    withCache = liftIO . atomically . withTVar fontCache
    modifyCache = liftIO . atomically . modifyTVar' fontCache
    replaceCache = liftIO . atomically . writeTVar fontCache
    lookupCache = withCache . LruCache.lookup
    insertCache = (modifyCache .) . LruCache.insert
