{-# LANGUAGE DuplicateRecordFields #-}

module Widget where

import Font
import SDL qualified
import Text
import Prelude

class Widget w where
    widget
        :: (MonadIO m)
        => SDL.Renderer
        -> Fonts
        -> SDL.Point SDL.V2 CInt
        -> CInt
        -> w
        -> m CInt

data Spacer = Spacer

instance Widget Spacer where
    widget _ _ _ _ Spacer = pure 8

data Label = Label {label :: Text, hAlign :: HAlign, vAlign :: VAlign, font :: Font}

instance Widget Label where
    widget renderer _ (SDL.P (SDL.V2 x y)) w Label{..} = do
        let padding = 4 :: CInt
        let h = font.size + 2 * padding
        drawText
            renderer
            font.font
            font.cache
            (SDL.V4 255 255 255 255)
            label
            hAlign
            vAlign
            (rect x y w h)
        pure h

instance Widget Text where
    widget renderer fonts p w label =
        widget
            renderer
            fonts
            p
            w
            Label{label, hAlign = Text.Left, vAlign = Top, font = fonts.cherry13r}

data Button = Button {label :: Text, width :: CInt}

instance Widget Button where
    widget renderer fonts (SDL.P (SDL.V2 x y)) w' Button{..} = do
        let w = min width w'
        let h = fonts.cherry13r.size + 8
        let r = rect x y w h
        SDL.rendererDrawColor renderer $=! SDL.V4 30 30 60 255
        SDL.fillRect renderer $ Just r
        drawText
            renderer
            fonts.cherry13r.font
            fonts.cherry13r.cache
            (SDL.V4 255 255 255 255)
            label
            HCentre
            VCentre
            r
        pure h

data TextInput = TextInput {label :: Text, value :: Text}

instance Widget TextInput where
    widget renderer fonts p@(SDL.P (SDL.V2 x y')) w TextInput{..} = do
        dy <-
            widget
                renderer
                fonts
                p
                w
                Label{label, hAlign = Text.Left, vAlign = Top, font = fonts.cherry13b}
        let y = y' + dy
        let padding = 4 :: CInt
        let h = fonts.cherry13r.size + 2 * padding
        let outline = SDL.V4 @Word8 128 128 128 255
        SDL.rendererDrawColor renderer $=! outline
        SDL.drawRect renderer $ Just $ rect x y w h
        drawText
            renderer
            fonts.cherry13r.font
            fonts.cherry13r.cache
            (SDL.V4 255 255 255 255)
            value
            Text.Left
            VCentre
            (rect (x + padding) y (w - 2 * padding) h)
        pure $ dy + h + padding
