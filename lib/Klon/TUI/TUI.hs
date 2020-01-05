{-# LANGUAGE CPP #-}
module Klon.TUI.TUI where

import           Brick
import           Brick.Focus          (focusRingCursor)
import           Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E
import           Data.Monoid          ((<>))
import           GHC.Generics
import qualified Graphics.Vty         as V
import           Lens.Micro.TH
import Klon.Config (AppEnv(..))

data Name = StagingEnvSelector | DevEnvSelector deriving (Generic, Show, Eq, Ord)

data EnvConfig =
    EnvConfig {_contextEnv :: AppEnv
             }
             deriving (Show)

makeLenses ''EnvConfig

mkForm :: EnvConfig -> Form EnvConfig e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Environment" @@=
                   radioField contextEnv
                      [ (Staging, StagingEnvSelector, "Staging")
                      , (Dev, DevEnvSelector, "Dev")
                      ]
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form EnvConfig e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Select Environment\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form EnvConfig e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                _ -> do
                    ns <- handleFormEvent ev s
                    continue ns

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

bootTUI :: IO AppEnv
bootTUI = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        f = mkForm (EnvConfig Staging)

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
    return $ _contextEnv $ formState f'
