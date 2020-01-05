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
import Brick.Types (ViewportType(..))
import Brick.BChan
import Control.Monad.IO.Class (liftIO)

data Name = StagingEnvSelector | DevEnvSelector deriving (Generic, Show, Eq, Ord)

data EnvConfig =
    EnvConfig {_contextEnv :: AppEnv
             }
             deriving (Show)

makeLenses ''EnvConfig

mkForm :: EnvConfig -> Form EnvConfig e ViewPortName
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Environment" @@=
                   radioField contextEnv
                      [ (Staging, ConfigViewPort StagingEnvSelector, "Staging")
                      , (Dev, ConfigViewPort DevEnvSelector, "Dev")
                      ]
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

drawConfigVP :: Form EnvConfig CustomEvent ViewPortName -> [Widget ViewPortName]
drawConfigVP f = [C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Select Environment\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

data ViewPortName = ConfigViewPort Name | StatusViewPort deriving (Generic, Show, Eq, Ord)

drawStatusVP :: Form EnvConfig CustomEvent ViewPortName -> [Widget ViewPortName]
drawStatusVP f = 
    [ (viewport (ConfigViewPort StagingEnvSelector) Vertical 
        $ B.border
        $ vLimit 40 
        $ vBox 
        $ drawConfigVP f)
    <+> 
      (viewport StatusViewPort Vertical $ B.border $ str "Status")
    ]

app :: BChan CustomEvent -> App (Form EnvConfig CustomEvent ViewPortName) CustomEvent ViewPortName
app chan =
    App { appDraw = drawStatusVP
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                AppEvent SelectedItem -> halt s
                _ -> do
                    ns <- handleFormEvent ev s
                    liftIO $ writeBChan chan SelectedItem
                    continue ns

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

data CustomEvent = SelectedItem

bootTUI :: IO AppEnv
bootTUI = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        f = mkForm (EnvConfig Staging)

    initialVty <- buildVty
    eventChan <- Brick.BChan.newBChan 10
    f' <- customMain initialVty buildVty (Just eventChan) (app eventChan) f

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
    return $ _contextEnv $ formState f'
