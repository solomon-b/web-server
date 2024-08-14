module API.User.Register.Get where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Effects.Observability qualified as Observability
import Htmx.Lucid.Head qualified as Lucid.Htmx
import Lucid
import OpenTelemetry.Trace qualified as Trace
import OrphanInstances.Lucid ()
import Servant ((:>))
import Servant qualified
import Utils.HTML (HTML, RawHtml, classes_, toHTML)

--------------------------------------------------------------------------------

type Route = "user" :> "register" :> Servant.Get '[HTML] RawHtml

--------------------------------------------------------------------------------

data Page = Page

header :: (Monad m) => HtmlT m ()
header =
  head_ $ do
    title_ "web-server"
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Html ())
    Lucid.Htmx.useHtmx

registerCard :: (Monad m) => HtmlT m ()
registerCard =
  div_ [id_ "authentication-modal", classes_ ["overflow-y-auto overflow-x-hidden block z-50 justify-center items-center w-full md:inset-0 h-[calc(100%-1rem)] max-h-full"]] $
    div_ [classes_ ["relative p-4 w-full max-w-md max-h-full mx-auto"]] $
      div_ [classes_ ["relative bg-white rounded-lg shadow dark:bg-gray-700"]] $ do
        div_ [classes_ ["flex items-center justify-between p-4 md:p-5 border-b rounded-t dark:border-gray-600"]] $ do
          h3_ [classes_ ["text-xl font-semibold text-gray-900 dark:text-white"]] "Sign in"
        div_ [classes_ ["p-4 md:p-5"]] $
          form_ [class_ "space-y-4", action_ "user/register", method_ "POST"] $ do
            div_ [] $ do
              label_ [for_ "displayName", classes_ ["block mb-2 text-sm font-medium text-gray-900 dark:text-white"]] "Your name"
              input_ [type_ "displayName", name_ "displayName", id_ "displayName", classes_ ["bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5 dark:bg-gray-600 dark:border-gray-500 dark:placeholder-gray-400 dark:text-white"], placeholder_ "name@company.com"]
            div_ [] $ do
              label_ [for_ "email", classes_ ["block mb-2 text-sm font-medium text-gray-900 dark:text-white"]] "Your email"
              input_ [type_ "email", name_ "email", id_ "email", classes_ ["bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5 dark:bg-gray-600 dark:border-gray-500 dark:placeholder-gray-400 dark:text-white"], placeholder_ "name@company.com"]
            div_ [] $ do
              label_ [for_ "password", classes_ ["block mb-2 text-sm font-medium text-gray-900 dark:text-white"]] "Your password"
              input_ [type_ "password", name_ "password", id_ "password", placeholder_ "••••••••", classes_ ["bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-green-500 focus:border-green-500 block w-full p-2.5 dark:bg-gray-600 dark:border-gray-500 dark:placeholder-gray-400 dark:text-white"]]
            div_ [classes_ ["flex justify-between"]] $ do
              div_ [class_ "flex items-start"] $ do
                div_ [classes_ ["flex items-center h-5"]] $
                  input_ [id_ "remember", type_ "checkbox", value_ "", classes_ ["w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-3 focus:ring-green-300 dark:bg-gray-600 dark:border-gray-500 dark:focus:ring-green-600 dark:ring-offset-gray-800 dark:focus:ring-offset-gray-800"]]
                label_ [for_ "remember", classes_ ["ms-2 text-sm font-medium text-gray-900 dark:text-gray-300"]] "Remember me"
              a_ [href_ "#", classes_ ["text-sm text-green-700 hover:underline dark:text-green-500"]] "Lost Password?"

            button_ [type_ "submit", classes_ ["w-full text-white bg-green-700 hover:bg-green-800 focus:ring-4 focus:outline-none focus:ring-green-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:bg-green-600 dark:hover:bg-green-700 dark:focus:ring-green-800"]] "Login to your account"
            div_ [classes_ ["text-sm font-medium text-gray-500 dark:text-gray-300"]] $ do
              span_ [class_ "px-2"] "Not registered?"
              a_ [href_ "#", classes_ ["text-green-700 hover:underline dark:text-green-500"]] "Create account"

page :: (Monad m) => Lucid.HtmlT m ()
page =
  Lucid.doctypehtml_ $ do
    header

    Lucid.body_ $
      div_ [classes_ ["container", "mx-auto"]] $ do
        registerCard

-- Lucid.body_ $ do
--   Lucid.div_
--     [ Lucid.class_
--         [ "font-sans",
--           "text-gray-900",
--           "antialiased"
--         ]
--     ]
--     $ do
--       Lucid.div_
--         [ Lucid.class_
--             [ "min-h-screen",
--               "flex",
--               "flex-col",
--               "sm:justify-center",
--               "items-center",
--               "pt-6",
--               "sm:pt-0",
--               "bg-[#f8f4f3]"
--             ]
--         ]
--         $ do
--           Lucid.div_
--             [ Lucid.class_
--                 [ "w-full",
--                   "sm:max-w-md",
--                   "mt-6",
--                   "px-6",
--                   "py-4",
--                   "bg-white",
--                   "shadow-md",
--                   "overflow-hidden",
--                   "sm:rounded-lg"
--                 ]
--             ]
--             $ Lucid.form_ [Lucid.method_ "POST", Lucid.action_ "register"]
--             $ do
--               Lucid.div_ [Lucid.class_ "py-8"] $
--                 Lucid.span_
--                   [ Lucid.class_
--                       [ "text-2xl",
--                         "text-center",
--                         "font-semibold",
--                         "grid",
--                         "place-items-center"
--                       ]
--                   ]
--                   "Register"

--               Lucid.div_
--                 $ Lucid.label_
--                   [ Lucid.class_
--                       [ "block",
--                         "font-medium",
--                         "text-sm",
--                         "text-gray-700"
--                       ],
--                     Lucid.value_ "Email"
--                   ]
--                 $ Lucid.input_
--                   [ Lucid.class_
--                       [ "w-full",
--                         "rounded-md",
--                         "py-2.5",
--                         "px-4",
--                         "border",
--                         "text-sm",
--                         "outline-[#f84525]"
--                       ],
--                     Lucid.type_ "text",
--                     Lucid.name_ "email",
--                     Lucid.placeholder_ "Email"
--                   ]

--               Lucid.div_ [Lucid.class_ "mt-4"]
--                 $ Lucid.label_
--                   [ Lucid.class_
--                       [ "block",
--                         "font-medium",
--                         "text-sm",
--                         "text-gray-700"
--                       ],
--                     Lucid.value_ "Password"
--                   ]
--                 $ Lucid.input_
--                   [ Lucid.id_ "password",
--                     Lucid.type_ "password",
--                     Lucid.name_ "password",
--                     Lucid.placeholder_ "Password",
--                     Lucid.class_
--                       [ "w-full",
--                         "rounded-md",
--                         "py-2.5",
--                         "px-4",
--                         "border",
--                         "text-sm",
--                         "outline-[#f84525]"
--                       ]
--                   ]

--               Lucid.div_ [Lucid.class_ "mt-4"]
--                 $ Lucid.label_
--                   [ Lucid.class_
--                       [ "block",
--                         "font-medium",
--                         "text-sm",
--                         "text-gray-700"
--                       ],
--                     Lucid.value_ "Password"
--                   ]
--                 $ Lucid.input_
--                   [ Lucid.id_ "password_confirmation",
--                     Lucid.type_ "password",
--                     Lucid.name_ "password_confirmation",
--                     Lucid.placeholder_ "Confirmation",
--                     Lucid.class_
--                       [ "w-full",
--                         "rounded-md",
--                         "py-2.5",
--                         "px-4",
--                         "border",
--                         "text-sm",
--                         "outline-[#f84525]"
--                       ]
--                   ]

--               Lucid.div_ [Lucid.class_ "mt-4"] $
--                 Lucid.label_ [Lucid.for_ "remember_me", Lucid.class_ ["flex", "items-center"]] $ do
--                   Lucid.input_
--                     [ Lucid.type_ "checkbox",
--                       Lucid.id_ "remember_me",
--                       Lucid.name_ "remember",
--                       Lucid.class_
--                         [ "rounded",
--                           "border-gray-300",
--                           "text-indigo-600",
--                           "shadow-sm",
--                           "focus:ring-indigo-500"
--                         ]
--                     ]
--                   Lucid.span_
--                     [ Lucid.class_
--                         [ "ms-2",
--                           "text-sm",
--                           "text-gray-600"
--                         ]
--                     ]
--                     "Remember Me"

--               Lucid.div_
--                 [ Lucid.class_
--                     [ "flex",
--                       "items-center",
--                       "justify-end",
--                       "mt-4"
--                     ]
--                 ]
--                 $ do
--                   Lucid.a_
--                     [ Lucid.class_
--                         [ "hover:underline",
--                           "text-sm",
--                           "text-gray-600",
--                           "hover:text-gray-900",
--                           "rounded-md",
--                           "focus:outline-none",
--                           "focus:ring-2",
--                           "focus:ring-offset-2",
--                           "focus:ring-indigo-500"
--                         ],
--                       Lucid.href_ "/"
--                     ]
--                     "Forgot your password?"
--                   Lucid.button_
--                     [ Lucid.class_
--                         [ "ms-4",
--                           "inline-flex",
--                           "items-center",
--                           "px-4",
--                           "py-2",
--                           "bg-[#f84525]",
--                           "border",
--                           "border-transparent",
--                           "rounded-md",
--                           "font-semibold",
--                           "text-xs",
--                           "text-white",
--                           "uppercase",
--                           "tracking-widest",
--                           "hover:bg-red-800",
--                           "focus:bg-gray-700",
--                           "active:bg-gray-900",
--                           "focus:outline-none",
--                           "focus:ring-2",
--                           "focus:ring-indigo-500",
--                           "focus:ring-offset-2",
--                           "transition",
--                           "ease-in-out",
--                           "duration-150"
--                         ]
--                     ]
--                     "Sign In"

--------------------------------------------------------------------------------

handler ::
  ( Applicative m,
    Has Trace.Tracer env,
    MonadCatch m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  m RawHtml
handler =
  Observability.handlerSpan "GET /user/login" () (const @Text "RawHtml") $ do
    pure $ toHTML page
