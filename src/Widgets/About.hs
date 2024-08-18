module Widgets.About where

--------------------------------------------------------------------------------

import Lucid (HtmlT)
import Lucid.Html5
import Utils.HTML (classes_)

--------------------------------------------------------------------------------

widget :: (Monad m) => HtmlT m ()
widget =
  div_ [classes_ ["w-2/3", "mx-auto"]] $ do
    img_ [src_ "static/what-is-Hypertext-Transfer-Protocol-HTTP-1920x1080.jpg", classes_ ["h-auto", "max-w-xl", "mx-auto", "my-3"]]

    h1_ [classes_ ["mb-4", "text-4xl", "text-center", "font-extrabold", "leading-none", "tracking-tight", "text-gray-900", "md:text-5xl", "lg:text-6xl"]] "Connect, Create, and Captivate."

    h2_ [classes_ ["text-4xl", "font-bold"]] "Overview of HyperNet"
    p_ [classes_ ["mb-3", "text-lg", "text-gray-500", "md:text-xl"]] "HyperNet is an advanced hypermedia system designed to enhance user interaction with digital content. This innovative platform allows developers to integrate text, images, audio, and video into a single, interactive environment."

    p_ [classes_ ["mb-3", "text-gray-500"]] "Leveraging the power of the World Wide Web, HyperNet supports the creation of rich multimedia presentations accessible through any web browser. With its intuitive interface, content creators can easily link media elements using clickable hotspots and hyperlinks, creating a dynamic navigation experience."

    h2_ [classes_ ["text-4xl", "font-bold"]] "Features and Functionality"
    p_ [classes_ ["text-gray-500"]] "HyperNet is built on a flexible architecture that supports integration with various legacy systems and media formats, making it ideal for corporate and educational settings. Its support for CGI scripting and server-side includes allows developers to create personalized and interactive content, enhancing user engagement. The system’s compatibility with popular media standards like JPEG, GIF, WAV, and MPEG ensures smooth playback and broad accessibility. HyperNet’s comprehensive documentation and active user community provide a wealth of resources for developers, fostering a collaborative environment."
