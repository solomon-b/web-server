module API where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.User qualified as User
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetLink :: Links.Link
staticGetLink :: Links.Link
mailingListPostLink :: Links.Link
blogDeleteLink :: Links.Link
blogGetLink :: Links.Link
blogIdGetLink :: BlogPosts.Id -> Links.Link
blogIdEditGetLink :: BlogPosts.Id -> Maybe BlogPosts.Body -> Links.Link
blogIdEditPostLink :: BlogPosts.Id -> Links.Link
blogNewGetLink :: Links.Link
blogNewPostLink :: Links.Link
storeDeleteLink :: Links.Link
storeGetLink :: Links.Link
storeNewGetLink :: Links.Link
storeNewPostLink :: Links.Link
storeIdDeleteLink :: Products.Id -> Links.Link
storeIdEditGetLink :: Products.Id -> Maybe Text -> Links.Link
storeIdEditPostLink :: Products.Id -> Links.Link
storeIdGetLink :: Products.Id -> Links.Link
storeIdTogglePublishPatchLink :: Products.Id -> Links.Link
imagePostLink :: Links.Link
markdownPostLink :: Links.Link
aboutGetLink :: Links.Link
userGetLink :: Links.Link
userIdGetLink :: User.Id -> Links.Link
userRegisterGetLink :: Maybe EmailAddress -> Maybe DisplayName -> Maybe FullName -> Links.Link
userRegisterPostLink :: Links.Link
userLoginPostLink :: Maybe Text -> Links.Link
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userCurrentGetLink :: Links.Link
userLogoutGetLink :: Links.Link
userLogoutPostLink :: Links.Link
userDeleteLink :: User.Id -> Links.Link
userPasswordResetPostLink :: User.Id -> Links.Link
adminGetLink :: Links.Link
blogTogglePublish :: BlogPosts.Id -> Links.Link
adminBlogGetLink :: Links.Link
