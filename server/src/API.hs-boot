module API where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.InvalidField (InvalidField)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetLink :: Links.Link
staticGetLink :: Links.Link
mailingListPostLink :: Links.Link
blogGetLink :: Links.Link
blogIdGetLink :: BlogPosts.Id -> Links.Link
blogIdEditGetLink :: BlogPosts.Id -> Maybe BlogPosts.Body -> Links.Link
blogIdEditPostLink :: BlogPosts.Id -> Links.Link
blogIdPreviewGetLink :: BlogPosts.Id -> Maybe Text -> Links.Link
blogNewGetLink :: [InvalidField] -> Links.Link
blogNewPostLink :: Links.Link
imagePostLink :: Links.Link
aboutGetLink :: Links.Link
userGetLink :: Links.Link
userIdGetLink :: User.Id -> Links.Link
userRegisterGetLink :: Maybe EmailAddress -> Maybe DisplayName -> Maybe FullName -> [InvalidField] -> Links.Link
userRegisterPostLink :: Links.Link
userLoginPostLink :: Maybe Text -> Links.Link
userLoginGetLink :: Maybe Text -> Maybe EmailAddress -> Links.Link
userCurrentGetLink :: Links.Link
userLogoutGetLink :: Links.Link
userLogoutPostLink :: Links.Link
userDeleteLink :: User.Id -> Links.Link
userPasswordResetPostLink :: User.Id -> Links.Link
adminGetLink :: Links.Link
