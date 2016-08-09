-- | Working with users, groups, file permissions and file ownerships.
--   Windows compatibility not guaranteed.
module Control.Shell.Posix
  ( -- * Users and groups
    User, Group
  , getCurrentUser, Control.Shell.Posix.getCurrentGroups
  , getOwner, getGroup, getOwnership
  , setOwner, setGroup, setOwnership

    -- * File permissions
  , CMode
  , unionFileModes, intersectFileModes
  , nullFileMode
  , ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes
  , groupReadMode, groupWriteMode, groupExecuteMode, groupModes
  , otherReadMode, otherWriteMode, otherExecuteMode, otherModes
  , stdFileMode, accessModes
  , Control.Shell.Posix.setFileMode, getFileMode
  , Control.Shell.Posix.setFileCreationMask
  ) where
import System.Posix as Posix
import Control.Shell

type User = String
type Group = String

-- | Set the mode (permissions, etc.) of the given file.
setFileMode :: FilePath -> CMode -> Shell ()
setFileMode f = liftIO . Posix.setFileMode f

-- | Get the mode of the given file.
getFileMode :: FilePath -> Shell CMode
getFileMode = liftIO . fmap fileMode . getFileStatus

-- | Get the currently effective user name.
getCurrentUser :: Shell String
getCurrentUser = liftIO getEffectiveUserName

-- | Get the list of groups associated with the current process.
getCurrentGroups :: Shell [String]
getCurrentGroups = liftIO $ do
  gids <- Posix.getGroups
  mapM (fmap groupName . getGroupEntryForID) gids

-- | Set the owner of the given file.
setOwner :: User -> FilePath -> Shell ()
setOwner name file = liftIO $ do
  uid <- userID <$> getUserEntryForName name
  setOwnerAndGroup file uid (-1)

-- | Set the group of the given file.
setGroup :: Group -> FilePath -> Shell ()
setGroup group file = liftIO $ do
  gid <- groupID <$> getGroupEntryForName group
  setOwnerAndGroup file (-1) gid

-- | Set the owner and group of the given file.
setOwnership :: User -> Group -> FilePath -> Shell ()
setOwnership name group file = liftIO $ do
  uid <- userID <$> getUserEntryForName name
  gid <- groupID <$> getGroupEntryForName group
  setOwnerAndGroup file uid gid

-- | Get the owner of the given file.
getOwner :: FilePath -> Shell User
getOwner file = liftIO $ do
  uid <- fileOwner <$> getFileStatus file
  userName <$> getUserEntryForID uid

-- | Get the group of the given file.
getGroup :: FilePath -> Shell Group
getGroup file = liftIO $ do
  gid <- fileGroup <$> getFileStatus file
  groupName <$> getGroupEntryForID gid

-- | Get the owner and group of the given file.
getOwnership :: FilePath -> Shell (User, Group)
getOwnership file = liftIO $ do
  st <- getFileStatus file
  user <- userName <$> getUserEntryForID (fileOwner st)
  group <- groupName <$> getGroupEntryForID (fileGroup st)
  return (user, group)

-- | Set the file creation mask of this process.
setFileCreationMask :: CMode -> Shell CMode
setFileCreationMask = unsafeLiftIO . Posix.setFileCreationMask
