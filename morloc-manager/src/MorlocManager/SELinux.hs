{- |
Module      : MorlocManager.SELinux
Description : SELinux detection and volume mount safety
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Handles SELinux-aware volume mounting for containers. On systems where
SELinux is Enforcing or Permissive, bind-mounted volumes need the @:z@
relabel suffix so the container process can access the files. However,
relabeling is dangerous on certain paths -- SELinux will refuse or cause
system-wide damage if you relabel system directories or the user's home
root.

@
  SELinux decision tree for volume mounts:

  getenforce
    |
    +--> Disabled --> mount as-is (no suffix)
    |
    +--> Enforcing/Permissive
           |
           +--> is path safe to relabel?
                  |
                  +--> YES: append :z suffix
                  |
                  +--> NO: error (user must use a subdirectory)

  Unsafe paths (must NOT be relabeled):
    /tmp/*           -- system directories, SELinux blocks relabeling
    /var/tmp/*       -- same as /tmp
    $HOME itself     -- would relabel SSH keys, dotfiles, everything
    /                -- root filesystem
@
-}

module MorlocManager.SELinux
  ( -- * Detection
    SELinuxMode(..)
  , detectSELinux
    -- * Mount suffix
  , volumeSuffix
    -- * Path safety
  , isSafeToRelabel
  , validateMountPath
  ) where

import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath (normalise, addTrailingPathSeparator)

import MorlocManager.Types (ManagerError(..))

-- | SELinux enforcement mode as reported by @getenforce@.
data SELinuxMode
  = SELinuxEnforcing
    -- ^ Policies are enforced. Relabeling is required for bind mounts.
  | SELinuxPermissive
    -- ^ Policies are not enforced but violations are logged.
    -- Relabeling is still needed to avoid log noise and future breakage.
  | SELinuxDisabled
    -- ^ SELinux is off. No relabeling needed.
  deriving (Eq, Show)

-- | Detect the current SELinux mode by running @getenforce@.
--
-- Returns 'SELinuxDisabled' if @getenforce@ is not found on PATH (common
-- on Debian, Ubuntu, Arch, and other non-SELinux distributions).
detectSELinux :: IO SELinuxMode
detectSELinux = do
  exists <- doesFileExist "/usr/sbin/getenforce"
  if not exists
    then pure SELinuxDisabled
    else do
      (code, stdout', _) <- readProcessWithExitCode "getenforce" [] ""
      case code of
        ExitSuccess -> case takeWhile (/= '\n') stdout' of
          "Enforcing"  -> pure SELinuxEnforcing
          "Permissive" -> pure SELinuxPermissive
          _            -> pure SELinuxDisabled
        _ -> pure SELinuxDisabled

-- | Determine the volume mount suffix for the current SELinux mode.
--
-- Returns @":z"@ only on Enforcing systems. Permissive mode logs
-- violations but does not block access, so relabeling is unnecessary
-- and would cause false-positive home-directory mount skips.
volumeSuffix :: SELinuxMode -> String
volumeSuffix SELinuxEnforcing  = ":z"
volumeSuffix SELinuxPermissive = ""
volumeSuffix SELinuxDisabled   = ""

-- | Check whether a path is safe to apply SELinux relabeling (@:z@).
--
-- Relabeling is UNSAFE for:
--
--   * @\/tmp@ and @\/var\/tmp@ -- SELinux blocks relabeling of system temp dirs
--   * The user's home directory root (@$HOME@ itself, not subdirectories)
--   * The filesystem root @\/@
--
-- Relabeling any of these would either fail or cause widespread damage
-- (e.g., relabeling @$HOME@ changes the SELinux context of SSH keys,
-- shell configs, and every dotfile).
isSafeToRelabel :: FilePath -> IO Bool
isSafeToRelabel path = do
  home <- getHomeDirectory
  let norm = normalise path
      homeNorm = addTrailingPathSeparator (normalise home)
      -- Check if path IS the home directory (not a subdirectory of it)
      isHomeRoot = addTrailingPathSeparator norm == homeNorm
  pure $ not (isUnsafeSystemPath norm || isHomeRoot)

-- | Validate that a mount path is safe for SELinux relabeling.
-- Returns @Right ()@ if safe, @Left SELinuxError@ with a helpful message
-- if the path would be dangerous to relabel.
validateMountPath :: FilePath -> IO (Either ManagerError ())
validateMountPath path = do
  safe <- isSafeToRelabel path
  if safe
    then pure (Right ())
    else pure (Left (SELinuxError (
      "Cannot bind-mount " <> path <> " with SELinux relabeling. "
      <> "This path is unsafe to relabel. "
      <> "Use a subdirectory instead (e.g., " <> path <> "/project/)."
      )))

-- ======================================================================
-- Internal
-- ======================================================================

-- | Paths that must never be relabeled under SELinux.
isUnsafeSystemPath :: FilePath -> Bool
isUnsafeSystemPath p =
  let norm = addTrailingPathSeparator (normalise p)
  in norm == "/" || hasPrefix "/tmp/" norm || hasPrefix "/var/tmp/" norm
  where
    hasPrefix prefix str =
      take (length prefix) str == prefix
      || addTrailingPathSeparator str == prefix
