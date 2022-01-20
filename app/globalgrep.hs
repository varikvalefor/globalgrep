import System.Process;
import System.Environment;
import Control.Concurrent.Async;

-- | For all HostName @a@, @a@ is the hostname of a network host.
type HostName = String;

-- | For all Result @a@, @a@ is a line of @grep@'s output, e.g.,
-- "@/root/Documents/quot/quot1.tex: \quoted{Don't quote me, boy,
-- `cause I ain't said shit.}{---EAZY-E}@".
type Result = String;

main :: IO ();
main = nabQuery >>= (\q -> getSearchHosts >>= search q) >>= display
  where
  display = mapM_ putStrLn . concat
  nabQuery = head <$> getArgs
  getSearchHosts = parseConfig <$> (readFile =<< configFilePath)
  search query = mapConcurrently (searchFor query);

-- | Where @k@ is the content of a @globalgrep@ configuration file,
-- @parseConfig k@ is a list of the names of the network hosts which
-- are mentioned in @k@.
parseConfig :: String -> [HostName];
parseConfig = words;

-- | @configFilePath@ returns the path of the @globalgrep@ configuration
-- file of @globalgrep@'s user.
configFilePath :: IO String;
configFilePath = (++ "/.config/globalgrep") <$> getEnv "HOME";
 
-- @searchFor query host@ returns a list of the @query@-matching files
-- which reside on @host@.
--
-- For all @k@ in @searchFor query host@, @k@ begins with @host@.
searchFor :: String
          -- ^ @globalgrep@ attempts to find the file which matches this
          -- regular expression.
          -> HostName
          -- ^ @globalgrep@ searches for the file on the host whose
          -- hostname is this argument.
          -- searches
          -> IO [Result];
searchFor query host = addHeaders <$> readProcess "ssh" [host, comet] []
  where
  comet :: String
  comet = "grep -ri " ++ show query ++ " /"
  --
  addHeaders :: String -> [String]
  addHeaders = map ((host ++ ": ") ++) . lines;
