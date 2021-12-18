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
main = head <$> getArgs >>= \query ->
       (parseConfig <$> (readFile =<< configFilePath)) >>= \hosts ->
       mapConcurrently (searchFor query) hosts >>= mapM_ putStrLn . concat;

-- | Where @k@ is the content of a @globalgrep@ configuration file,
-- @parseConfig k@ is a list of the names of the network hosts which
-- are mentioned in @k@.
parseConfig :: String -> [HostName];
parseConfig = words;

-- | @configFilePath@ returns the path of the @globalgrep@ configuration
-- file of @globalgrep@'s user.
configFilePath :: IO String;
configFilePath = (++ "/.config/globalgrep") <$> getEnv "HOME";
 
-- | Where @query@ is a string such that files which contain @query@
-- should be located and @host@ is the hostname of a network
-- host on which files which contain @query@ may be located,
-- @searchFor query host@ returns a list of the @query@-containing
-- files which reside on @host@.
--
-- For all @k@ in @searchFor query host@, @k@ begins with @host@.
searchFor :: String
          -- ^ The string which @globalgrep@ attempts to locate
          -> HostName
          -- ^ The hostname of the network host which @globalgrep@
          -- searches
          -> IO [Result];
searchFor query host = map ((host ++ ": ") ++) . lines <$>
                       readProcess "ssh" [host, "grep -ri " ++ show query ++ " /"][];
