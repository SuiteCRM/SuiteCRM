<?php

/*
********************************************************
TinyButStrong Plug-in: CacheSystem for TBS => 3.0.2
Version 1.0.6, on 2008-02-29, by Skrol29
********************************************************
*/

define('TBS_CACHE','clsTbsCacheSytem');

define('TBS_DELETE', -1); // For compatibility
define('TBS_CANCEL', -2); // For compatibility
define('TBS_CACHEDELETE', -1);
define('TBS_CACHECANCEL', -2);
define('TBS_CACHENOW', -3);
define('TBS_CACHEONSHOW', -4);
define('TBS_CACHELOAD', -5);
define('TBS_CACHEGETAGE', -6);
define('TBS_CACHEGETNAME', -7);
define('TBS_CACHEISONSHOW', -8);
define('TBS_CACHEDELETEMASK', -9);

#[\AllowDynamicProperties]
class clsTbsCacheSytem {

	function OnInstall($CacheDir=false,$CacheMask=false) {
		$this->Version = '1.0.6';
		$this->ShowFromCache = false;
		$this->CacheFile = array();
		$TBS =& $this->TBS;
		if (!isset($TBS->CacheMask))  $TBS->CacheMask = 'cache_tbs_*.php'; // for compatibility
		if (!isset($TBS->CacheDir))   $TBS->CacheDir = '';
		if ($CacheMask!==false) $TBS->CacheMask = $CacheMask;
		if ($CacheDir!==false ) $TBS->CacheDir  = $CacheDir;
		return array('OnCommand','BeforeShow','AfterShow');
	}

	function OnCommand($CacheId,$Action=3600,$Dir=false) {

		$TBS =& $this->TBS;

		$CacheId = trim($CacheId);
		$Res = false;
		if ($Dir===false) $Dir = $TBS->CacheDir;
		if (!isset($this->CacheFile[$TBS->_Mode])) $this->CacheFile[$TBS->_Mode] = false;
		
		if ($Action===TBS_CACHECANCEL) { // Cancel cache save if any
			$this->CacheFile[$TBS->_Mode] = false;
		} elseif (($CacheId === '*') and ($Action===TBS_CACHEDELETE)) {
			$Res = tbs_Cache_DeleteAll($Dir,$TBS->CacheMask);
		} elseif ($Action===TBS_CACHEDELETEMASK) {
			$Res = tbs_Cache_DeleteAll($Dir,$CacheId);
		} else {
			$CacheFile = tbs_Cache_File($Dir,$CacheId,$TBS->CacheMask);
			if ($Action===TBS_CACHENOW) {
				$this->meth_Cache_Save($CacheFile,$TBS->Source);
			} elseif ($Action===TBS_CACHEGETAGE) {
				if (file_exists($CacheFile)) $Res = time()-filemtime($CacheFile);
			} elseif ($Action===TBS_CACHEGETNAME) {
				$Res = $CacheFile;
			} elseif ($Action===TBS_CACHEISONSHOW) {
				$Res = ($this->CacheFile[$TBS->_Mode]!==false);
			} elseif ($Action===TBS_CACHELOAD) {
				if (file_exists($CacheFile)) {
					if ($TBS->f_Misc_GetFile($TBS->Source,$CacheFile)) {
						$this->CacheFile[$TBS->_Mode] = $CacheFile;
						$Res = true;
					}
				}
				if ($Res===false)	$TBS->Source = '';
			} elseif ($Action===TBS_CACHEDELETE) {
				if (file_exists($CacheFile)) $Res = @unlink($CacheFile);
			} elseif ($Action===TBS_CACHEONSHOW) {
				$this->CacheFile[$TBS->_Mode] = $CacheFile;
				@touch($CacheFile);
			} elseif($Action>=0) {
				$Res = tbs_Cache_IsValide($CacheFile,$Action);
				if ($Res) { // Load the cache
					if ($TBS->f_Misc_GetFile($TBS->Source,$CacheFile)) {
						// Display cache contents
						$this->ShowFromCache = true;
						$TBS->Show();
						$this->ShowFromCache = false;
					} else {
						$TBS->meth_Misc_Alert('CacheSystem plug-in','Unable to read the file \''.$CacheFile.'\'.');
						$Res==false;
					}
					$this->CacheFile[$TBS->_Mode] = false;
				} else {
					// The result will be saved in the cache when the Show() method is called
					$this->CacheFile[$TBS->_Mode] = $CacheFile;
					@touch($CacheFile);
				}
			}
		}
	
		return $Res;
			
	}

	function BeforeShow(&$Render) {
		if ($this->ShowFromCache) return false; // Cancel automatic merges
	}

	function AfterShow(&$Render) {
		// Save cache file if planned to
		if (isset($this->CacheFile[$this->TBS->_Mode]) and is_string($this->CacheFile[$this->TBS->_Mode])) {
			$this->meth_Cache_Save($this->CacheFile[$this->TBS->_Mode],$this->TBS->Source);
		}
	}

	function meth_Cache_Save($CacheFile,&$Txt) {
		$fid = @fopen($CacheFile, 'w');
		if ($fid===false) {
			$this->TBS->meth_Misc_Alert('CacheSystem plug-in','The cache file \''.$CacheFile.'\' can not be saved.');
			return false;
		} else {
			flock($fid,2); // acquire an exlusive lock
			fwrite($fid,$Txt);
			flock($fid,3); // release the lock
			fclose($fid);
			return true;
		}
	}

}

function tbs_Cache_IsValide($CacheFile,$TimeOut) {
// Return True if there is a existing valid cache for the given file id.
	if (file_exists($CacheFile)) {
		if (time()-filemtime($CacheFile)>$TimeOut) {
			return false;
		} else {
			return true;
		}
	} else {
		return false;
	}
}

function tbs_Cache_File($Dir,$CacheId,$Mask) {
// Return the cache file path for a given Id.
	if (strlen($Dir)>0) {
		if ($Dir[strlen($Dir)-1]<>'/') {
			$Dir .= '/';
		}
	}
	return $Dir.str_replace('*',$CacheId,$Mask);
}

function tbs_Cache_DeleteAll($Dir,$Mask) {

	if (strlen($Dir)==0) {
		$Dir = '.';
	}
	if ($Dir[strlen($Dir)-1]<>'/') {
		$Dir .= '/';
	}
	$DirObj = dir($Dir);
	$Nbr = 0;

	// Get the list of cache files
	$FileLst = array();
	while ($FileName = $DirObj->read()) {
		$FullPath = $Dir.$FileName;
		if (strtolower(filetype($FullPath))==='file') {
			if (@preg_match('/^' . strtr(addcslashes($Mask, '\\.+^$(){}=!<>|'), array('*' => '.*', '?' => '.?')) . '$/i', $FileName)) {
				$FileLst[] = $FullPath;
			}
		}
	}
	// Delete all listed files
	foreach ($FileLst as $FullPath) {
		if (@unlink($FullPath)) $Nbr++;
	}

	return $Nbr;

}

?>