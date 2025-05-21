<?php
/*
********************************************************
TinyButStrong - Template Engine for Pro and Beginners
------------------------
Version  : 3.9.0 for PHP 5
Date     : 2014-01-26
Web site : http://www.tinybutstrong.com
Author   : http://www.tinybutstrong.com/onlyyou.html
********************************************************
This library is free software.
You can redistribute and modify it even for commercial usage,
but you must accept and respect the LPGL License version 3.
*/
// Check PHP version
if (version_compare(PHP_VERSION,'5.0')<0) echo '<br><b>TinyButStrong Error</b> (PHP Version Check) : Your PHP version is '.PHP_VERSION.' while TinyButStrong needs PHP version 5.0 or higher. You should try with TinyButStrong Edition for PHP 4.';

// Render flags
define('TBS_NOTHING', 0);
define('TBS_OUTPUT', 1);
define('TBS_EXIT', 2);

// Plug-ins actions
define('TBS_INSTALL', -1);
define('TBS_ISINSTALLED', -3);

// *********************************************
#[\AllowDynamicProperties]
class clsTbsLocator {
	public $PosBeg = false;
	public $PosEnd = false;
	public $Enlarged = false;
	public $FullName = false;
	public $SubName = '';
	public $SubOk = false;
	public $SubLst = array();
	public $SubNbr = 0;
	public $PrmLst = array();
	public $PrmIfNbr = false;
	public $MagnetId = false;
	public $BlockFound = false;
	public $FirstMerge = true;
	public $ConvProtect = true;
	public $ConvStr = true;
	public $ConvMode = 1; // Normal
	public $ConvBr = true;
}

// *********************************************

#[\AllowDynamicProperties]
class clsTbsDataSource {

public $Type = false;
public $SubType = 0;
public $SrcId = false;
public $Query = '';
public $RecSet = false;
public $RecKey = '';
public $RecNum = 0;
public $RecNumInit = 0;
public $RecSaving = false;
public $RecSaved = false;
public $RecBuffer = false;
public $CurrRec = false;
public $TBS = false;
public $OnDataOk = false;
public $OnDataPrm = false;
public $OnDataPrmDone = array();
public $OnDataPi = false;

public function DataAlert($Msg) {
	if (is_array($this->TBS->_CurrBlock)) {
		return $this->TBS->meth_Misc_Alert('when merging block "'.implode(',',$this->TBS->_CurrBlock).'"',$Msg);
	} else {
		return $this->TBS->meth_Misc_Alert('when merging block '.$this->TBS->_ChrOpen.$this->TBS->_CurrBlock.$this->TBS->_ChrClose,$Msg);
	}
}

public function DataPrepare(&$SrcId,&$TBS) {

	$this->SrcId = &$SrcId;
	$this->TBS = &$TBS;
	$FctInfo = false;
	$FctObj = false;

	if (is_array($SrcId)) {
		$this->Type = 0;
	} elseif (is_resource($SrcId)) {

		$Key = get_resource_type($SrcId);
		switch ($Key) {
		case 'mysql link'            : $this->Type = 6; break;
		case 'mysql link persistent' : $this->Type = 6; break;
		case 'mysql result'          : $this->Type = 6; $this->SubType = 1; break;
		case 'pgsql link'            : $this->Type = 7; break;
		case 'pgsql link persistent' : $this->Type = 7; break;
		case 'pgsql result'          : $this->Type = 7; $this->SubType = 1; break;
		case 'sqlite database'       : $this->Type = 8; break;
		case 'sqlite database (persistent)'	: $this->Type = 8; break;
		case 'sqlite result'         : $this->Type = 8; $this->SubType = 1; break;
		default :
			$FctInfo = $Key;
			$FctCat = 'r';
		}

	} elseif (is_string($SrcId)) {

		switch (strtolower($SrcId)) {
		case 'array' : $this->Type = 0; $this->SubType = 1; break;
		case 'clear' : $this->Type = 0; $this->SubType = 3; break;
		case 'mysql' : $this->Type = 6; $this->SubType = 2; break;
		case 'text'  : $this->Type = 2; break;
		case 'num'   : $this->Type = 1; break;
		default :
			$FctInfo = $SrcId;
			$FctCat = 'k';
		}

	} elseif ($SrcId instanceof Iterator) {
		$this->Type = 9; $this->SubType = 1;
	} elseif ($SrcId instanceof ArrayObject) {
		$this->Type = 9; $this->SubType = 2;
	} elseif ($SrcId instanceof IteratorAggregate) {
		$this->Type = 9; $this->SubType = 3;
	} elseif ($SrcId instanceof MySQLi) {
		$this->Type = 10;
	} elseif ($SrcId instanceof PDO) {
		$this->Type = 11;
	} elseif ($SrcId instanceof Zend_Db_Adapter_Abstract) {
		$this->Type = 12;
	} elseif (is_object($SrcId)) {
		$FctInfo = get_class($SrcId);
		$FctCat = 'o';
		$FctObj = &$SrcId;
		$this->SrcId = &$SrcId;
	} elseif ($SrcId===false) {
		$this->DataAlert('the specified source is set to FALSE. Maybe your connection has failed.');
	} else {
		$this->DataAlert('unsupported variable type : \''.gettype($SrcId).'\'.');
	}

	if ($FctInfo!==false) {
		$ErrMsg = false;
		if ($TBS->meth_Misc_UserFctCheck($FctInfo,$FctCat,$FctObj,$ErrMsg,false)) {
			$this->Type = $FctInfo['type'];
			if ($this->Type!==5) {
				if ($this->Type===4) {
					$this->FctPrm = array(false,0);
					$this->SrcId = &$FctInfo['open'][0];
				}
				$this->FctOpen  = &$FctInfo['open'];
				$this->FctFetch = &$FctInfo['fetch'];
				$this->FctClose = &$FctInfo['close'];
			}
		} else {
			$this->Type = $this->DataAlert($ErrMsg);
		}
	}

	return ($this->Type!==false);

}

public function DataOpen(&$Query,$QryPrms=false) {

	// Init values
	unset($this->CurrRec); $this->CurrRec = true;
	if ($this->RecSaved) {
		$this->FirstRec = true;
		unset($this->RecKey); $this->RecKey = '';
		$this->RecNum = $this->RecNumInit;
		if ($this->OnDataOk) $this->OnDataArgs[1] = &$this->CurrRec;
		return true;
	}
	unset($this->RecSet); $this->RecSet = false;
	$this->RecNumInit = 0;
	$this->RecNum = 0;

	if (isset($this->TBS->_piOnData)) {
		$this->OnDataPi = true;
		$this->OnDataPiRef = &$this->TBS->_piOnData;
		$this->OnDataOk = true;
	}
	if ($this->OnDataOk) {
		$this->OnDataArgs = array();
		$this->OnDataArgs[0] = &$this->TBS->_CurrBlock;
		$this->OnDataArgs[1] = &$this->CurrRec;
		$this->OnDataArgs[2] = &$this->RecNum;
		$this->OnDataArgs[3] = &$this->TBS;
	}

	switch ($this->Type) {
	case 0: // Array
		if (($this->SubType===1) && (is_string($Query))) $this->SubType = 2;
		if ($this->SubType===0) {
			$this->RecSet = &$this->SrcId;
		} elseif ($this->SubType===1) {
			if (is_array($Query)) {
				$this->RecSet = &$Query;
			} else {
				$this->DataAlert('type \''.gettype($Query).'\' not supported for the Query Parameter going with \'array\' Source Type.');
			}
		} elseif ($this->SubType===2) {
			// TBS query string for array and objects, syntax: "var[item1][item2]->item3[item4]..."
			$x = trim($Query);
			$z = chr(0);
			$x = str_replace(array(']->','][','->','['),$z,$x);
			if (substr($x,strlen($x)-1,1)===']') $x = substr($x,0,strlen($x)-1);
			$ItemLst = explode($z,$x);
			$ItemNbr = count($ItemLst);
			$Item0 = &$ItemLst[0];
			// Check first item
			if ($Item0[0]==='~') {
				$Item0 = substr($Item0,1);
				if ($this->TBS->ObjectRef!==false) {
					$Var = &$this->TBS->ObjectRef;
					$i = 0;
				} else {
					$i = $this->DataAlert('invalid query \''.$Query.'\' because property ObjectRef is not set.');
				}
			} else {
				if (isset($this->TBS->VarRef[$Item0])) {
					$Var = &$this->TBS->VarRef[$Item0];
					$i = 1;
				} else {
					$i = $this->DataAlert('invalid query \''.$Query.'\' because VarRef item \''.$Item0.'\' is not found.');
				}
			}
			// Check sub-items
			$Empty = false;
			while (($i!==false) && ($i<$ItemNbr) && ($Empty===false)) {
				$x = $ItemLst[$i];
				if (is_array($Var)) {
					if (isset($Var[$x])) {
						$Var = &$Var[$x];
					} else {
						$Empty = true;
					}
				} elseif (is_object($Var)) {
					$ArgLst = $this->TBS->f_Misc_CheckArgLst($x);
					if (method_exists($Var,$x)) {
						$f = array(&$Var,$x); unset($Var);
						$Var = call_user_func_array($f,$ArgLst);
					} elseif (property_exists(get_class($Var),$x)) {
						if (isset($Var->$x)) $Var = &$Var->$x;
					} elseif (isset($Var->$x)) {
						$Var = $Var->$x; // useful for overloaded property
					} else {
						$Empty = true;
					}
				} else {
					$i = $this->DataAlert('invalid query \''.$Query.'\' because item \''.$ItemLst[$i].'\' is neither an Array nor an Object. Its type is \''.gettype($Var).'\'.');
				}
				if ($i!==false) $i++;
			}
			// Assign data
			if ($i!==false) {
				if ($Empty) {
					$this->RecSet = array();
				} else {
					$this->RecSet = &$Var;
				}
			}
		} elseif ($this->SubType===3) { // Clear
			$this->RecSet = array();
		}
		// First record
		if ($this->RecSet!==false) {
			$this->RecNbr = $this->RecNumInit + count($this->RecSet);
			$this->FirstRec = true;
			$this->RecSaved = true;
			$this->RecSaving = false;
		}
		break;
	case 6: // MySQL
		switch ($this->SubType) {
		case 0: $this->RecSet = @mysql_query($Query,$this->SrcId); break;
		case 1: $this->RecSet = $this->SrcId; break;
		case 2: $this->RecSet = @mysql_query($Query); break;
		}
		if ($this->RecSet===false) $this->DataAlert('MySql error message when opening the query: '.mysql_error());
		break;
	case 1: // Num
		$this->RecSet = true;
		$this->NumMin = 1;
		$this->NumMax = 1;
		$this->NumStep = 1;
		if (is_array($Query)) {
			if (isset($Query['min'])) $this->NumMin = $Query['min'];
			if (isset($Query['step'])) $this->NumStep = $Query['step'];
			if (isset($Query['max'])) {
				$this->NumMax = $Query['max'];
			} else {
				$this->RecSet = $this->DataAlert('the \'num\' source is an array that has no value for the \'max\' key.');
			}
			if ($this->NumStep==0) $this->RecSet = $this->DataAlert('the \'num\' source is an array that has a step value set to zero.');
		} else {
			$this->NumMax = ceil($Query);
		}
		if ($this->RecSet) {
			if ($this->NumStep>0) {
				$this->NumVal = $this->NumMin;
			} else {
				$this->NumVal = $this->NumMax;
			}
		}
		break;
	case 2: // Text
		if (is_string($Query)) {
			$this->RecSet = &$Query;
		} else {
			$this->RecSet = $this->TBS->meth_Misc_ToStr($Query);
		}
		break;
	case 3: // Custom function
		$FctOpen = $this->FctOpen;
		$this->RecSet = $FctOpen($this->SrcId,$Query,$QryPrms);
		if ($this->RecSet===false) $this->DataAlert('function '.$FctOpen.'() has failed to open query {'.$Query.'}');
		break;
	case 4: // Custom method from ObjectRef
		$this->RecSet = call_user_func_array($this->FctOpen,array(&$this->SrcId,&$Query,&$QryPrms));
		if ($this->RecSet===false) $this->DataAlert('method '.get_class($this->FctOpen[0]).'::'.$this->FctOpen[1].'() has failed to open query {'.$Query.'}');
		break;
	case 5: // Custom method of object
		$this->RecSet = $this->SrcId->tbsdb_open($this->SrcId,$Query,$QryPrms);
		if ($this->RecSet===false) $this->DataAlert('method '.get_class($this->SrcId).'::tbsdb_open() has failed to open query {'.$Query.'}');
		break;
	case 7: // PostgreSQL
		switch ($this->SubType) {
		case 0: $this->RecSet = @pg_query($this->SrcId,$Query); break;
		case 1: $this->RecSet = $this->SrcId; break;
		}
		if ($this->RecSet===false) $this->DataAlert('PostgreSQL error message when opening the query: '.pg_last_error($this->SrcId));
		break;
	case 8: // SQLite
		switch ($this->SubType) {
		case 0: $this->RecSet = @sqlite_query($this->SrcId,$Query); break;
		case 1: $this->RecSet = $this->SrcId; break;
		}
		if ($this->RecSet===false) $this->DataAlert('SQLite error message when opening the query:'.sqlite_error_string(sqlite_last_error($this->SrcId)));
		break;
	case 9: // Iterator
		if ($this->SubType==1) {
			$this->RecSet = $this->SrcId;
		} else { // 2 or 3
			$this->RecSet = $this->SrcId->getIterator();
		}
		$this->RecSet->rewind();
		break;
	case 10: // MySQLi
		$this->RecSet = $this->SrcId->query($Query);
		if ($this->RecSet===false) $this->DataAlert('MySQLi error message when opening the query:'.$this->SrcId->error);
		break;
	case 11: // PDO
		$this->RecSet = $this->SrcId->prepare($Query);
		if ($this->RecSet===false) {
			$ok = false;
		} else {
			if (!is_array($QryPrms)) $QryPrms = array();
			$ok = $this->RecSet->execute($QryPrms);
		}
		if (!$ok) {
			$err = $this->SrcId->errorInfo();
			$this->DataAlert('PDO error message when opening the query:'.$err[2]);
		}
		break;
	case 12: // Zend_DB_Adapter
		try {
			if (!is_array($QryPrms)) $QryPrms = array();
			$this->RecSet = $this->SrcId->query($Query, $QryPrms);
		} catch (Exception $e) {
			$this->DataAlert('Zend_DB_Adapter error message when opening the query: '.$e->getMessage());
		}
		break;
	}

	if (($this->Type===0) || ($this->Type===9)) {
		unset($this->RecKey); $this->RecKey = '';
	} else {
		if ($this->RecSaving) {
			unset($this->RecBuffer); $this->RecBuffer = array();
		}
		$this->RecKey = &$this->RecNum; // Not array: RecKey = RecNum
	}

	return ($this->RecSet!==false);

}

public function DataFetch() {

	if ($this->RecSaved) {
		if ($this->RecNum<$this->RecNbr) {
			if ($this->FirstRec) {
				if ($this->SubType===2) { // From string
					reset($this->RecSet);
					$this->RecKey = key($this->RecSet);
					$this->CurrRec = &$this->RecSet[$this->RecKey];
				} else {
					$this->CurrRec = reset($this->RecSet);
					$this->RecKey = key($this->RecSet);
				}
				$this->FirstRec = false;
			} else {
				if ($this->SubType===2) { // From string
					next($this->RecSet);
					$this->RecKey = key($this->RecSet);
					$this->CurrRec = &$this->RecSet[$this->RecKey];
				} else {
					$this->CurrRec = next($this->RecSet);
					$this->RecKey = key($this->RecSet);
				}
			}
			if ((!is_array($this->CurrRec)) && (!is_object($this->CurrRec))) $this->CurrRec = array('key'=>$this->RecKey, 'val'=>$this->CurrRec);
			$this->RecNum++;
			if ($this->OnDataOk) {
				$this->OnDataArgs[1] = &$this->CurrRec; // Reference has changed if ($this->SubType===2)
				if ($this->OnDataPrm) call_user_func_array($this->OnDataPrmRef,$this->OnDataArgs);
				if ($this->OnDataPi) $this->TBS->meth_PlugIn_RunAll($this->OnDataPiRef,$this->OnDataArgs);
				if ($this->SubType!==2) $this->RecSet[$this->RecKey] = $this->CurrRec; // save modifications because array reading is done without reference :(
			}
		} else {
			unset($this->CurrRec); $this->CurrRec = false;
		}
		return;
	}

	switch ($this->Type) {
	case 6: // MySQL
		$this->CurrRec = mysql_fetch_assoc($this->RecSet);
		break;
	case 1: // Num
		if (($this->NumVal>=$this->NumMin) && ($this->NumVal<=$this->NumMax)) {
			$this->CurrRec = array('val'=>$this->NumVal);
			$this->NumVal += $this->NumStep;
		} else {
			$this->CurrRec = false;
		}
		break;
	case 2: // Text
		if ($this->RecNum===0) {
			if ($this->RecSet==='') {
				$this->CurrRec = false;
			} else {
				$this->CurrRec = &$this->RecSet;
			}
		} else {
			$this->CurrRec = false;
		}
		break;
	case 3: // Custom function
		$FctFetch = $this->FctFetch;
		$this->CurrRec = $FctFetch($this->RecSet,$this->RecNum+1);
		break;
	case 4: // Custom method from ObjectRef
		$this->FctPrm[0] = &$this->RecSet; $this->FctPrm[1] = $this->RecNum+1;
		$this->CurrRec = call_user_func_array($this->FctFetch,$this->FctPrm);
		break;
	case 5: // Custom method of object
		$this->CurrRec = $this->SrcId->tbsdb_fetch($this->RecSet,$this->RecNum+1);
		break;
	case 7: // PostgreSQL
		$this->CurrRec = pg_fetch_assoc($this->RecSet);
		break;
	case 8: // SQLite
		$this->CurrRec = sqlite_fetch_array($this->RecSet,SQLITE_ASSOC);
		break;
	case 9: // Iterator
		if ($this->RecSet->valid()) {
			$this->CurrRec = $this->RecSet->current();
			$this->RecKey = $this->RecSet->key();
			$this->RecSet->next();
		} else {
			$this->CurrRec = false;
		}
		break;
	case 10: // MySQLi
		$this->CurrRec = $this->RecSet->fetch_assoc();
		if (is_null($this->CurrRec)) $this->CurrRec = false;
		break;
	case 11: // PDO
		$this->CurrRec = $this->RecSet->fetch(PDO::FETCH_ASSOC);
		break;
	case 12: // Zend_DB_Adapater
		$this->CurrRec = $this->RecSet->fetch(Zend_Db::FETCH_ASSOC);
		break;
	}

	// Set the row count
	if ($this->CurrRec!==false) {
		$this->RecNum++;
		if ($this->OnDataOk) {
			if ($this->OnDataPrm) call_user_func_array($this->OnDataPrmRef,$this->OnDataArgs);
			if ($this->OnDataPi) $this->TBS->meth_PlugIn_RunAll($this->OnDataPiRef,$this->OnDataArgs);
		}
		if ($this->RecSaving) $this->RecBuffer[$this->RecKey] = $this->CurrRec;
	}

}

public function DataClose() {
	$this->OnDataOk = false;
	$this->OnDataPrm = false;
	$this->OnDataPi = false;
	if ($this->RecSaved) return;
	switch ($this->Type) {
	case 6: mysql_free_result($this->RecSet); break;
	case 3: $FctClose=$this->FctClose; $FctClose($this->RecSet); break;
	case 4: call_user_func_array($this->FctClose,array(&$this->RecSet)); break;
	case 5: $this->SrcId->tbsdb_close($this->RecSet); break;
	case 7: pg_free_result($this->RecSet); break;
	case 10: $this->RecSet->free(); break; // MySQLi
	//case 11: $this->RecSet->closeCursor(); break; // PDO
	}
	if ($this->RecSaving) {
		$this->RecSet = &$this->RecBuffer;
		$this->RecNbr = $this->RecNumInit + count($this->RecSet);
		$this->RecSaving = false;
		$this->RecSaved = true;
	}
}

}

// *********************************************

#[\AllowDynamicProperties]
class clsTinyButStrong {

// Public properties
public $Source = '';
public $Render = 3;
public $TplVars = array();
public $ObjectRef = false;
public $NoErr = false;
public $Assigned = array();
public $ExtendedMethods = array();
public $ErrCount = 0;
// Undocumented (can change at any version)
public $Version = '3.9.0';
public $Charset = '';
public $TurboBlock = true;
public $VarPrefix = '';
public $VarRef = null;
public $FctPrefix = '';
public $Protect = true;
public $ErrMsg = '';
public $AttDelim = false;
public $MethodsAllowed = false;
public $OnLoad = true;
public $OnShow = true;
public $IncludePath = array();
public $TplStore = array();
public $OldSubTpl = false;
// Private
public $_ErrMsgName = '';
public $_LastFile = '';
public $_CharsetFct = false;
public $_Mode = 0;
public $_CurrBlock = '';
public $_ChrOpen = '[';
public $_ChrClose = ']';
public $_ChrVal = '[val]';
public $_ChrProtect = '&#91;';
public $_PlugIns = array();
public $_PlugIns_Ok = false;
public $_piOnFrm_Ok = false;

function __construct($Options=null,$VarPrefix='',$FctPrefix='') {

	// Compatibility
	if (is_string($Options)) {
		$Chrs = $Options;
		$Options = array('var_prefix'=>$VarPrefix, 'fct_prefix'=>$FctPrefix);
		if ($Chrs!=='') {
			$Err = true;
			$Len = strlen($Chrs);
			if ($Len===2) { // For compatibility
				$Options['chr_open']  = $Chrs[0];
				$Options['chr_close'] = $Chrs[1];
				$Err = false;
			} else {
				$Pos = strpos($Chrs,',');
				if (($Pos!==false) && ($Pos>0) && ($Pos<$Len-1)) {
					$Options['chr_open']  = substr($Chrs,0,$Pos);
					$Options['chr_close'] = substr($Chrs,$Pos+1);
					$Err = false;
				}
			}
			if ($Err) $this->meth_Misc_Alert('with clsTinyButStrong() function','value \''.$Chrs.'\' is a bad tag delimitor definition.');
		}
	} 

	// Set options
	$this->VarRef = $GLOBALS;
	if (is_array($Options)) $this->SetOption($Options);

	// Links to global variables (cannot be converted to static yet because of compatibility)
	global $_TBS_FormatLst, $_TBS_UserFctLst, $_TBS_BlockAlias, $_TBS_AutoInstallPlugIns;
	if (!isset($_TBS_FormatLst))  $_TBS_FormatLst  = array();
	if (!isset($_TBS_UserFctLst)) $_TBS_UserFctLst = array();
	if (!isset($_TBS_BlockAlias)) $_TBS_BlockAlias = array();
	$this->_UserFctLst = &$_TBS_UserFctLst;

	// Auto-installing plug-ins
	if (isset($_TBS_AutoInstallPlugIns)) foreach ($_TBS_AutoInstallPlugIns as $pi) $this->PlugIn(TBS_INSTALL,$pi);

}

function __call($meth, $args) {
	if (isset($this->ExtendedMethods[$meth])) {
		if ( is_array($this->ExtendedMethods[$meth]) || is_string($this->ExtendedMethods[$meth]) ) {
			return call_user_func_array($this->ExtendedMethods[$meth], $args);
		} else {
			return call_user_func_array(array(&$this->ExtendedMethods[$meth], $meth), $args);
		}
	} else {
		$this->meth_Misc_Alert('Method not found','\''.$meth.'\' is neither a native nor an extended method of TinyButStrong.');
	}
}

function SetOption($o, $v=false, $d=false) {
	if (!is_array($o)) $o = array($o=>$v);
	if (isset($o['var_prefix'])) $this->VarPrefix = $o['var_prefix'];
	if (isset($o['fct_prefix'])) $this->FctPrefix = $o['fct_prefix'];
	if (isset($o['noerr'])) $this->NoErr = $o['noerr'];
	if (isset($o['old_subtemplate'])) $this->OldSubTpl = $o['old_subtemplate'];
	if (isset($o['auto_merge'])) {
		$this->OnLoad = $o['auto_merge'];
		$this->OnShow = $o['auto_merge'];
	}
	if (isset($o['onload'])) $this->OnLoad = $o['onload'];
	if (isset($o['onshow'])) $this->OnShow = $o['onshow'];
	if (isset($o['att_delim'])) $this->AttDelim = $o['att_delim'];
	if (isset($o['protect'])) $this->Protect = $o['protect'];
	if (isset($o['turbo_block'])) $this->TurboBlock = $o['turbo_block'];
	if (isset($o['charset'])) $this->meth_Misc_Charset($o['charset']);

	$UpdateChr = false;
	if (isset($o['chr_open'])) {
		$this->_ChrOpen = $o['chr_open'];
		$UpdateChr = true;
	}
	if (isset($o['chr_close'])) {
		$this->_ChrClose = $o['chr_close'];
		$UpdateChr = true;
	}
	if ($UpdateChr) {
		$this->_ChrVal = $this->_ChrOpen.'val'.$this->_ChrClose;
		$this->_ChrProtect = '&#'.ord($this->_ChrOpen[0]).';'.substr($this->_ChrOpen,1);
	}
	if (array_key_exists('tpl_frms',$o)) self::f_Misc_UpdateArray($GLOBALS['_TBS_FormatLst'], 'frm', $o['tpl_frms'], $d);
	if (array_key_exists('block_alias',$o)) self::f_Misc_UpdateArray($GLOBALS['_TBS_BlockAlias'], false, $o['block_alias'], $d);
	if (array_key_exists('parallel_conf',$o)) self::f_Misc_UpdateArray($GLOBALS['_TBS_ParallelLst'], false, $o['parallel_conf'], $d);
	if (array_key_exists('include_path',$o)) self::f_Misc_UpdateArray($this->IncludePath, true, $o['include_path'], $d);
	if (isset($o['render'])) $this->Render = $o['render'];
	if (isset($o['methods_allowed'])) $this->MethodsAllowed = $o['methods_allowed'];
}

function GetOption($o) {
	if ($o==='all') {
		$x = explode(',', 'var_prefix,fct_prefix,noerr,auto_merge,onload,onshow,att_delim,protect,turbo_block,charset,chr_open,chr_close,tpl_frms,block_alias,include_path,render');
		$r = array();
		foreach ($x as $o) $r[$o] = $this->GetOption($o);
		return $r;
	}
	if ($o==='var_prefix') return $this->VarPrefix;
	if ($o==='fct_prefix') return $this->FctPrefix;
	if ($o==='noerr') return $this->NoErr;
	if ($o==='auto_merge') return ($this->OnLoad && $this->OnShow);
	if ($o==='onload') return $this->OnLoad;
	if ($o==='onshow') return $this->OnShow;
	if ($o==='att_delim') return $this->AttDelim;
	if ($o==='protect') return $this->Protect;
	if ($o==='turbo_block') return $this->TurboBlock;
	if ($o==='charset') return $this->Charset;
	if ($o==='chr_open') return $this->_ChrOpen;
	if ($o==='chr_close') return $this->_ChrClose;
	if ($o==='tpl_frms') {
		// simplify the list of formats
		$x = array();
		foreach ($GLOBALS['_TBS_FormatLst'] as $s=>$i) $x[$s] = $i['Str'];
		return $x;
	}
	if ($o==='include_path') return $this->IncludePath;
	if ($o==='render') return $this->Render;
	if ($o==='methods_allowed') return $this->MethodsAllowed;
	return $this->meth_Misc_Alert('with GetOption() method','option \''.$o.'\' is not supported.');;
}

public function ResetVarRef($ToGlobal) {
	if ($ToGlobal) {
		$this->VarRef = $GLOBALS;
	} else {
		$x = array();
		$this->VarRef = &$x;
	}
}

// Public methods
public function LoadTemplate($File,$Charset='') {
	if ($File==='') {
		$this->meth_Misc_Charset($Charset);
		return true;
	}
	$Ok = true;
	if ($this->_PlugIns_Ok) {
		if (isset($this->_piBeforeLoadTemplate) || isset($this->_piAfterLoadTemplate)) {
			// Plug-ins
			$ArgLst = func_get_args();
			$ArgLst[0] = &$File;
			$ArgLst[1] = &$Charset;
			if (isset($this->_piBeforeLoadTemplate)) $Ok = $this->meth_PlugIn_RunAll($this->_piBeforeLoadTemplate,$ArgLst);
		}
	}
	// Load the file
	if ($Ok!==false) {
		if (!is_null($File)) {
			$x = '';
			if (!static::f_Misc_GetFile($x, $File, $this->_LastFile, $this->IncludePath)) return $this->meth_Misc_Alert('with LoadTemplate() method','file \''.$File.'\' is not found or not readable.');
			if ($Charset==='+') {
				$this->Source .= $x;
			} else {
				$this->Source = $x;
			}
		}
		if ($this->meth_Misc_IsMainTpl()) {
			if (!is_null($File)) $this->_LastFile = $File;
			if ($Charset!=='+') $this->TplVars = array();
			$this->meth_Misc_Charset($Charset);
		}
		// Automatic fields and blocks
		if ($this->OnLoad) $this->meth_Merge_AutoOn($this->Source,'onload',true,true);
	}
	// Plug-ins
	if ($this->_PlugIns_Ok && isset($ArgLst) && isset($this->_piAfterLoadTemplate)) $Ok = $this->meth_PlugIn_RunAll($this->_piAfterLoadTemplate,$ArgLst);
	return $Ok;
}

public function GetBlockSource($BlockName,$AsArray=false,$DefTags=true,$ReplaceWith=false) {
	$RetVal = array();
	$Nbr = 0;
	$Pos = 0;
	$FieldOutside = false;
	$P1 = false;
	$Mode = ($DefTags) ? 3 : 2;
	$PosBeg1 = 0;
	$PosEndPrec = false;
	while ($Loc = $this->meth_Locator_FindBlockNext($this->Source,$BlockName,$Pos,'.',$Mode,$P1,$FieldOutside)) {
		$Nbr++;
		$Sep = '';
		if ($Nbr==1) {
			$PosBeg1 = $Loc->PosBeg;
		} elseif (!$AsArray) {
			$Sep = substr($this->Source,$PosSep,$Loc->PosBeg-$PosSep); // part of the source between sections
		}
		$RetVal[$Nbr] = $Sep.$Loc->BlockSrc;
		$Pos = $Loc->PosEnd;
		$PosSep = $Loc->PosEnd+1;
		$P1 = false;
	}
	if ($Nbr==0) return false;
	if (!$AsArray) {
		if ($DefTags)  {
			// Return the true part of the template
			$RetVal = substr($this->Source,$PosBeg1,$Pos-$PosBeg1+1);
		} else {
			// Return the concatenated section without def tags
			$RetVal = implode('', $RetVal);
		}
	}
	if ($ReplaceWith!==false) $this->Source = substr($this->Source,0,$PosBeg1).$ReplaceWith.substr($this->Source,$Pos+1);
	return $RetVal;
}

public function MergeBlock($BlockLst,$SrcId='assigned',$Query='',$QryPrms=false) {

	if ($SrcId==='assigned') {
		$Arg = array($BlockLst,&$SrcId,&$Query,&$QryPrms);
		if (!$this->meth_Misc_Assign($BlockLst, $Arg, 'MergeBlock')) return 0;
		$BlockLst = $Arg[0]; $SrcId = &$Arg[1]; $Query = &$Arg[2];
	}

	if (is_string($BlockLst)) $BlockLst = explode(',',$BlockLst);

	if ($SrcId==='cond') {
		$Nbr = 0;
		foreach ($BlockLst as $Block) {
			$Block = trim($Block);
			if ($Block!=='') $Nbr += $this->meth_Merge_AutoOn($this->Source,$Block,true,true);
		}
		return $Nbr;
	} else {
		return $this->meth_Merge_Block($this->Source,$BlockLst,$SrcId,$Query,false,0,$QryPrms);
	}

}

public function MergeField($NameLst,$Value='assigned',$IsUserFct=false,$DefaultPrm=false) {

	$FctCheck = $IsUserFct;
	if ($PlugIn = isset($this->_piOnMergeField)) $ArgPi = array('','',&$Value,0,&$this->Source,0,0);
	$SubStart = 0;
	$Ok = true;
	$Prm = is_array($DefaultPrm);

	if ( ($Value==='assigned') && ($NameLst!=='var') && ($NameLst!=='onshow') && ($NameLst!=='onload') ) {
		$Arg = array($NameLst,&$Value,&$IsUserFct,&$DefaultPrm);
		if (!$this->meth_Misc_Assign($NameLst, $Arg, 'MergeField')) return false;
		$NameLst = $Arg[0]; $Value = &$Arg[1]; $IsUserFct = &$Arg[2]; $DefaultPrm = &$Arg[3];
	}

	$NameLst = explode(',',$NameLst);

	foreach ($NameLst as $Name) {
		$Name = trim($Name);
		$Cont = false;
		switch ($Name) {
		case '': $Cont=true;break;
		case 'onload': $this->meth_Merge_AutoOn($this->Source,'onload',true,true);$Cont=true;break;
		case 'onshow': $this->meth_Merge_AutoOn($this->Source,'onshow',true,true);$Cont=true;break;
		case 'var':	$this->meth_Merge_AutoVar($this->Source,true);$Cont=true;break;
		}
		if ($Cont) continue;
		if ($PlugIn) $ArgPi[0] = $Name;
		$PosBeg = 0;
		// Initilize the user function (only once)
		if ($FctCheck) {
			$FctInfo = $Value;
			$ErrMsg = false;
			if (!$this->meth_Misc_UserFctCheck($FctInfo,'f',$ErrMsg,$ErrMsg,false)) return $this->meth_Misc_Alert('with MergeField() method',$ErrMsg);
			$FctArg = array('','');
			$SubStart = false;
			$FctCheck = false;
		}
		while ($Loc = $this->meth_Locator_FindTbs($this->Source,$Name,$PosBeg,'.')) {
			if ($Prm) $Loc->PrmLst = array_merge($DefaultPrm,$Loc->PrmLst);
			// Apply user function
			if ($IsUserFct) {
				$FctArg[0] = &$Loc->SubName; $FctArg[1] = &$Loc->PrmLst;
				$Value = call_user_func_array($FctInfo,$FctArg);
			}
			// Plug-ins
			if ($PlugIn) {
				$ArgPi[1] = $Loc->SubName; $ArgPi[3] = &$Loc->PrmLst; $ArgPi[5] = &$Loc->PosBeg; $ArgPi[6] = &$Loc->PosEnd;
				$Ok = $this->meth_PlugIn_RunAll($this->_piOnMergeField,$ArgPi);
			}
			// Merge the field
			if ($Ok) {
				$PosBeg = $this->meth_Locator_Replace($this->Source,$Loc,$Value,$SubStart);
			} else {
				$PosBeg = $Loc->PosEnd;
			}
		}
	}
}

public function Show($Render=false) {
	$Ok = true;
	if ($Render===false) $Render = $this->Render;
	if ($this->_PlugIns_Ok) {
		if (isset($this->_piBeforeShow) || isset($this->_piAfterShow)) {
			// Plug-ins
			$ArgLst = func_get_args();
			$ArgLst[0] = &$Render;
			if (isset($this->_piBeforeShow)) $Ok = $this->meth_PlugIn_RunAll($this->_piBeforeShow,$ArgLst);
		}
	}
	if ($Ok!==false) {
		if ($this->OnShow) $this->meth_Merge_AutoOn($this->Source,'onshow',true,true);
		$this->meth_Merge_AutoVar($this->Source,true);
	}
	if ($this->_PlugIns_Ok && isset($ArgLst) && isset($this->_piAfterShow)) $this->meth_PlugIn_RunAll($this->_piAfterShow,$ArgLst);
	if ($this->_ErrMsgName!=='') $this->MergeField($this->_ErrMsgName, $this->ErrMsg);
	if ($this->meth_Misc_IsMainTpl()) {
		if (($Render & TBS_OUTPUT)==TBS_OUTPUT) echo $this->Source;
		if (($Render & TBS_EXIT)==TBS_EXIT) exit;
	} elseif ($this->OldSubTpl) {
		if (($Render & TBS_OUTPUT)==TBS_OUTPUT) echo $this->Source;
	}
	return $Ok;
}

public function PlugIn($Prm1,$Prm2=0) {

	if (is_numeric($Prm1)) {
		switch ($Prm1) {
		case TBS_INSTALL: // Try to install the plug-in
			$PlugInId = $Prm2;
			if (isset($this->_PlugIns[$PlugInId])) {
				return $this->meth_Misc_Alert('with PlugIn() method','plug-in \''.$PlugInId.'\' is already installed.');
			} else {
				$ArgLst = func_get_args();
				array_shift($ArgLst); array_shift($ArgLst);
				return $this->meth_PlugIn_Install($PlugInId,$ArgLst,false);
			}
		case TBS_ISINSTALLED: // Check if the plug-in is installed
			return isset($this->_PlugIns[$Prm2]);
		case -4: // Deactivate special plug-ins
			$this->_PlugIns_Ok_save = $this->_PlugIns_Ok;
			$this->_PlugIns_Ok = false;
			return true;
		case -5: // Deactivate OnFormat
			$this->_piOnFrm_Ok_save = $this->_piOnFrm_Ok;
			$this->_piOnFrm_Ok = false;
			return true;
		case -10:  // Restore
			if (isset($this->_PlugIns_Ok_save)) $this->_PlugIns_Ok = $this->_PlugIns_Ok_save;
			if (isset($this->_piOnFrm_Ok_save)) $this->_piOnFrm_Ok = $this->_piOnFrm_Ok_save;
			return true;
		}

	} elseif (is_string($Prm1)) {
		// Plug-in's command
		$p = strpos($Prm1,'.');
		if ($p===false) {
			$PlugInId = $Prm1;
		} else {
			$PlugInId = substr($Prm1,0,$p); // direct command
		}
		if (!isset($this->_PlugIns[$PlugInId])) {
			if (!$this->meth_PlugIn_Install($PlugInId,array(),true)) return false;
		}
		if (!isset($this->_piOnCommand[$PlugInId])) return $this->meth_Misc_Alert('with PlugIn() method','plug-in \''.$PlugInId.'\' can\'t run any command because the OnCommand event is not defined or activated.');
		$ArgLst = func_get_args();
		if ($p===false) array_shift($ArgLst);
		$Ok = call_user_func_array($this->_piOnCommand[$PlugInId],$ArgLst);
		if (is_null($Ok)) $Ok = true;
		return $Ok;
	}
	return $this->meth_Misc_Alert('with PlugIn() method','\''.$Prm1.'\' is an invalid plug-in key, the type of the value is \''.gettype($Prm1).'\'.');

}

// *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

function meth_Locator_FindTbs(&$Txt,$Name,$Pos,$ChrSub) {
// Find a TBS Locator

	$PosEnd = false;
	$PosMax = strlen($Txt) -1;
	$Start = $this->_ChrOpen.$Name;

	do {
		// Search for the opening char
		if ($Pos>$PosMax) return false;
		$Pos = strpos($Txt,$Start,$Pos);

		// If found => next chars are analyzed
		if ($Pos===false) {
			return false;
		} else {
			$Loc = new clsTbsLocator;
			$ReadPrm = false;
			$PosX = $Pos + strlen($Start);
			$x = $Txt[$PosX];

			if ($x===$this->_ChrClose) {
				$PosEnd = $PosX;
			} elseif ($x===$ChrSub) {
				$Loc->SubOk = true; // it is no longer the false value
				$ReadPrm = true;
				$PosX++;
			} elseif (strpos(';',(string) $x)!==false) {
				$ReadPrm = true;
				$PosX++;
			} else {
				$Pos++;
			}

			$Loc->PosBeg = $Pos;
			if ($ReadPrm) {
				self::f_Loc_PrmRead($Txt,$PosX,false,'\'',$this->_ChrOpen,$this->_ChrClose,$Loc,$PosEnd);
				if ($PosEnd===false) {
					$this->meth_Misc_Alert('','can\'t found the end of the tag \''.substr($Txt,$Pos,$PosX-$Pos+10).'...\'.');
					$Pos++;
				}
			}

		}

	} while ($PosEnd===false);

	$Loc->PosEnd = $PosEnd;
	if ($Loc->SubOk) {
		$Loc->FullName = $Name.'.'.$Loc->SubName;
		$Loc->SubLst = explode('.',$Loc->SubName);
		$Loc->SubNbr = count($Loc->SubLst);
	} else {
		$Loc->FullName = $Name;
	}
	if ( $ReadPrm && ( isset($Loc->PrmLst['enlarge']) || isset($Loc->PrmLst['comm']) ) ) {
		$Loc->PosBeg0 = $Loc->PosBeg;
		$Loc->PosEnd0 = $Loc->PosEnd;
		$enlarge = (isset($Loc->PrmLst['enlarge'])) ? $Loc->PrmLst['enlarge'] : $Loc->PrmLst['comm'];
		if (($enlarge===true) || ($enlarge==='')) {
			$Loc->Enlarged = self::f_Loc_EnlargeToStr($Txt,$Loc,'<!--' ,'-->');
		} else {
			$Loc->Enlarged = self::f_Loc_EnlargeToTag($Txt,$Loc,$enlarge,false);
		}
	}

	return $Loc;

}

function &meth_Locator_SectionNewBDef(&$LocR,$BlockName,$Txt,$PrmLst,$Cache) {

	$Chk = true;
	$LocLst = array();
	$LocNbr = 0;

	if ($this->_PlugIns_Ok && isset($this->_piOnCacheField)) {
		$pi = true;
		$ArgLst = array(0=>$BlockName, 1=>false, 2=>&$Txt, 3=>array('att'=>true));
	} else {
		$pi = false;
	}

	// Cache TBS locators
	$Cache = ($Cache && $this->TurboBlock);
	if ($Cache) {

		$Chk = false;
		$Pos = 0;
		$PrevEnd = -1;
		$PrevIsAMF = false; // AMF means Attribute Moved Forward
		while ($Loc = $this->meth_Locator_FindTbs($Txt,$BlockName,$Pos,'.')) {
			
			$IsAMF = false;
			$IsAtt = false;
			$NewIdx = false;
			
			if ($pi) {
				$ArgLst[1] = &$Loc;
				$this->meth_Plugin_RunAll($this->_piOnCacheField,$ArgLst);
			}

			if (($Loc->SubName==='#') || ($Loc->SubName==='$')) {
				$Loc->IsRecInfo = true;
				$Loc->RecInfo = $Loc->SubName;
				$Loc->SubName = '';
			} else {
				$Loc->IsRecInfo = false;
			}

			if ($Loc->PosBeg>$PrevEnd) { // No embedding
				if (isset($Loc->PrmLst['att'])) {
					$LocSrc = substr($Txt,$Loc->PosBeg,$Loc->PosEnd-$Loc->PosBeg+1);
					if (static::f_Xml_AttFind($Txt,$Loc,true,$this->AttDelim)) {
						if (isset($Loc->PrmLst['atttrue'])) {
							$Loc->PrmLst['magnet'] = '#';
							$Loc->PrmLst['ope'] = (isset($Loc->PrmLst['ope'])) ? $Loc->PrmLst['ope'].',attbool' : 'attbool';
						}
						$IsAtt = true;
						if ($Loc->AttForward) {
							$IsAMF = true;
						} else {
							if ($Loc->AttInsLen>0) {
								for ($i=$LocNbr;$i>0;$i--) {
									if ($LocLst[$i]->PosEnd>=$Loc->PosBeg) {
										$NewIdx = $i;
										$li = $LocLst[$i];
										$li->PosBeg += $Loc->AttInsLen;
										$li->PosEnd += $Loc->AttInsLen;
										$LocLst[$i+1] = $li;
									} else {
										$i = 0;
									}
								}
							}
						}
					} else {
						$this->meth_Misc_Alert('','TBS is not able to merge the field '.$LocSrc.' because the entity targeted by parameter \'att\' cannot be found.');
					}
					unset($Loc->PrmLst['att']);
				}
				$LocNbr++;
			} else {
				// The previous tag is embedding => no increment, then previous Loc is overwrited
				$Chk = true;
				if ($PrevIsAMF) {
					$l = &$LocLst[$LocNbr];
					$this->meth_Misc_Alert('','TBS is not able to merge the field '.$LocSrc.' because parameter \'att\' makes this fied moving forward over another TBS field.');
				}
			}

			$PrevIsAMF = false;
			if ($IsAtt) {
				$Pos = $Loc->PrevPosBeg;
				if ($IsAMF) $PrevIsAMF = true;
			} elseif ($Loc->Enlarged) {
				$Pos = $Loc->PosBeg0+1;
				$Loc->Enlarged = false;
			} else {
				$Pos = $Loc->PosBeg+1;
			}

			if ($NewIdx===false) {
				$LocLst[$LocNbr] = $Loc;
				$PrevEnd = $Loc->PosEnd;
			} else {
				$LocLst[$NewIdx] = $Loc;
				$PrevEnd = $LocLst[$LocNbr]->PosEnd;
			}

		}

	}

	// Create the object
	$o = (object) null;
	$o->Prm = $PrmLst;
	$o->LocLst = $LocLst;
	$o->LocNbr = $LocNbr;
	$o->Name = $BlockName;
	$o->Src = $Txt;
	$o->Chk = $Chk;
	$o->IsSerial = false;
	$o->AutoSub = false;
	$i = 1;
	while (isset($PrmLst['sub'.$i])) {
		$o->AutoSub = $i;
		$i++;
	}

	$LocR->BDefLst[] = &$o; // Can be usefull for plug-in
	return $o;

}

function meth_Locator_SectionAddGrp(&$LocR,$BlockName,&$BDef,$Type,$Field,$Prm) {

	$BDef->PrevValue = false;
	$BDef->Type = $Type;

	// Save sub items in a structure near to Locator.
	$Field0 = $Field;
	if (strpos($Field,(string) $this->_ChrOpen)===false) $Field = $this->_ChrOpen.$BlockName.'.'.$Field.';tbstype='.$Prm.$this->_ChrClose; // tbstype is an internal parameter for catching errors
	$BDef->FDef = &$this->meth_Locator_SectionNewBDef($LocR,$BlockName,$Field,array(),true);
	if ($BDef->FDef->LocNbr==0)	$this->meth_Misc_Alert('Parameter '.$Prm,'The value \''.$Field0.'\' is unvalide for this parameter.');

	if ($Type==='H') {
		if ($LocR->HeaderFound===false) {
			$LocR->HeaderFound = true;
			$LocR->HeaderNbr = 0;
			$LocR->HeaderDef = array(); // 1 to HeaderNbr
		}
		$i = ++$LocR->HeaderNbr;
		$LocR->HeaderDef[$i] = &$BDef;
	} else {
		if ($LocR->FooterFound===false) {
			$LocR->FooterFound = true;
			$LocR->FooterNbr = 0;
			$LocR->FooterDef = array(); // 1 to FooterNbr
		}
		$BDef->AddLastGrp = ($Type==='F');
		$i = ++$LocR->FooterNbr;
		$LocR->FooterDef[$i] = &$BDef;
	}

}

function meth_Locator_Replace(&$Txt,&$Loc,&$Value,$SubStart) {
// This function enables to merge a locator with a text and returns the position just after the replaced block
// This position can be useful because we don't know in advance how $Value will be replaced.

	// Found the value if there is a subname
	if (($SubStart!==false) && $Loc->SubOk) {
		for ($i=$SubStart;$i<$Loc->SubNbr;$i++) {
			$x = $Loc->SubLst[$i]; // &$Loc... brings an error with Event Example, I don't know why.
			if (is_array($Value)) {
				if (isset($Value[$x])) {
					$Value = &$Value[$x];
				} elseif (array_key_exists($x,$Value)) {// can happens when value is NULL
					$Value = &$Value[$x];
				} else {
					if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'item \''.$x.'\' is not an existing key in the array.',true);
					unset($Value); $Value = ''; break;
				}
			} elseif (is_object($Value)) {
				$ArgLst = static::f_Misc_CheckArgLst($x);
				if (method_exists($Value,$x)) {
					if ($this->MethodsAllowed || !in_array(strtok($Loc->FullName,'.'),array('onload','onshow','var')) ) {
						$x = call_user_func_array(array(&$Value,$x),$ArgLst);
					} else {
						if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'\''.$x.'\' is a method and the current TBS settings do not allow to call methods on automatic fields.',true);
						$x = '';	
					}
				} elseif (property_exists($Value,$x)) {
					$x = &$Value->$x;
				} elseif (isset($Value->$x)) {
					$x = $Value->$x; // useful for overloaded property
				} else {
					if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'item '.$x.'\' is neither a method nor a property in the class \''.get_class($Value).'\'.',true);
					unset($Value); $Value = ''; break;
				}
				$Value = &$x; unset($x); $x = '';
			} else {
				if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'item before \''.$x.'\' is neither an object nor an array. Its type is '.gettype($Value).'.',true);
				unset($Value); $Value = ''; break;
			}
		}
	}

	$CurrVal = $Value; // Unlink

	if (isset($Loc->PrmLst['onformat'])) {
		if ($Loc->FirstMerge) {
			$Loc->OnFrmInfo = $Loc->PrmLst['onformat'];
			$Loc->OnFrmArg = array($Loc->FullName,'',&$Loc->PrmLst,&$this);
			$ErrMsg = false;
			if (!$this->meth_Misc_UserFctCheck($Loc->OnFrmInfo,'f',$ErrMsg,$ErrMsg,true)) {
				unset($Loc->PrmLst['onformat']);
				if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'(parameter onformat) '.$ErrMsg);
				$Loc->OnFrmInfo = 'pi'; // Execute the function pi() just to avoid extra error messages
			}
		} else {
			$Loc->OnFrmArg[3] = &$this; // bugs.php.net/51174
		}
		$Loc->OnFrmArg[1] = &$CurrVal;
		if (isset($Loc->PrmLst['subtpl'])) {
			$this->meth_Misc_ChangeMode(true,$Loc,$CurrVal);
			call_user_func_array($Loc->OnFrmInfo,$Loc->OnFrmArg);
			$this->meth_Misc_ChangeMode(false,$Loc,$CurrVal);
			$Loc->ConvProtect = false;
			$Loc->ConvStr = false;
		} else {
			call_user_func_array($Loc->OnFrmInfo,$Loc->OnFrmArg);
		}
	}

	if ($Loc->FirstMerge) {
		if (isset($Loc->PrmLst['frm'])) {
			$Loc->ConvMode = 0; // Frm
			$Loc->ConvProtect = false;
		} else {
			// Analyze parameter 'strconv'
			if (isset($Loc->PrmLst['strconv'])) {
				$this->meth_Conv_Prepare($Loc, $Loc->PrmLst['strconv']);
			} elseif (isset($Loc->PrmLst['htmlconv'])) { // compatibility
				$this->meth_Conv_Prepare($Loc, $Loc->PrmLst['htmlconv']);
			} else {
				if ($this->Charset===false) $Loc->ConvStr = false; // No conversion
			}
			// Analyze parameter 'protect'
			if (isset($Loc->PrmLst['protect'])) {
				$x = strtolower($Loc->PrmLst['protect']);
				if ($x==='no') {
					$Loc->ConvProtect = false;
				} elseif ($x==='yes') {
					$Loc->ConvProtect = true;
				}
			} elseif ($this->Protect===false) {
				$Loc->ConvProtect = false;
			}
		}
		if ($Loc->Ope = isset($Loc->PrmLst['ope'])) {
			$OpeLst = explode(',',$Loc->PrmLst['ope']);
			$Loc->OpeAct = array();
			$Loc->OpeArg = array();
			$Loc->OpeUtf8 = false;
			foreach ($OpeLst as $i=>$ope) {
				if ($ope==='list') {
					$Loc->OpeAct[$i] = 1;
					$Loc->OpePrm[$i] = (isset($Loc->PrmLst['valsep'])) ? $Loc->PrmLst['valsep'] : ',';
					if (($Loc->ConvMode===1) && $Loc->ConvStr) $Loc->ConvMode = -1; // special mode for item list conversion
				} elseif ($ope==='minv') {
					$Loc->OpeAct[$i] = 11;
					$Loc->MSave = $Loc->MagnetId;
				} elseif ($ope==='attbool') { // this operation key is set when a loc is cached with paremeter atttrue
					$Loc->OpeAct[$i] = 14;
				} elseif ($ope==='utf8')  { $Loc->OpeUtf8 = true;
				} elseif ($ope==='upper') { $Loc->OpeAct[$i] = 15;
				} elseif ($ope==='lower') { $Loc->OpeAct[$i] = 16;
				} elseif ($ope==='upper1') { $Loc->OpeAct[$i] = 17;
				} elseif ($ope==='upperw') { $Loc->OpeAct[$i] = 18;
				} else {
					$x = substr($ope,0,4);
					if ($x==='max:') {
						$Loc->OpeAct[$i] = (isset($Loc->PrmLst['maxhtml'])) ? 2 : 3;
						if (isset($Loc->PrmLst['maxutf8'])) $Loc->OpeUtf8 = true;
						$Loc->OpePrm[$i] = intval(trim(substr($ope,4)));
						$Loc->OpeEnd = (isset($Loc->PrmLst['maxend'])) ? $Loc->PrmLst['maxend'] : '...';
						if ($Loc->OpePrm[$i]<=0) $Loc->Ope = false;
					} elseif ($x==='mod:') {$Loc->OpeAct[$i] = 5; $Loc->OpePrm[$i] = '0'+trim(substr($ope,4));
					} elseif ($x==='add:') {$Loc->OpeAct[$i] = 6; $Loc->OpePrm[$i] = '0'+trim(substr($ope,4));
					} elseif ($x==='mul:') {$Loc->OpeAct[$i] = 7; $Loc->OpePrm[$i] = '0'+trim(substr($ope,4));
					} elseif ($x==='div:') {$Loc->OpeAct[$i] = 8; $Loc->OpePrm[$i] = '0'+trim(substr($ope,4));
					} elseif ($x==='mok:') {$Loc->OpeAct[$i] = 9; $Loc->OpeMOK[] = trim(substr($ope,4)); $Loc->MSave = $Loc->MagnetId;
					} elseif ($x==='mko:') {$Loc->OpeAct[$i] =10; $Loc->OpeMKO[] = trim(substr($ope,4)); $Loc->MSave = $Loc->MagnetId;
					} elseif ($x==='nif:') {$Loc->OpeAct[$i] =12; $Loc->OpePrm[$i] = trim(substr($ope,4));
					} elseif ($x==='msk:') {$Loc->OpeAct[$i] =13; $Loc->OpePrm[$i] = trim(substr($ope,4));
					} elseif (isset($this->_piOnOperation)) {
						$Loc->OpeAct[$i] = 0;
						$Loc->OpePrm[$i] = $ope;
						$Loc->OpeArg[$i] = array($Loc->FullName,&$CurrVal,&$Loc->PrmLst,&$Txt,$Loc->PosBeg,$Loc->PosEnd,&$Loc);
						$Loc->PrmLst['_ope'] = $Loc->PrmLst['ope'];
					} elseif (!isset($Loc->PrmLst['noerr'])) {
						$this->meth_Misc_Alert($Loc,'parameter ope doesn\'t support value \''.$ope.'\'.',true);
					}
				}
			}
		}
		$Loc->FirstMerge = false;
	}
	$ConvProtect = $Loc->ConvProtect;

	// Plug-in OnFormat
	if ($this->_piOnFrm_Ok) {
		if (isset($Loc->OnFrmArgPi)) {
			$Loc->OnFrmArgPi[1] = &$CurrVal;
			$Loc->OnFrmArgPi[3] = &$this; // bugs.php.net/51174
		} else {
			$Loc->OnFrmArgPi = array($Loc->FullName,&$CurrVal,&$Loc->PrmLst,&$this);
		}
		$this->meth_PlugIn_RunAll($this->_piOnFormat,$Loc->OnFrmArgPi);
	}

	// Operation
	if ($Loc->Ope) {
		foreach ($Loc->OpeAct as $i=>$ope) {
			switch ($ope) {
			case 0:
				$Loc->PrmLst['ope'] = $Loc->OpePrm[$i]; // for compatibility
				$OpeArg = &$Loc->OpeArg[$i];
				$OpeArg[1] = &$CurrVal; $OpeArg[3] = &$Txt;
				if (!$this->meth_PlugIn_RunAll($this->_piOnOperation,$OpeArg)) return $Loc->PosBeg;
				break;
			case  1:
				if ($Loc->ConvMode===-1) {
					if (is_array($CurrVal)) {
						foreach ($CurrVal as $k=>$v) {
							$v = static::meth_Misc_ToStr($v);
							$this->meth_Conv_Str($v,$Loc->ConvBr);
							$CurrVal[$k] = $v;
						}
						$CurrVal = implode($Loc->OpePrm[$i],$CurrVal);
					} else {
						$CurrVal = static::meth_Misc_ToStr($CurrVal);
						$this->meth_Conv_Str($CurrVal,$Loc->ConvBr);
					}
				} else {
					if (is_array($CurrVal)) $CurrVal = implode($Loc->OpePrm[$i],$CurrVal);
				}
				break;
			case  2:
				$x = static::meth_Misc_ToStr($CurrVal);
				if (strlen($x)>$Loc->OpePrm[$i]) {
					static::f_Xml_Max($x,$Loc->OpePrm[$i],$Loc->OpeEnd);
				}
				break;
			case  3:
				$x = static::meth_Misc_ToStr($CurrVal);
				if (strlen($x)>$Loc->OpePrm[$i]) {
					if ($Loc->OpeUtf8) {
						$CurrVal = mb_substr($x,0,$Loc->OpePrm[$i],'UTF-8').$Loc->OpeEnd;
					} else {
						$CurrVal = substr($x,0,$Loc->OpePrm[$i]).$Loc->OpeEnd;
					}
				}
				break;
			case  5: $CurrVal = ('0'+$CurrVal) % $Loc->OpePrm[$i]; break;
			case  6: $CurrVal = ('0'+$CurrVal) + $Loc->OpePrm[$i]; break;
			case  7: $CurrVal = ('0'+$CurrVal) * $Loc->OpePrm[$i]; break;
			case  8: $CurrVal = ('0'+$CurrVal) / $Loc->OpePrm[$i]; break;
			case  9; case 10:
				if ($ope===9) {
				 $CurrVal = (in_array(static::meth_Misc_ToStr($CurrVal),$Loc->OpeMOK)) ? ' ' : '';
				} else {
				 $CurrVal = (in_array(static::meth_Misc_ToStr($CurrVal),$Loc->OpeMKO)) ? '' : ' ';
				} // no break here
			case 11:
				if (static::meth_Misc_ToStr($CurrVal)==='') {
					if ($Loc->MagnetId===0) $Loc->MagnetId = $Loc->MSave;
				} else {
					if ($Loc->MagnetId!==0) {
						$Loc->MSave = $Loc->MagnetId;
						$Loc->MagnetId = 0;
					}
					$CurrVal = '';
				}
				break;
			case 12: if (static::meth_Misc_ToStr($CurrVal)===$Loc->OpePrm[$i]) $CurrVal = ''; break;
			case 13: $CurrVal = str_replace('*',$CurrVal,$Loc->OpePrm[$i]); break;
			case 14: $CurrVal = self::f_Loc_AttBoolean($CurrVal, $Loc->PrmLst['atttrue'], $Loc->AttName); break;
			case 15: $CurrVal = ($Loc->OpeUtf8) ? mb_convert_case($CurrVal, MB_CASE_UPPER, 'UTF-8') : strtoupper($CurrVal); break;
			case 16: $CurrVal = ($Loc->OpeUtf8) ? mb_convert_case($CurrVal, MB_CASE_LOWER, 'UTF-8') : strtolower($CurrVal); break;
			case 17: $CurrVal = ucfirst($CurrVal); break;
			case 18: $CurrVal = ($Loc->OpeUtf8) ? mb_convert_case($CurrVal, MB_CASE_TITLE, 'UTF-8') : ucwords($CurrVal); break;
			}
		}
	}

	// String conversion or format
	if ($Loc->ConvMode===1) { // Usual string conversion
		$CurrVal = static::meth_Misc_ToStr($CurrVal);
		if ($Loc->ConvStr) $this->meth_Conv_Str($CurrVal,$Loc->ConvBr);
	} elseif ($Loc->ConvMode===0) { // Format
		$CurrVal = $this->meth_Misc_Format($CurrVal,$Loc->PrmLst);
	} elseif ($Loc->ConvMode===2) { // Special string conversion
		$CurrVal = static::meth_Misc_ToStr($CurrVal);
		if ($Loc->ConvStr) $this->meth_Conv_Str($CurrVal,$Loc->ConvBr);
		if ($Loc->ConvEsc) $CurrVal = str_replace('\'','\'\'',$CurrVal);
		if ($Loc->ConvWS) {
			$check = '  ';
			$nbsp = '&nbsp;';
			do {
				$pos = strpos($CurrVal,$check);
				if ($pos!==false) $CurrVal = substr_replace($CurrVal,$nbsp,$pos,1);
			} while ($pos!==false);
		}
		if ($Loc->ConvJS) {
			$CurrVal = addslashes($CurrVal); // apply to ('), ("), (\) and (null)
			$CurrVal = str_replace(array("\n","\r","\t"),array('\n','\r','\t'),$CurrVal);
		}
		if ($Loc->ConvUrl) $CurrVal = urlencode($CurrVal);
		// Avoid Deprecated warning: Function utf8_encode() is deprecated since 8.2
		// if ($Loc->ConvUtf8) $CurrVal = utf8_encode($CurrVal);
		if ($Loc->ConvUtf8) $CurrVal = mb_convert_encoding($CurrVal, 'UTF-8', 'ISO-8859-1');
	}

	// if/then/else process, there may be several if/then
	if ($Loc->PrmIfNbr) {
		$z = false;
		$i = 1;
		while ($i!==false) {
			if ($Loc->PrmIfVar[$i]) $Loc->PrmIfVar[$i] = $this->meth_Merge_AutoVar($Loc->PrmIf[$i],true);
			$x = str_replace($this->_ChrVal,$CurrVal,$Loc->PrmIf[$i]);
			if (static::f_Misc_CheckCondition($x)) {
				if (isset($Loc->PrmThen[$i])) {
					if ($Loc->PrmThenVar[$i]) $Loc->PrmThenVar[$i] = $this->meth_Merge_AutoVar($Loc->PrmThen[$i],true);
					$z = $Loc->PrmThen[$i];
				}
				$i = false;
			} else {
				$i++;
				if ($i>$Loc->PrmIfNbr) {
					if (isset($Loc->PrmLst['else'])) {
						if ($Loc->PrmElseVar) $Loc->PrmElseVar = $this->meth_Merge_AutoVar($Loc->PrmLst['else'],true);
						$z =$Loc->PrmLst['else'];
					}
					$i = false;
				}
			}
		}
		if ($z!==false) {
			if ($ConvProtect) {
				$CurrVal = str_replace($this->_ChrOpen,$this->_ChrProtect,$CurrVal); // TBS protection
				$ConvProtect = false;
			}
			$CurrVal = str_replace($this->_ChrVal,$CurrVal,$z);
		}
	}

	if (isset($Loc->PrmLst['file'])) {
		$x = $Loc->PrmLst['file'];
		if ($x===true) $x = $CurrVal;
		$this->meth_Merge_AutoVar($x,false);
		$x = trim(str_replace($this->_ChrVal,$CurrVal,$x));
		$CurrVal = '';
		if ($x!=='') {
			if (static::f_Misc_GetFile($CurrVal, $x, $this->_LastFile, $this->IncludePath)) {
				$this->meth_Locator_PartAndRename($CurrVal, $Loc->PrmLst);
			} else {
				if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'the file \''.$x.'\' given by parameter file is not found or not readable.',true);
			}
			$ConvProtect = false;
		}
	}

	if (isset($Loc->PrmLst['script'])) {// Include external PHP script
		$x = $Loc->PrmLst['script'];
		if ($x===true) $x = $CurrVal;
		$this->meth_Merge_AutoVar($x,false);
		$x = trim(str_replace($this->_ChrVal,$CurrVal,$x));
		if ($x!=='') {
			$this->_Subscript = $x;
			$this->CurrPrm = &$Loc->PrmLst;
			$sub = isset($Loc->PrmLst['subtpl']);
			if ($sub) $this->meth_Misc_ChangeMode(true,$Loc,$CurrVal);
			if ($this->meth_Misc_RunSubscript($CurrVal,$Loc->PrmLst)===false) {
				if (!isset($Loc->PrmLst['noerr'])) $this->meth_Misc_Alert($Loc,'the file \''.$x.'\' given by parameter script is not found or not readable.',true);
			}
			if ($sub) $this->meth_Misc_ChangeMode(false,$Loc,$CurrVal);
			$this->meth_Locator_PartAndRename($CurrVal, $Loc->PrmLst);
			unset($this->CurrPrm);
			$ConvProtect = false;
		}
	}

	if (isset($Loc->PrmLst['att'])) {
		static::f_Xml_AttFind($Txt,$Loc,true,$this->AttDelim);
		if (isset($Loc->PrmLst['atttrue'])) {
			$CurrVal = self::f_Loc_AttBoolean($CurrVal, $Loc->PrmLst['atttrue'], $Loc->AttName);
			$Loc->PrmLst['magnet'] = '#';
		}
	}

	// Case when it's an empty string
	if ($CurrVal==='') {

		if ($Loc->MagnetId===false) {
			if (isset($Loc->PrmLst['.'])) {
				$Loc->MagnetId = -1;
			} elseif (isset($Loc->PrmLst['ifempty'])) {
				$Loc->MagnetId = -2;
			} elseif (isset($Loc->PrmLst['magnet'])) {
				$Loc->MagnetId = 1;
				$Loc->PosBeg0 = $Loc->PosBeg;
				$Loc->PosEnd0 = $Loc->PosEnd;
				if ($Loc->PrmLst['magnet']==='#') {
					if (!isset($Loc->AttBeg)) {
						$Loc->PrmLst['att'] = '.';
						static::f_Xml_AttFind($Txt,$Loc,true,$this->AttDelim);
					}
					if (isset($Loc->AttBeg)) {
						$Loc->MagnetId = -3;
					} else {
						$this->meth_Misc_Alert($Loc,'parameter \'magnet=#\' cannot be processed because the corresponding attribute is not found.',true);
					}
				} elseif (isset($Loc->PrmLst['mtype'])) {
					switch ($Loc->PrmLst['mtype']) {
					case 'm+m': $Loc->MagnetId = 2; break;
					case 'm*': $Loc->MagnetId = 3; break;
					case '*m': $Loc->MagnetId = 4; break;
					}
				}
			} elseif (isset($Loc->PrmLst['attadd'])) {
				// In order to delete extra space
				$Loc->PosBeg0 = $Loc->PosBeg;
				$Loc->PosEnd0 = $Loc->PosEnd;
				$Loc->MagnetId = 5;
			} else {
				$Loc->MagnetId = 0;
			}
		}

		switch ($Loc->MagnetId) {
		case 0: break;
		case -1: $CurrVal = '&nbsp;'; break; // Enables to avoid null cells in HTML tables
		case -2: $CurrVal = $Loc->PrmLst['ifempty']; break;
		case -3: $Loc->Enlarged = true; $Loc->PosBeg = $Loc->AttBegM; $Loc->PosEnd = $Loc->AttEnd; break;
		case 1:
			$Loc->Enlarged = true;
			static::f_Loc_EnlargeToTag($Txt,$Loc,$Loc->PrmLst['magnet'],false);
			break;
		case 2:
			$Loc->Enlarged = true;
			$CurrVal = static::f_Loc_EnlargeToTag($Txt,$Loc,$Loc->PrmLst['magnet'],true);
			break;
		case 3:
			$Loc->Enlarged = true;
			$Loc2 = static::f_Xml_FindTag($Txt,$Loc->PrmLst['magnet'],true,$Loc->PosBeg,false,false,false);
			if ($Loc2!==false) {
				$Loc->PosBeg = $Loc2->PosBeg;
				if ($Loc->PosEnd<$Loc2->PosEnd) $Loc->PosEnd = $Loc2->PosEnd;
			}
			break;
		case 4:
			$Loc->Enlarged = true;
			$Loc2 = static::f_Xml_FindTag($Txt,$Loc->PrmLst['magnet'],true,$Loc->PosBeg,true,false,false);
			if ($Loc2!==false) $Loc->PosEnd = $Loc2->PosEnd;
			break;
		case 5:
			$Loc->Enlarged = true;
			if (substr($Txt,$Loc->PosBeg-1,1)==' ') $Loc->PosBeg--;
			break;
		}
		$NewEnd = $Loc->PosBeg; // Useful when mtype='m+m'
	} else {

		if ($ConvProtect) $CurrVal = str_replace($this->_ChrOpen,$this->_ChrProtect,$CurrVal); // TBS protection
		$NewEnd = $Loc->PosBeg + strlen($CurrVal);

	}

	$Txt = substr_replace($Txt,$CurrVal,$Loc->PosBeg,$Loc->PosEnd-$Loc->PosBeg+1);
	return $NewEnd; // Return the new end position of the field

}

function meth_Locator_FindBlockNext(&$Txt,$BlockName,$PosBeg,$ChrSub,$Mode,&$P1,&$FieldBefore) {
// Return the first block locator just after the PosBeg position
// Mode = 1 : Merge_Auto => doesn't save $Loc->BlockSrc, save the bounds of TBS Def tags instead, return also fields
// Mode = 2 : FindBlockLst or GetBlockSource => save $Loc->BlockSrc without TBS Def tags
// Mode = 3 : GetBlockSource => save $Loc->BlockSrc with TBS Def tags

	$SearchDef = true;
	$FirstField = false;
	// Search for the first tag with parameter "block"
	while ($SearchDef && ($Loc = $this->meth_Locator_FindTbs($Txt,$BlockName,$PosBeg,$ChrSub))) {
		if (isset($Loc->PrmLst['block'])) {
			if (isset($Loc->PrmLst['p1'])) {
				if ($P1) return false;
				$P1 = true;
			}
			$Block = $Loc->PrmLst['block'];
			$SearchDef = false;
		} elseif ($Mode===1) {
			return $Loc;
		} elseif ($FirstField===false) {
			$FirstField = $Loc;
		}
		$PosBeg = $Loc->PosEnd;
	}

	if ($SearchDef) {
		if ($FirstField!==false) $FieldBefore = true;
		return false;
	}

	$Loc->PosDefBeg = -1;

	if ($Block==='begin') { // Block definied using begin/end

		if (($FirstField!==false) && ($FirstField->PosEnd<$Loc->PosBeg)) $FieldBefore = true;

		$Opened = 1;
		while ($Loc2 = $this->meth_Locator_FindTbs($Txt,$BlockName,$PosBeg,$ChrSub)) {
			if (isset($Loc2->PrmLst['block'])) {
				switch ($Loc2->PrmLst['block']) {
				case 'end':   $Opened--; break;
				case 'begin': $Opened++; break;
				}
				if ($Opened==0) {
					if ($Mode===1) {
						$Loc->PosBeg2 = $Loc2->PosBeg;
						$Loc->PosEnd2 = $Loc2->PosEnd;
					} else {
						if ($Mode===2) {
							$Loc->BlockSrc = substr($Txt,$Loc->PosEnd+1,$Loc2->PosBeg-$Loc->PosEnd-1);
						} else {
							$Loc->BlockSrc = substr($Txt,$Loc->PosBeg,$Loc2->PosEnd-$Loc->PosBeg+1);
						}
						$Loc->PosEnd = $Loc2->PosEnd;
					}
					$Loc->BlockFound = true;
					return $Loc;
				}
			}
			$PosBeg = $Loc2->PosEnd;
		}

		return $this->meth_Misc_Alert($Loc,'a least one tag with parameter \'block=end\' is missing.',false,'in block\'s definition');

	}

	if ($Mode===1) {
		$Loc->PosBeg2 = false;
	} else {
		$beg = $Loc->PosBeg;
		$end = $Loc->PosEnd;
		if (static::f_Loc_EnlargeToTag($Txt,$Loc,$Block,false)===false) return $this->meth_Misc_Alert($Loc,'at least one tag corresponding to '.$Loc->PrmLst['block'].' is not found. Check opening tags, closing tags and embedding levels.',false,'in block\'s definition');
		if ($Loc->SubOk || ($Mode===3)) {
			$Loc->BlockSrc = substr($Txt,$Loc->PosBeg,$Loc->PosEnd-$Loc->PosBeg+1);
			$Loc->PosDefBeg = $beg - $Loc->PosBeg;
			$Loc->PosDefEnd = $end - $Loc->PosBeg;
		} else {
			$Loc->BlockSrc = substr($Txt,$Loc->PosBeg,$beg-$Loc->PosBeg).substr($Txt,$end+1,$Loc->PosEnd-$end);
		}
	}

	$Loc->BlockFound = true;
	if (($FirstField!==false) && ($FirstField->PosEnd<$Loc->PosBeg)) $FieldBefore = true;
	return $Loc; // methods return by ref by default

}

function meth_Locator_PartAndRename(&$CurrVal, &$PrmLst) {

	// Store part
	if (isset($PrmLst['store'])) {
		$storename = (isset($PrmLst['storename'])) ? $PrmLst['storename'] : 'default';
		if (!isset($this->TplStore[$storename])) $this->TplStore[$storename] = '';
		$this->TplStore[$storename] .= static::f_Xml_GetPart($CurrVal, $PrmLst['store'], false);
	}

	// Get part
	if (isset($PrmLst['getpart'])) {
		$part = $PrmLst['getpart'];
	} elseif (isset($PrmLst['getbody'])) {
		$part = $PrmLst['getbody'];
	} else {
		$part = false;
	}
	if ($part!=false) {
		$CurrVal = static::f_Xml_GetPart($CurrVal, $part, true);
	}

	// Rename or delete TBS tags names
	if (isset($PrmLst['rename'])) {
	
		$Replace = $PrmLst['rename'];

		if (is_string($Replace)) $Replace = explode(',',$Replace);
		foreach ($Replace as $x) {
			if (is_string($x)) $x = explode('=', $x);
			if (count($x)==2) {
				$old = trim($x[0]);
				$new = trim($x[1]);
				if ($old!=='') {
					if ($new==='') {
						$q = false;
						$s = 'clear';
						$this->meth_Merge_Block($CurrVal, $old, $s, $q, false, false, false);
					} else {
						$old = $this->_ChrOpen.$old;
						$old = array($old.'.', $old.' ', $old.';', $old.$this->_ChrClose);
						$new = $this->_ChrOpen.$new;
						$new = array($new.'.', $new.' ', $new.';', $new.$this->_ChrClose);
						$CurrVal = str_replace($old,$new,$CurrVal);
					}
				}
			}
		} 

	}

}

function meth_Locator_FindBlockLst(&$Txt,$BlockName,$Pos,$SpePrm) {
// Return a locator object covering all block definitions, even if there is no block definition found.

	$LocR = new clsTbsLocator;
	$LocR->P1 = false;
	$LocR->FieldOutside = false;
	$LocR->FOStop = false;
	$LocR->BDefLst = array();

	$LocR->NoData = false;
	$LocR->Special = false;
	$LocR->HeaderFound = false;
	$LocR->FooterFound = false;
	$LocR->SerialEmpty = false;
	$LocR->GrpBreak = false; // Only for plug-ins

	$LocR->WhenFound = false;
	$LocR->WhenDefault = false;

	$LocR->SectionNbr = 0;       // Normal sections
	$LocR->SectionLst = array(); // 1 to SectionNbr

	$BDef = false;
	$ParentLst = array();
	$Pid = 0;

	do {

		if ($BlockName==='') {
			$Loc = false;
		} else {
			$Loc = $this->meth_Locator_FindBlockNext($Txt,$BlockName,$Pos,'.',2,$LocR->P1,$LocR->FieldOutside);
		}

		if ($Loc===false) {

			if ($Pid>0) { // parentgrp mode => disconnect $Txt from the source
				$Parent = &$ParentLst[$Pid];
				$Src = $Txt;
				$Txt = &$Parent->Txt;
				if ($LocR->BlockFound) {
					// Redefine the Header block
					$Parent->Src = substr($Src,0,$LocR->PosBeg);
					// Add a Footer block
					$BDef = &$this->meth_Locator_SectionNewBDef($LocR,$BlockName,substr($Src,$LocR->PosEnd+1),$Parent->Prm,true);
					$this->meth_Locator_SectionAddGrp($LocR,$BlockName,$BDef,'F',$Parent->Fld,'parentgrp');
				}
				// Now go down to previous level
				$Pos = $Parent->Pos;
				$LocR->PosBeg = $Parent->Beg;
				$LocR->PosEnd = $Parent->End;
				$LocR->BlockFound = true;
				unset($Parent);
				unset($ParentLst[$Pid]);
				$Pid--;
				$Loc = true;
			}

		} else {

			$Pos = $Loc->PosEnd;

			// Define the block limits
			if ($LocR->BlockFound) {
				if ( $LocR->PosBeg > $Loc->PosBeg ) $LocR->PosBeg = $Loc->PosBeg;
				if ( $LocR->PosEnd < $Loc->PosEnd ) $LocR->PosEnd = $Loc->PosEnd;
			} else {
				$LocR->BlockFound = true;
				$LocR->PosBeg = $Loc->PosBeg;
				$LocR->PosEnd = $Loc->PosEnd;
			}

			// Merge block parameters
			if (count($Loc->PrmLst)>0) $LocR->PrmLst = array_merge($LocR->PrmLst,$Loc->PrmLst);

			// Force dynamic parameter to be cachable
			if ($Loc->PosDefBeg>=0) {
				$dynprm = array('when','headergrp','footergrp','parentgrp');
				foreach($dynprm as $dp) {
					$n = 0;
					if ((isset($Loc->PrmLst[$dp])) && (strpos($Loc->PrmLst[$dp],$this->_ChrOpen.$BlockName)!==false)) {
						$n++;
						if ($n==1) {
							$len = $Loc->PosDefEnd - $Loc->PosDefBeg + 1;
							$x = substr($Loc->BlockSrc,$Loc->PosDefBeg,$len);
						}
						$x = str_replace($Loc->PrmLst[$dp],'',$x);
					}
					if ($n>0) $Loc->BlockSrc = substr_replace($Loc->BlockSrc,$x,$Loc->PosDefBeg,$len);
				}
			}
			// Save the block and cache its tags
			$IsParentGrp = isset($Loc->PrmLst['parentgrp']);
			$BDef = &$this->meth_Locator_SectionNewBDef($LocR,$BlockName,$Loc->BlockSrc,$Loc->PrmLst,!$IsParentGrp);

			// Add the text in the list of blocks
			if (isset($Loc->PrmLst['nodata'])) { // Nodata section
				$LocR->NoData = &$BDef;
			} elseif (($SpePrm!==false) && isset($Loc->PrmLst[$SpePrm])) { // Special section (used for navigation bar)
				$LocR->Special = &$BDef;
			} elseif (isset($Loc->PrmLst['when'])) {
				if ($LocR->WhenFound===false) {
					$LocR->WhenFound = true;
					$LocR->WhenSeveral = false;
					$LocR->WhenNbr = 0;
					$LocR->WhenLst = array();
				}
				$this->meth_Merge_AutoVar($Loc->PrmLst['when'],false);
				$BDef->WhenCond = &$this->meth_Locator_SectionNewBDef($LocR,$BlockName,$Loc->PrmLst['when'],array(),true);
				$BDef->WhenBeforeNS = ($LocR->SectionNbr===0);
				$i = ++$LocR->WhenNbr;
				$LocR->WhenLst[$i] = &$BDef;
				if (isset($Loc->PrmLst['several'])) $LocR->WhenSeveral = true;
			} elseif (isset($Loc->PrmLst['default'])) {
				$LocR->WhenDefault = &$BDef;
				$LocR->WhenDefaultBeforeNS = ($LocR->SectionNbr===0);
			} elseif (isset($Loc->PrmLst['headergrp'])) {
				$this->meth_Locator_SectionAddGrp($LocR,$BlockName,$BDef,'H',$Loc->PrmLst['headergrp'],'headergrp');
			} elseif (isset($Loc->PrmLst['footergrp'])) {
				$this->meth_Locator_SectionAddGrp($LocR,$BlockName,$BDef,'F',$Loc->PrmLst['footergrp'],'footergrp');
			} elseif (isset($Loc->PrmLst['splittergrp'])) {
				$this->meth_Locator_SectionAddGrp($LocR,$BlockName,$BDef,'S',$Loc->PrmLst['splittergrp'],'splittergrp');
			} elseif ($IsParentGrp) {
				$this->meth_Locator_SectionAddGrp($LocR,$BlockName,$BDef,'H',$Loc->PrmLst['parentgrp'],'parentgrp');
				$BDef->Fld = $Loc->PrmLst['parentgrp'];
				$BDef->Txt = &$Txt;
				$BDef->Pos = $Pos;
				$BDef->Beg = $LocR->PosBeg;
				$BDef->End = $LocR->PosEnd;
				$Pid++;
				$ParentLst[$Pid] = &$BDef;
				$Txt = &$BDef->Src;
				$Pos = $Loc->PosDefBeg + 1;
				$LocR->BlockFound = false;
				$LocR->PosBeg = false;
				$LocR->PosEnd = false;
			} elseif (isset($Loc->PrmLst['serial'])) {
				// Section	with serial subsections
				$SrSrc = &$BDef->Src;
				// Search the empty item
				if ($LocR->SerialEmpty===false) {
					$SrName = $BlockName.'_0';
					$x = false;
					$SrLoc = $this->meth_Locator_FindBlockNext($SrSrc,$SrName,0,'.',2,$x,$x);
					if ($SrLoc!==false) {
						$LocR->SerialEmpty = $SrLoc->BlockSrc;
						$SrSrc = substr_replace($SrSrc,'',$SrLoc->PosBeg,$SrLoc->PosEnd-$SrLoc->PosBeg+1);
					}
				}
				$SrName = $BlockName.'_1';
				$x = false;
				$SrLoc = $this->meth_Locator_FindBlockNext($SrSrc,$SrName,0,'.',2,$x,$x);
				if ($SrLoc!==false) {
					$SrId = 1;
					do {
						// Save previous subsection
						$SrBDef = &$this->meth_Locator_SectionNewBDef($LocR,$SrName,$SrLoc->BlockSrc,$SrLoc->PrmLst,true);
						$SrBDef->SrBeg = $SrLoc->PosBeg;
						$SrBDef->SrLen = $SrLoc->PosEnd - $SrLoc->PosBeg + 1;
						$SrBDef->SrTxt = false;
						$BDef->SrBDefLst[$SrId] = &$SrBDef;
						// Put in order
						$BDef->SrBDefOrdered[$SrId] = &$SrBDef;
						$i = $SrId;
						while (($i>1) && ($SrBDef->SrBeg<$BDef->SrBDefOrdered[$SrId-1]->SrBeg)) {
							$BDef->SrBDefOrdered[$i] = &$BDef->SrBDefOrdered[$i-1];
							$BDef->SrBDefOrdered[$i-1] = &$SrBDef;
							$i--;
						}
						// Search next subsection
						$SrId++;
						$SrName = $BlockName.'_'.$SrId;
						$x = false;
						$SrLoc = $this->meth_Locator_FindBlockNext($SrSrc,$SrName,0,'.',2,$x,$x);
					} while ($SrLoc!==false);
					$BDef->SrBDefNbr = $SrId-1;
					$BDef->IsSerial = true;
					$i = ++$LocR->SectionNbr;
					$LocR->SectionLst[$i] = &$BDef;
				}
			} elseif (isset($Loc->PrmLst['parallel'])) {
				$BlockLst = $this->meth_Locator_FindParallel($Txt, $Loc->PosBeg, $Loc->PosEnd, $Loc->PrmLst['parallel']);
				if ($BlockLst) {
					// Store BDefs
					foreach ($BlockLst as $i => $Blk) {
						if ($Blk['IsRef']) {
							$PrBDef = &$BDef;
						} else {
							$PrBDef = &$this->meth_Locator_SectionNewBDef($LocR,$BlockName,$Blk['Src'],array(),true);
						}
						$PrBDef->PosBeg = $Blk['PosBeg'];
						$PrBDef->PosEnd = $Blk['PosEnd'];
						$i = ++$LocR->SectionNbr;
						$LocR->SectionLst[$i] = &$PrBDef;
					}
					$LocR->PosBeg = $BlockLst[0]['PosBeg'];
					$LocR->PosEnd = $BlockLst[$LocR->SectionNbr-1]['PosEnd'];
				}
			} else {
				// Normal section
				$i = ++$LocR->SectionNbr;
				$LocR->SectionLst[$i] = &$BDef;
			}

		}

	} while ($Loc!==false);

	if ($LocR->WhenFound && ($LocR->SectionNbr===0)) {
		// Add a blank section if When is used without a normal section
		$BDef = &$this->meth_Locator_SectionNewBDef($LocR,$BlockName,'',array(),false);
		$LocR->SectionNbr = 1;
		$LocR->SectionLst[1] = &$BDef;
	}

	return $LocR; // methods return by ref by default

}

function meth_Locator_FindParallel(&$Txt, $ZoneBeg, $ZoneEnd, $ConfId) {

	// Define configurations
	global $_TBS_ParallelLst, $_TBS_BlockAlias;

	if (!isset($_TBS_ParallelLst)) $_TBS_ParallelLst = array();

	if ( ($ConfId=='tbs:table')  && (!isset($_TBS_ParallelLst['tbs:table'])) ) {
		$_TBS_ParallelLst['tbs:table'] = array(
			'parent' => 'table',
			'ignore' => array('!--', 'caption', 'thead', 'thbody', 'thfoot'),
			'cols' => array(),
			'rows' => array('tr'),
			'cells' => array('td'=>'colspan', 'th'=>'colspan'),
		);
	}

	if (!isset($_TBS_ParallelLst[$ConfId])) return $this->meth_Misc_Alert("Parallel", "The configuration '$ConfId' is not found.");

	$conf = $_TBS_ParallelLst[$ConfId];

	$Parent = $conf['parent'];

	// Search parent bounds
	$par_o = self::f_Xml_FindTag($Txt,$Parent,true ,$ZoneBeg,false,1,false);
	if ($par_o===false) return $this->meth_Misc_Alert("Parallel", "The opening tag '$Parent' is not found.");

	$par_c = self::f_Xml_FindTag($Txt,$Parent,false,$ZoneBeg,true,-1,false);
	if ($par_c===false) return $this->meth_Misc_Alert("Parallel", "The closing tag '$Parent' is not found.");

	$SrcPOffset = $par_o->PosEnd + 1;
	$SrcP = substr($Txt, $SrcPOffset, $par_c->PosBeg - $SrcPOffset);

	// temporary variables
	$tagR = '';
	$tagC = '';
	$z = '';
	$pRO  = false;
	$pROe = false;
	$pCO  = false;
	$pCOe = false;
	$p = false;
	$Loc = new clsTbsLocator;

	$Rows  = array();
	$RowIdx = 0;
	$RefRow = false;
	$RefCellB= false;
	$RefCellE = false;
	
	$RowType = array();

	// Loop on entities inside the parent entity
	$PosR = 0;

	$mode_column = true;
	$Cells = array();
	$ColNum = 1;
	$IsRef = false;
	
	// Search for the next Row Opening tag
	while (self::f_Xml_GetNextEntityName($SrcP, $PosR, $tagR, $pRO, $p)) {

		$pROe = strpos($SrcP, '>', $p) + 1;
		$singleR = ($SrcP[$pROe-2] === '/');

		// If the tag is not a closing, a self-closing and has a name
		if ($tagR!=='') {

			if (in_array($tagR, $conf['ignore'])) {
				// This tag must be ignored
				$PosR = $p;
			} elseif (isset($conf['cols'][$tagR])) {
				// Column definition that must be merged as a cell
				if ($mode_column === false)  return $this->meth_Misc_Alert("Parallel", "There is a column definition ($tagR) after a row (".$Rows[$RowIdx-1]['tag'].").");
				if (isset($RowType['_column'])) {
					$RowType['_column']++;
				} else {
					$RowType['_column'] = 1;
				}
				$att = $conf['cols'][$tagR];
				$this->meth_Locator_FindParallelCol($SrcP, $PosR, $tagR, $pRO, $p, $SrcPOffset, $RowIdx, $ZoneBeg, $ZoneEnd, $att, $Loc, $Cells, $ColNum, $IsRef, $RefCellB, $RefCellE, $RefRow);

			} elseif (!$singleR) {

				// Search the Row Closing tag
				$locRE = self::f_Xml_FindTag($SrcP, $tagR, false, $pROe, true, -1, false);
				if ($locRE===false) return $this->meth_Misc_Alert("Parallel", "The row closing tag is not found. (tagR=$tagR, p=$p, pROe=$pROe)");

				// Inner source
				$SrcR = substr($SrcP, $pROe, $locRE->PosBeg - $pROe);
				$SrcROffset = $SrcPOffset + $pROe;

				if (in_array($tagR, $conf['rows'])) {

					if ( $mode_column && isset($RowType['_column']) ) {
						$Rows[$RowIdx] = array('tag'=>'_column', 'cells' => $Cells, 'isref' => $IsRef, 'count' => $RowType['_column']);
						$RowIdx++;
					}

					$mode_column = false;

					if (isset($RowType[$tagR])) {
						$RowType[$tagR]++;
					} else {
						$RowType[$tagR] = 1;
					}

					// Now we've got the row entity, we search for cell entities
					$Cells = array();
					$ColNum = 1;
					$PosC = 0;
					$IsRef = false;

					// Loop on Cell Opening tags
					while (self::f_Xml_GetNextEntityName($SrcR, $PosC, $tagC, $pCO, $p)) {
						if (isset($conf['cells'][$tagC]) ) {
							$att = $conf['cells'][$tagC];
							$this->meth_Locator_FindParallelCol($SrcR, $PosC, $tagC, $pCO, $p, $SrcROffset, $RowIdx, $ZoneBeg, $ZoneEnd, $att, $Loc, $Cells, $ColNum, $IsRef, $RefCellB, $RefCellE, $RefRow);
						} else {
							$PosC = $p;
						}
					}

					$Rows[$RowIdx] = array('tag'=>$tagR, 'cells' => $Cells, 'isref' => $IsRef, 'count' => $RowType[$tagR]);
					$RowIdx++;

				}

				$PosR = $locRE->PosEnd; 

			} else {
				$PosR = $pROe;
			}
		} else {
			$PosR = $pROe;
		}
	}

	//return $Rows;

	$Blocks = array();
	$rMax = count($Rows) -1;
	foreach ($Rows as $r=>$Row) {
		$Cells = $Row['cells'];
		if (isset($Cells[$RefCellB]) && $Cells[$RefCellB]['IsBegin']) {
			if ( isset($Cells[$RefCellE]) &&  $Cells[$RefCellE]['IsEnd'] ) {
				$PosBeg = $Cells[$RefCellB]['PosBeg'];
				$PosEnd = $Cells[$RefCellE]['PosEnd'];
				$Blocks[$r] = array(
					'PosBeg' => $PosBeg,
					'PosEnd' => $PosEnd,
					'IsRef'  => $Row['isref'],
					'Src' => substr($Txt, $PosBeg, $PosEnd - $PosBeg + 1),
				);
			} else {
				return $this->meth_Misc_Alert("Parallel", "At row ".$Row['count']." having entity [".$Row['tag']."], the column $RefCellE is missing or is not the last in a set of spanned columns. (The block is defined from column $RefCellB to $RefCellE)");
			}
		} else {
			return $this->meth_Misc_Alert("Parallel", "At row ".$Row['count']." having entity [".$Row['tag']."],the column $RefCellB is missing or is not the first in a set of spanned columns. (The block is defined from column $RefCellB to $RefCellE)");
		}
	}

	return $Blocks;

}

function meth_Locator_FindParallelCol($SrcR, &$PosC, $tagC, $pCO, $p, $SrcROffset, $RowIdx, $ZoneBeg, $ZoneEnd, &$att, &$Loc, &$Cells, &$ColNum, &$IsRef, &$RefCellB, &$RefCellE, &$RefRow) {

	$pCOe = false;

	// Read parameters
	$Loc->PrmLst = array();
	self::f_Loc_PrmRead($SrcR,$p,true,'\'"','<','>',$Loc,$pCOe,true);

	$singleC = ($SrcR[$pCOe-1] === '/');
	if ($singleC) {
		$pCEe = $pCOe;
	} else {
		// Find the Cell Closing tag
		$locCE = self::f_Xml_FindTag($SrcR, $tagC, false, $pCOe, true, -1, false);
		if ($locCE===false) return $this->meth_Misc_Alert("Parallel", "The cell closing tag is not found. (pCOe=$pCOe)");
		$pCEe = $locCE->PosEnd;
	}
	
	// Check the cell of reference
	$Width = (isset($Loc->PrmLst[$att])) ? intval($Loc->PrmLst[$att]) : 1;
	$ColNumE = $ColNum + $Width -1; // Ending Cell
	$PosBeg = $SrcROffset + $pCO;
	$PosEnd = $SrcROffset + $pCEe;
	$OnZone = false;
	if ( ($PosBeg <= $ZoneBeg) && ($ZoneBeg <= $PosEnd) && ($RefRow===false) ) {
		$RefRow = $RowIdx;
		$RefCellB = $ColNum;
		$OnZone = true;
		$IsRef = true;
	}
	if ( ($PosBeg <= $ZoneEnd) && ($ZoneEnd <= $PosEnd) ) {
		$RefCellE = $ColNum;
		$OnZone = true;
	}
	
	// Save info
	$Cell = array(
		//'_tagR' => $tagR, '_tagC' => $tagC, '_att' => $att, '_OnZone' => $OnZone, '_PrmLst' => $Loc->PrmLst, '_Offset' => $SrcROffset, '_Src' => substr($SrcR, $pCO, $locCE->PosEnd - $pCO + 1),
		'PosBeg' => $PosBeg,
		'PosEnd' => $PosEnd,
		'ColNum' => $ColNum,
		'Width' => $Width,
		'IsBegin' => true,
		'IsEnd' => false,
	);
	$Cells[$ColNum] = $Cell;
	
	// add a virtual column to say if its a ending
	if (!isset($Cells[$ColNumE])) $Cells[$ColNumE] = array('IsBegin' => false);
	
	$Cells[$ColNumE]['IsEnd'] = true;
	$Cells[$ColNumE]['PosEnd'] = $Cells[$ColNum]['PosEnd'];
	
	$PosC = $pCEe;
	$ColNum += $Width;

}

function meth_Merge_Block(&$Txt,$BlockLst,&$SrcId,&$Query,$SpePrm,$SpeRecNum,$QryPrms=false) {

	$BlockSave = $this->_CurrBlock;
	$this->_CurrBlock = $BlockLst;

	// Get source type and info
	$Src = new clsTbsDataSource;
	if (!$Src->DataPrepare($SrcId,$this)) {
		$this->_CurrBlock = $BlockSave;
		return 0;
	}

	if (is_string($BlockLst)) $BlockLst = explode(',', $BlockLst);
	$BlockNbr = count($BlockLst);
	$BlockId = 0;
	$WasP1 = false;
	$NbrRecTot = 0;
	$QueryZ = &$Query;
	$ReturnData = false;

	while ($BlockId<$BlockNbr) {

		$RecSpe = 0;  // Row with a special block's definition (used for the navigation bar)
		$QueryOk = true;
		$this->_CurrBlock = trim($BlockLst[$BlockId]);
		if ($this->_CurrBlock==='*') {
			$ReturnData = true;
			if ($Src->RecSaved===false) $Src->RecSaving = true;
			$this->_CurrBlock = '';
		}

		// Search the block
		$LocR = $this->meth_Locator_FindBlockLst($Txt,$this->_CurrBlock,0,$SpePrm);

		if ($LocR->BlockFound) {

			if ($LocR->Special!==false) $RecSpe = $SpeRecNum;
			// OnData
			if ($Src->OnDataPrm = isset($LocR->PrmLst['ondata'])) {
				$Src->OnDataPrmRef = $LocR->PrmLst['ondata'];
				if (isset($Src->OnDataPrmDone[$Src->OnDataPrmRef])) {
					$Src->OnDataPrm = false;
				} else {
					$ErrMsg = false;
					if ($this->meth_Misc_UserFctCheck($Src->OnDataPrmRef,'f',$ErrMsg,$ErrMsg,true)) {
						$Src->OnDataOk = true;
					} else {
						$LocR->FullName = $this->_CurrBlock;
						$Src->OnDataPrm = $this->meth_Misc_Alert($LocR,'(parameter ondata) '.$ErrMsg,false,'block');
					}
				}
			}
			// Dynamic query
			if ($LocR->P1) {
				if ( ($LocR->PrmLst['p1']===true) && ((!is_string($Query)) || (strpos($Query,'%p1%')===false)) ) { // p1 with no value is a trick to perform new block with same name
					if ($Src->RecSaved===false) $Src->RecSaving = true;
				} elseif (is_string($Query)) {
					$Src->RecSaved = false;
					unset($QueryZ); $QueryZ = ''.$Query;
					$i = 1;
					do {
						$x = 'p'.$i;
						if (isset($LocR->PrmLst[$x])) {
							$QueryZ = str_replace('%p'.$i.'%',$LocR->PrmLst[$x],$QueryZ);
							$i++;
						} else {
							$i = false;
						}
					} while ($i!==false);
				}
				$WasP1 = true;
			} elseif (($Src->RecSaved===false) && ($BlockNbr-$BlockId>1)) {
				$Src->RecSaving = true;
			}
		} elseif ($WasP1) {
			$QueryOk = false;
			$WasP1 = false;
		}

		// Open the recordset
		if ($QueryOk) {
			if ((!$LocR->BlockFound) && (!$LocR->FieldOutside)) {
				// Special case: return data without any block to merge
				$QueryOk = false;
				if ($ReturnData && (!$Src->RecSaved)) {
					if ($Src->DataOpen($QueryZ,$QryPrms)) {
						do {$Src->DataFetch();} while ($Src->CurrRec!==false);
						$Src->DataClose();
					}
				}
			}	else {
				$QueryOk = $Src->DataOpen($QueryZ,$QryPrms);
				if (!$QueryOk) {
					if ($WasP1) {	$WasP1 = false;} else {$LocR->FieldOutside = false;} // prevent from infinit loop
				}
			}
		}

		// Merge sections
		if ($QueryOk) {
			if ($Src->Type===2) { // Special for Text merge
				if ($LocR->BlockFound) {
					$Txt = substr_replace($Txt,$Src->RecSet,$LocR->PosBeg,$LocR->PosEnd-$LocR->PosBeg+1);
					$Src->DataFetch(); // store data, may be needed for multiple blocks
					$Src->RecNum = 1;
					$Src->CurrRec = false;
				} else {
					$Src->DataAlert('can\'t merge the block with a text value because the block definition is not found.');
				}
			} elseif ($LocR->BlockFound===false) {
				$Src->DataFetch(); // Merge first record only
			} elseif (isset($LocR->PrmLst['parallel'])) {
				$this->meth_Merge_BlockParallel($Txt,$LocR,$Src);
			} else {
				$this->meth_Merge_BlockSections($Txt,$LocR,$Src,$RecSpe);
			}
			$Src->DataClose(); // Close the resource
		}

		if (!$WasP1) {
			$NbrRecTot += $Src->RecNum;
			$BlockId++;
		}
		if ($LocR->FieldOutside) $this->meth_Merge_FieldOutside($Txt,$Src->CurrRec,$Src->RecNum,$LocR->FOStop);

	}

	// End of the merge
	unset($LocR);
	$this->_CurrBlock = $BlockSave;
	if ($ReturnData) {
		return $Src->RecSet;
	} else {
		unset($Src);
		return $NbrRecTot;
	}

}

function meth_Merge_BlockParallel(&$Txt,&$LocR,&$Src) {

	// Main loop
	$Src->DataFetch();

	$FirstRec = true;
	
	// Prepare sources
	$BlockRes = array();
	for ($i=1 ; $i<=$LocR->SectionNbr ; $i++) {
		if ($i>1) {
			// Add txt source between the BDefs
			$BlockRes[$i] = substr($Txt, $LocR->SectionLst[$i-1]->PosEnd + 1, $LocR->SectionLst[$i]->PosBeg - $LocR->SectionLst[$i-1]->PosEnd -1); 
		} else {
			$BlockRes[$i] = '';
		}
	}
	
	while($Src->CurrRec!==false) {
		// Merge the current record with all sections
		for ($i=1 ; $i<=$LocR->SectionNbr ; $i++) {
			$SecDef = &$LocR->SectionLst[$i];
			$SecSrc = $this->meth_Merge_SectionNormal($SecDef,$Src);
			$BlockRes[$i] .= $SecSrc;
		}
		// Next row
		$Src->DataFetch();
	}
	
	$BlockRes = implode('', $BlockRes);
	$Txt = substr_replace($Txt,$BlockRes,$LocR->PosBeg,$LocR->PosEnd-$LocR->PosBeg+1);

}

function meth_Merge_BlockSections(&$Txt,&$LocR,&$Src,&$RecSpe) {

	// Initialise
	$SecId = 0;
	$SecOk = ($LocR->SectionNbr>0);
	$SecSrc = '';
	$BlockRes = ''; // The result of the chained merged blocks
	$IsSerial = false;
	$SrId = 0;
	$SrNbr = 0;
	$GrpFound = false;
	if ($LocR->HeaderFound || $LocR->FooterFound) {
		$GrpFound = true;
		$piOMG = false;
		if ($LocR->FooterFound) $Src->PrevRec = (object) null;
	}
  // Plug-ins
	$piOMS = false;
	if ($this->_PlugIns_Ok) {
		if (isset($this->_piBeforeMergeBlock)) {
			$ArgLst = array(&$Txt,&$LocR->PosBeg,&$LocR->PosEnd,$LocR->PrmLst,&$Src,&$LocR);
			$this->meth_Plugin_RunAll($this->_piBeforeMergeBlock,$ArgLst);
		}
		if (isset($this->_piOnMergeSection)) {
			$ArgLst = array(&$BlockRes,&$SecSrc);
			$piOMS = true;
		}
		if ($GrpFound && isset($this->_piOnMergeGroup)) {
			$ArgLst2 = array(0,0,&$Src,&$LocR);
			$piOMG = true;
		}
	}

	// Main loop
	$Src->DataFetch();

	// STIC-Custom 20230903 EPS - Only one tag w:body will be used in the document
	// https://github.com/SinergiaTIC/SinergiaCRM/pull/29
	$bodyPresent = false;
	// END STIC-Custom

	while($Src->CurrRec!==false) {

		// Headers and Footers
		if ($GrpFound) {
			$brk_any = false;
			$brk_src = '';
			if ($LocR->FooterFound) {
				$brk = false;
				for ($i=$LocR->FooterNbr;$i>=1;$i--) {
					$GrpDef = &$LocR->FooterDef[$i];
					$x = $this->meth_Merge_SectionNormal($GrpDef->FDef,$Src);
					if ($Src->RecNum===1) {
						$GrpDef->PrevValue = $x;
						$brk_i = false;
					} else {
						if ($GrpDef->AddLastGrp) {
							$brk_i = &$brk;
						} else {
							unset($brk_i); $brk_i = false;
						}
						if (!$brk_i) $brk_i = !($GrpDef->PrevValue===$x);
						if ($brk_i) {
							$brk_any = true;
							$ok = true;
							if ($piOMG) {$ArgLst2[0]=&$Src->PrevRec; $ArgLst2[1]=&$GrpDef; $ok = $this->meth_PlugIn_RunAll($this->_piOnMergeGroup,$ArgLst2);}
							if ($ok!==false) $brk_src = $this->meth_Merge_SectionNormal($GrpDef,$Src->PrevRec).$brk_src;
							$GrpDef->PrevValue = $x;
						}
					}
				}
				$Src->PrevRec->CurrRec = $Src->CurrRec;
				$Src->PrevRec->RecNum = $Src->RecNum;
				$Src->PrevRec->RecKey = $Src->RecKey;
			}
			if ($LocR->HeaderFound) {
				$brk = ($Src->RecNum===1);
				for ($i=1;$i<=$LocR->HeaderNbr;$i++) {
					$GrpDef = &$LocR->HeaderDef[$i];
					$x = $this->meth_Merge_SectionNormal($GrpDef->FDef,$Src);
					if (!$brk) $brk = !($GrpDef->PrevValue===$x);
					if ($brk) {
						$ok = true;
						if ($piOMG) {$ArgLst2[0]=&$Src; $ArgLst2[1]=&$GrpDef; $ok = $this->meth_PlugIn_RunAll($this->_piOnMergeGroup,$ArgLst2);}
						if ($ok!==false) $brk_src .= $this->meth_Merge_SectionNormal($GrpDef,$Src);
						$GrpDef->PrevValue = $x;
					}
				}
				$brk_any = ($brk_any || $brk);
			}
			if ($brk_any) {
				if ($IsSerial) {
					$BlockRes .= $this->meth_Merge_SectionSerial($SecDef,$SrId,$LocR);
					$IsSerial = false;
				}
				$BlockRes .= $brk_src;
			}
		} // end of header and footer

		// Increment Section
		if (($IsSerial===false) && $SecOk) {
			$SecId++;
			if ($SecId>$LocR->SectionNbr) $SecId = 1;
			$SecDef = &$LocR->SectionLst[$SecId];
			$IsSerial = $SecDef->IsSerial;
			if ($IsSerial) {
				$SrId = 0;
				$SrNbr = $SecDef->SrBDefNbr;
			}
		}

		// Serial Mode Activation
		if ($IsSerial) { // Serial Merge
			$SrId++;
			$SrBDef = &$SecDef->SrBDefLst[$SrId];
			$SrBDef->SrTxt = $this->meth_Merge_SectionNormal($SrBDef,$Src);
			if ($SrId>=$SrNbr) {
				$SecSrc = $this->meth_Merge_SectionSerial($SecDef,$SrId,$LocR);
				$BlockRes .= $SecSrc;
				$IsSerial = false;
			}
		} else { // Classic merge
			if ($SecOk) {
				if ($Src->RecNum===$RecSpe) $SecDef = &$LocR->Special;
				$SecSrc = $this->meth_Merge_SectionNormal($SecDef,$Src);
			} else {
				$SecSrc = '';
			}
			if ($LocR->WhenFound) { // With conditional blocks
				$found = false;
				$continue = true;
				$i = 1;
				do {
					$WhenBDef = &$LocR->WhenLst[$i];
					$cond = $this->meth_Merge_SectionNormal($WhenBDef->WhenCond,$Src);
					if (static::f_Misc_CheckCondition($cond)) {
						$x_when = $this->meth_Merge_SectionNormal($WhenBDef,$Src);
						if ($WhenBDef->WhenBeforeNS) {$SecSrc = $x_when.$SecSrc;} else {$SecSrc = $SecSrc.$x_when;}
						$found = true;
						if ($LocR->WhenSeveral===false) $continue = false;
					}
					$i++;
					if ($i>$LocR->WhenNbr) $continue = false;
				} while ($continue);
				if (($found===false) && ($LocR->WhenDefault!==false)) {
					$x_when = $this->meth_Merge_SectionNormal($LocR->WhenDefault,$Src);
					if ($LocR->WhenDefaultBeforeNS) {$SecSrc = $x_when.$SecSrc;} else {$SecSrc = $SecSrc.$x_when;}
				}
			}
			if ($piOMS) $this->meth_PlugIn_RunAll($this->_piOnMergeSection,$ArgLst);

			// STIC-Custom 20230903 EPS - Only one tag w:body will be used in the document
			if ($bodyPresent){
				$SecSrc = str_replace('<w:body>', '', $SecSrc);
				$BlockRes = str_replace('</w:body>', '', $BlockRes);
			}
			$BlockRes .= $SecSrc;
			$bodyPresent = true;
			//END STIC-Custom
		}

		// Next row
		$Src->DataFetch();

	} //--> while($CurrRec!==false) {

	$SecSrc = '';

	// Serial: merge the extra the sub-blocks
	if ($IsSerial) $SecSrc .= $this->meth_Merge_SectionSerial($SecDef,$SrId,$LocR);

	// Footer
	if ($LocR->FooterFound) {
		if ($Src->RecNum>0) {
			for ($i=1;$i<=$LocR->FooterNbr;$i++) {
				$GrpDef = &$LocR->FooterDef[$i];
				if ($GrpDef->AddLastGrp) {
					$ok = true;
					if ($piOMG) {$ArgLst2[0]=&$Src->PrevRec; $ArgLst2[1]=&$GrpDef; $ok = $this->meth_PlugIn_RunAll($this->_piOnMergeGroup,$ArgLst2);}
					if ($ok!==false) $SecSrc .= $this->meth_Merge_SectionNormal($GrpDef,$Src->PrevRec);
				}
			}
		}
	}

	// NoData
	if ($Src->RecNum===0) {
		if ($LocR->NoData!==false) {
			$SecSrc = $LocR->NoData->Src;
		} elseif(isset($LocR->PrmLst['bmagnet'])) {
			static::f_Loc_EnlargeToTag($Txt,$LocR,$LocR->PrmLst['bmagnet'],false);
		}
	}

	// Plug-ins
	if ($piOMS && ($SecSrc!=='')) $this->meth_PlugIn_RunAll($this->_piOnMergeSection,$ArgLst);

	$BlockRes .= $SecSrc;

	// Plug-ins
	if ($this->_PlugIns_Ok && isset($ArgLst) && isset($this->_piAfterMergeBlock)) {
		$ArgLst = array(&$BlockRes,&$Src,&$LocR);
		$this->meth_PlugIn_RunAll($this->_piAfterMergeBlock,$ArgLst);
	}

	// Merge the result
	$Txt = substr_replace($Txt,$BlockRes,$LocR->PosBeg,$LocR->PosEnd-$LocR->PosBeg+1);
	if ($LocR->P1) $LocR->FOStop = $LocR->PosBeg + strlen($BlockRes) -1;

}

function meth_Merge_AutoVar(&$Txt,$ConvStr,$Id='var') {
// Merge automatic fields with VarRef

	$Pref = &$this->VarPrefix;
	$PrefL = strlen($Pref);
	$PrefOk = ($PrefL>0);

	if ($ConvStr===false) {
		$Charset = $this->Charset;
		$this->Charset = false;
	}

	// Then we scann all fields in the model
	$x = '';
	$Pos = 0;
	while ($Loc = $this->meth_Locator_FindTbs($Txt,$Id,$Pos,'.')) {
		if ($Loc->SubNbr==0) $Loc->SubLst[0]=''; // In order to force error message
		if ($Loc->SubLst[0]==='') {
			$Pos = $this->meth_Merge_AutoSpe($Txt,$Loc);
		} elseif ($Loc->SubLst[0][0]==='~') {
			if (!isset($ObjOk)) $ObjOk = (is_object($this->ObjectRef) || is_array($this->ObjectRef));
			if ($ObjOk) {
				$Loc->SubLst[0] = substr($Loc->SubLst[0],1);
				$Pos = $this->meth_Locator_Replace($Txt,$Loc,$this->ObjectRef,0);
			} elseif (isset($Loc->PrmLst['noerr'])) {
				$Pos = $this->meth_Locator_Replace($Txt,$Loc,$x,false);
			} else {
				$this->meth_Misc_Alert($Loc,'property ObjectRef is neither an object nor an array. Its type is \''.gettype($this->ObjectRef).'\'.',true);
				$Pos = $Loc->PosEnd + 1;
			}
		} elseif ($PrefOk && (substr($Loc->SubLst[0],0,$PrefL)!==$Pref)) {
			if (isset($Loc->PrmLst['noerr'])) {
				$Pos = $this->meth_Locator_Replace($Txt,$Loc,$x,false);
			} else {
				$this->meth_Misc_Alert($Loc,'does not match the allowed prefix.',true);
				$Pos = $Loc->PosEnd + 1;
			}
		} elseif (isset($this->VarRef[$Loc->SubLst[0]])) {
			$Pos = $this->meth_Locator_Replace($Txt,$Loc,$this->VarRef[$Loc->SubLst[0]],1);
		} else {
			if (isset($Loc->PrmLst['noerr'])) {
				$Pos = $this->meth_Locator_Replace($Txt,$Loc,$x,false);
			} else {
				$Pos = $Loc->PosEnd + 1;
				$msg = (isset($this->VarRef['GLOBALS'])) ? 'VarRef seems refers to $GLOBALS' : 'VarRef seems refers to a custom array of values';
				$this->meth_Misc_Alert($Loc,'the key \''.$Loc->SubLst[0].'\' does not exist or is not set in VarRef. ('.$msg.')',true);
			}
		}
	}

	if ($ConvStr===false) $this->Charset = $Charset;

	return false; // Useful for properties PrmIfVar & PrmThenVar

}

function meth_Merge_AutoSpe(&$Txt,&$Loc) {
// Merge Special Var Fields ([var..*])

	$ErrMsg = false;
	$SubStart = false;
	if (isset($Loc->SubLst[1])) {
		switch ($Loc->SubLst[1]) {
		case 'now': $x = time(); break;
		case 'version': $x = $this->Version; break;
		case 'script_name': $x = basename(((isset($_SERVER)) ? $_SERVER['PHP_SELF'] : $GLOBALS['HTTP_SERVER_VARS']['PHP_SELF'] )); break;
		case 'template_name': $x = $this->_LastFile; break;
		case 'template_date': $x = ''; if (static::f_Misc_GetFile($x,$this->_LastFile,'',array(),false)) $x = $x['mtime']; break;
		case 'template_path': $x = dirname($this->_LastFile).'/'; break;
		case 'name': $x = 'TinyButStrong'; break;
		case 'logo': $x = '**TinyButStrong**'; break;
		case 'charset': $x = $this->Charset; break;
		case 'error_msg': $this->_ErrMsgName = $Loc->FullName; return $Loc->PosEnd;	break;
		case '': $ErrMsg = 'it doesn\'t have any keyword.'; break;
		case 'tplvars':
			if ($Loc->SubNbr==2) {
				$SubStart = 2;
				$x = implode(',',array_keys($this->TplVars)); // list of all template variables
			} else {
				if (isset($this->TplVars[$Loc->SubLst[2]])) {
					$SubStart = 3;
					$x = &$this->TplVars[$Loc->SubLst[2]];
				} else {
					$ErrMsg = 'property TplVars doesn\'t have any item named \''.$Loc->SubLst[2].'\'.';
				}
			}
			break;
		case 'store':
			if ($Loc->SubNbr==2) {
				$SubStart = 2;
				$x = implode('',$this->TplStore); // concatenation of all stores
			} else {
				if (isset($this->TplStore[$Loc->SubLst[2]])) {
					$SubStart = 3;
					$x = &$this->TplStore[$Loc->SubLst[2]];
				} else {
					$ErrMsg = 'Store named \''.$Loc->SubLst[2].'\' is not defined yet.';
				}
			}
			if (!isset($Loc->PrmLst['strconv'])) {$Loc->PrmLst['strconv'] = 'no'; $Loc->PrmLst['protect'] = 'no';}
			break;
		case 'cst': $x = @constant($Loc->SubLst[2]); break;
		case 'tbs_info':
			$x = 'TinyButStrong version '.$this->Version.' for PHP 5';
			$x .= "\r\nInstalled plug-ins: ".count($this->_PlugIns);
			foreach (array_keys($this->_PlugIns) as $pi) {
				$o = &$this->_PlugIns[$pi];
				$x .= "\r\n- plug-in [".(isset($o->Name) ? $o->Name : $pi ).'] version '.(isset($o->Version) ? $o->Version : '?' );
			}
			break;
		case 'php_info':
			ob_start();
			phpinfo();
			$x = ob_get_contents();
			ob_end_clean();
			$x = self::f_Xml_GetPart($x, '(style)+body', false);
			if (!isset($Loc->PrmLst['strconv'])) {$Loc->PrmLst['strconv'] = 'no'; $Loc->PrmLst['protect'] = 'no';}
			break;
		default:
			$IsSupported = false;
			if (isset($this->_piOnSpecialVar)) {
				$x = '';
				$ArgLst = array(substr($Loc->SubName,1),&$IsSupported ,&$x, &$Loc->PrmLst,&$Txt,&$Loc->PosBeg,&$Loc->PosEnd,&$Loc);
				$this->meth_PlugIn_RunAll($this->_piOnSpecialVar,$ArgLst);
			}
			if (!$IsSupported) $ErrMsg = '\''.$Loc->SubLst[1].'\' is an unsupported keyword.';
		}
	} else {
		$ErrMsg = 'it doesn\'t have any subname.';
	}
	if ($ErrMsg!==false) {
		$this->meth_Misc_Alert($Loc,$ErrMsg);
		$x = '';
	}
	if ($Loc->PosBeg===false) {
		return $Loc->PosEnd;
	} else {
		return $this->meth_Locator_Replace($Txt,$Loc,$x,$SubStart);
	}
}

function meth_Merge_FieldOutside(&$Txt, &$CurrRec, $RecNum, $PosMax) {
	$Pos = 0;
	$SubStart = ($CurrRec===false) ? false : 0;
	do {
		$Loc = $this->meth_Locator_FindTbs($Txt,$this->_CurrBlock,$Pos,'.');
		if ($Loc!==false) {
			if (($PosMax!==false) && ($Loc->PosEnd>$PosMax)) return;
			if ($Loc->SubName==='#') {
				$NewEnd = $this->meth_Locator_Replace($Txt,$Loc,$RecNum,false);
			} else {
				$NewEnd = $this->meth_Locator_Replace($Txt,$Loc,$CurrRec,$SubStart);
			}
			if ($PosMax!==false) $PosMax += $NewEnd - $Loc->PosEnd;
			$Pos = $NewEnd;
		}
	} while ($Loc!==false);
}

function meth_Merge_SectionNormal(&$BDef,&$Src) {

	$Txt = $BDef->Src;
	$LocLst = &$BDef->LocLst;
	$iMax = $BDef->LocNbr;
	$PosMax = strlen($Txt);

	if ($Src===false) { // Erase all fields

		$x = '';

		// Chached locators
		for ($i=$iMax;$i>0;$i--) {
			if ($LocLst[$i]->PosBeg<$PosMax) {
				$this->meth_Locator_Replace($Txt,$LocLst[$i],$x,false);
				if ($LocLst[$i]->Enlarged) {
					$PosMax = $LocLst[$i]->PosBeg;
					$LocLst[$i]->PosBeg = $LocLst[$i]->PosBeg0;
					$LocLst[$i]->PosEnd = $LocLst[$i]->PosEnd0;
					$LocLst[$i]->Enlarged = false;
				}
			}
		}

		// Unchached locators
		if ($BDef->Chk) {
			$BlockName = &$BDef->Name;
			$Pos = 0;
			while ($Loc = $this->meth_Locator_FindTbs($Txt,$BlockName,$Pos,'.')) $Pos = $this->meth_Locator_Replace($Txt,$Loc,$x,false);
		}

	} else {

		// Chached locators
		for ($i=$iMax;$i>0;$i--) {
			if ($LocLst[$i]->PosBeg<$PosMax) {
				if ($LocLst[$i]->IsRecInfo) {
					if ($LocLst[$i]->RecInfo==='#') {
						$this->meth_Locator_Replace($Txt,$LocLst[$i],$Src->RecNum,false);
					} else {
						$this->meth_Locator_Replace($Txt,$LocLst[$i],$Src->RecKey,false);
					}
				} else {
					$this->meth_Locator_Replace($Txt,$LocLst[$i],$Src->CurrRec,0);
				}
				if ($LocLst[$i]->Enlarged) {
					$PosMax = $LocLst[$i]->PosBeg;
					$LocLst[$i]->PosBeg = $LocLst[$i]->PosBeg0;
					$LocLst[$i]->PosEnd = $LocLst[$i]->PosEnd0;
					$LocLst[$i]->Enlarged = false;
				}
			}
		}

		// Unchached locators
		if ($BDef->Chk) {
			$BlockName = &$BDef->Name;
			foreach ($Src->CurrRec as $key => $val) {
				$Pos = 0;
				$Name = $BlockName.'.'.$key;
				while ($Loc = $this->meth_Locator_FindTbs($Txt,$Name,$Pos,'.')) $Pos = $this->meth_Locator_Replace($Txt,$Loc,$val,0);
			}
			$Pos = 0;
			$Name = $BlockName.'.#';
			while ($Loc = $this->meth_Locator_FindTbs($Txt,$Name,$Pos,'.')) $Pos = $this->meth_Locator_Replace($Txt,$Loc,$Src->RecNum,0);
			$Pos = 0;
			$Name = $BlockName.'.$';
			while ($Loc = $this->meth_Locator_FindTbs($Txt,$Name,$Pos,'.')) $Pos = $this->meth_Locator_Replace($Txt,$Loc,$Src->RecKey,0);
		}

	}

	// Automatic sub-blocks
	if (isset($BDef->AutoSub)) {
		for ($i=1;$i<=$BDef->AutoSub;$i++) {
			$name = $BDef->Name.'_sub'.$i;
			$query = '';
			$col = $BDef->Prm['sub'.$i];
			if ($col===true) $col = '';
			$col_opt = (substr($col,0,1)==='(') && (substr($col,-1,1)===')');
			if ($col_opt) $col = substr($col,1,strlen($col)-2);
			if ($col==='') {
				// $col_opt cannot be used here because values which are not array nore object are reformated by $Src into an array with keys 'key' and 'val'
				$data = &$Src->CurrRec;
			} elseif (is_object($Src->CurrRec)) {
				$data = &$Src->CurrRec->$col;
			} else {
				if (array_key_exists($col, $Src->CurrRec)) {
					$data = &$Src->CurrRec[$col];
				} else {
					if (!$col_opt) $this->meth_Misc_Alert('for merging the automatic sub-block ['.$name.']','key \''.$col.'\' is not found in record #'.$Src->RecNum.' of block ['.$BDef->Name.']. This key can become optional if you designate it with parenthesis in the main block, i.e.: sub'.$i.'=('.$col.')');
					unset($data); $data = array();
				}
			}
			if (is_string($data)) {
				$data = explode(',',$data);
			} elseif (is_null($data) || ($data===false)) {
				$data = array();
			}
			$this->meth_Merge_Block($Txt, $name, $data, $query, false, 0, false);
		}
	}

	return $Txt;

}

function meth_Merge_SectionSerial(&$BDef,&$SrId,&$LocR) {

	$Txt = $BDef->Src;
	$SrBDefOrdered = &$BDef->SrBDefOrdered;
	$Empty = &$LocR->SerialEmpty;

	// All Items
	$F = false;
	for ($i=$BDef->SrBDefNbr;$i>0;$i--) {
		$SrBDef = &$SrBDefOrdered[$i];
		if ($SrBDef->SrTxt===false) { // Subsection not merged with a record
			if ($Empty===false) {
				$SrBDef->SrTxt = $this->meth_Merge_SectionNormal($SrBDef,$F);
			} else {
				$SrBDef->SrTxt = $Empty;
			}
		}
		$Txt = substr_replace($Txt,$SrBDef->SrTxt,$SrBDef->SrBeg,$SrBDef->SrLen);
		$SrBDef->SrTxt = false;
	}

	$SrId = 0;
	return $Txt;

}

function meth_Merge_AutoOn(&$Txt,$Name,$TplVar,$MergeVar) {
// Merge [onload] or [onshow] fields and blocks

	$GrpDisplayed = array();
	$GrpExclusive = array();
	$P1 = false;
	$FieldBefore = false;
	$Pos = 0;

	while ($LocA=$this->meth_Locator_FindBlockNext($Txt,$Name,$Pos,'_',1,$P1,$FieldBefore)) {

		if ($LocA->BlockFound) {

			if (!isset($GrpDisplayed[$LocA->SubName])) {
				$GrpDisplayed[$LocA->SubName] = false;
				$GrpExclusive[$LocA->SubName] = ($LocA->SubName!=='');
			}
			$Displayed = &$GrpDisplayed[$LocA->SubName];
			$Exclusive = &$GrpExclusive[$LocA->SubName];

			$DelBlock = false;
			$DelField = false;
			if ($Displayed && $Exclusive) {
				$DelBlock = true;
			} else {
				if (isset($LocA->PrmLst['when'])) {
					if (isset($LocA->PrmLst['several'])) $Exclusive=false;
					$x = $LocA->PrmLst['when'];
					$this->meth_Merge_AutoVar($x,false);
					if (static::f_Misc_CheckCondition($x)) {
						$DelField = true;
						$Displayed = true;
					} else {
						$DelBlock = true;
					}
				} elseif(isset($LocA->PrmLst['default'])) {
					if ($Displayed) {
						$DelBlock = true;
					} else {
						$Displayed = true;
						$DelField = true;
					}
					$Exclusive = true; // No more block displayed for the group after
				}
			}

			// Del parts
			if ($DelField) {
				if ($LocA->PosBeg2!==false) $Txt = substr_replace($Txt,'',$LocA->PosBeg2,$LocA->PosEnd2-$LocA->PosBeg2+1);
				$Txt = substr_replace($Txt,'',$LocA->PosBeg,$LocA->PosEnd-$LocA->PosBeg+1);
				$Pos = $LocA->PosBeg;
			} else {
				$FldPos = $LocA->PosBeg;
				$FldLen = $LocA->PosEnd - $LocA->PosBeg + 1;
				if ($LocA->PosBeg2===false) {
					if (static::f_Loc_EnlargeToTag($Txt, $LocA, $LocA->PrmLst['block'],false)===false) $this->meth_Misc_Alert($LocA,'at least one tag corresponding to '.$LocA->PrmLst['block'].' is not found. Check opening tags, closing tags and embedding levels.',false,'in block\'s definition');
				} else {
					$LocA->PosEnd = $LocA->PosEnd2;
				}
				if ($DelBlock) {
					$parallel = false;
					if (isset($LocA->PrmLst['parallel'])) {
						// may return false if error
						$parallel = $this->meth_Locator_FindParallel($Txt, $LocA->PosBeg, $LocA->PosEnd, $LocA->PrmLst['parallel']);
						if ($parallel===false) {
							$Txt = substr_replace($Txt,'',$FldPos,$FldLen);
						} else {
							// delete in reverse order
							for ($r = count($parallel)-1 ; $r >= 0 ; $r--) {
								$p = $parallel[$r];
								$Txt = substr_replace($Txt,'',$p['PosBeg'],$p['PosEnd']-$p['PosBeg']+1);
							}
						}
					} else {
						$Txt = substr_replace($Txt,'',$LocA->PosBeg,$LocA->PosEnd-$LocA->PosBeg+1);
					}
				} else {
					// Merge the block as if it was a field
					$x = '';
					$this->meth_Locator_Replace($Txt,$LocA,$x,false);
				}
				$Pos = $LocA->PosBeg;
			}

		} else { // Field (has no subname at this point)

			// Check for Template Var
			if ($TplVar) {
				if (isset($LocA->PrmLst['tplvars']) || isset($LocA->PrmLst['tplfrms'])) {
					$Scan = '';
					foreach ($LocA->PrmLst as $Key => $Val) {
						if ($Scan=='v') {
							$this->TplVars[$Key] = $Val;
						} elseif ($Scan=='f') {
							self::f_Misc_FormatSave($Val,$Key);
						} elseif ($Key==='tplvars') {
							$Scan = 'v';
						} elseif ($Key==='tplfrms') {
							$Scan = 'f';
						}
					}
				}
			}

			$x = '';
			$Pos = $this->meth_Locator_Replace($Txt,$LocA,$x,false);
			$Pos = $LocA->PosBeg;

		}

	}

	if ($MergeVar) $this->meth_Merge_AutoVar($this->Source,true,$Name); // merge other fields (must have subnames)

	foreach ($this->Assigned as $n=>$a) {
		if (isset($a['auto']) && ($a['auto']===$Name)) {
			$x = array();
			$this->meth_Misc_Assign($n,$x,false);
		}
	}

}

// Prepare the strconv parameter
function meth_Conv_Prepare(&$Loc, $StrConv) {
	$x = strtolower($StrConv);
	$x = '+'.str_replace(' ','',$x).'+';
	if (strpos($x,'+esc+')!==false)  {static::f_Misc_ConvSpe($Loc); $Loc->ConvStr = false; $Loc->ConvEsc = true; }
	if (strpos($x,'+wsp+')!==false)  {static::f_Misc_ConvSpe($Loc); $Loc->ConvWS = true; }
	if (strpos($x,'+js+')!==false)   {static::f_Misc_ConvSpe($Loc); $Loc->ConvStr = false; $Loc->ConvJS = true; }
	if (strpos($x,'+url+')!==false)  {static::f_Misc_ConvSpe($Loc); $Loc->ConvStr = false; $Loc->ConvUrl = true; }
	if (strpos($x,'+utf8+')!==false)  {static::f_Misc_ConvSpe($Loc); $Loc->ConvStr = false; $Loc->ConvUtf8 = true; }
	if (strpos($x,'+no+')!==false)   $Loc->ConvStr = false;
	if (strpos($x,'+yes+')!==false)  $Loc->ConvStr = true;
	if (strpos($x,'+nobr+')!==false) {$Loc->ConvStr = true; $Loc->ConvBr = false; }
}

// Convert a string with charset or custom function
function meth_Conv_Str(&$Txt,$ConvBr=true) {
	if ($this->Charset==='') { // Html by default
		$Txt = htmlspecialchars($Txt);
		if ($ConvBr) $Txt = nl2br($Txt);
	} elseif ($this->_CharsetFct) {
		$Txt = call_user_func($this->Charset,$Txt,$ConvBr);
	} else {
		$Txt = htmlspecialchars($Txt,ENT_COMPAT,$this->Charset);
		if ($ConvBr) $Txt = nl2br($Txt);
	}
}

// Standard alert message provided by TinyButStrong, return False is the message is cancelled.
function meth_Misc_Alert($Src,$Msg,$NoErrMsg=false,$SrcType=false) {
	$this->ErrCount++;
	if ($this->NoErr || (PHP_SAPI==='cli') ) {
		$t = array('','','','','');
	} else {
		$t = array('<br /><b>','</b>','<em>','</em>','<br />');
		$Msg = htmlentities($Msg);
	}
	if (!is_string($Src)) {
		if ($SrcType===false) $SrcType='in field';
		if (isset($Src->PrmLst['tbstype'])) {
			$Msg = 'Column \''.$Src->SubName.'\' is expected but missing in the current record.';
			$Src = 'Parameter \''.$Src->PrmLst['tbstype'].'='.$Src->SubName.'\'';
			$NoErrMsg = false;
		} else {
			$Src = $SrcType.' '.$this->_ChrOpen.$Src->FullName.'...'.$this->_ChrClose;
		}
	}
	$x = $t[0].'TinyButStrong Error'.$t[1].' '.$Src.': '.$Msg;
	if ($NoErrMsg) $x = $x.' '.$t[2].'This message can be cancelled using parameter \'noerr\'.'.$t[3];
	$x = $x.$t[4]."\n";
	if ($this->NoErr) {
		$this->ErrMsg .= $x;
	} else {
		if (PHP_SAPI!=='cli') {
			$x = str_replace($this->_ChrOpen,$this->_ChrProtect,$x);
		}
		echo $x;
	}
	return false;
}

function meth_Misc_Assign($Name,&$ArgLst,$CallingMeth) {
// $ArgLst must be by reference in order to have its inner items by reference too.

	if (!isset($this->Assigned[$Name])) {
		if ($CallingMeth===false) return true;
		return $this->meth_Misc_Alert('with '.$CallingMeth.'() method','key \''.$Name.'\' is not defined in property Assigned.');
	}

	$a = &$this->Assigned[$Name];
	$meth = (isset($a['type'])) ? $a['type'] : 'MergeBlock';
	if (($CallingMeth!==false) && (strcasecmp($CallingMeth,$meth)!=0)) return $this->meth_Misc_Alert('with '.$CallingMeth.'() method','the assigned key \''.$Name.'\' cannot be used with method '.$CallingMeth.' because it is defined to run with '.$meth.'.');

	$n = count($a);
	for ($i=0;$i<$n;$i++) {
		if (isset($a[$i])) $ArgLst[$i] = &$a[$i];
	}

	if ($CallingMeth===false) {
		if (in_array(strtolower($meth),array('mergeblock','mergefield'))) {
			call_user_func_array(array(&$this,$meth), $ArgLst);
		} else {
			return $this->meth_Misc_Alert('The assigned field \''.$Name.'\'. cannot be merged because its type \''.$a[0].'\' is not supported.');
		}
	}
	if (!isset($a['merged'])) $a['merged'] = 0;
	$a['merged']++;
	return true;
}

function meth_Misc_IsMainTpl() {
	return ($this->_Mode==0);
}

function meth_Misc_ChangeMode($Init,&$Loc,&$CurrVal) {
	if ($Init) {
		// Save contents configuration
		$Loc->SaveSrc = &$this->Source;
		$Loc->SaveMode = $this->_Mode;
		$Loc->SaveVarRef = &$this->VarRef;
		unset($this->Source); $this->Source = '';
		$this->_Mode++; // Mode>0 means subtemplate mode
		if ($this->OldSubTpl) {
			ob_start(); // Start buffuring output
			$Loc->SaveRender = $this->Render;
		}
		$this->Render = TBS_OUTPUT;
	} else {
		// Restore contents configuration
		if ($this->OldSubTpl) {
			$CurrVal = ob_get_contents();
			ob_end_clean();
			$this->Render = $Loc->SaveRender;
		} else {
			$CurrVal = $this->Source;
		}
		$this->Source = &$Loc->SaveSrc;
		$this->_Mode = $Loc->SaveMode;
		$this->VarRef = &$Loc->SaveVarRef;
	}
}

function meth_Misc_UserFctCheck(&$FctInfo,$FctCat,&$FctObj,&$ErrMsg,$FctCheck=false) {

	$FctId = $FctCat.':'.$FctInfo;
	if (isset($this->_UserFctLst[$FctId])) {
		$FctInfo = $this->_UserFctLst[$FctId];
		return true;
	}

	// Check and put in cache
	$FctStr = $FctInfo;
	$IsData = ($FctCat!=='f');
	$Save = true;
	if ($FctStr[0]==='~') {
		$ObjRef = &$this->ObjectRef;
		$Lst = explode('.',substr($FctStr,1));
		$iMax = count($Lst) - 1;
		$Suff = 'tbsdb';
		$iMax0 = $iMax;
		if ($IsData) {
			$Suff = $Lst[$iMax];
			$iMax--;
		}
		// Reading sub items
		for ($i=0;$i<=$iMax;$i++) {
			$x = &$Lst[$i];
			if (is_object($ObjRef)) {
				$ArgLst = static::f_Misc_CheckArgLst($x);
				if (method_exists($ObjRef,$x)) {
					if ($i<$iMax) {
						$f = array(&$ObjRef,$x); unset($ObjRef);
						$ObjRef = call_user_func_array($f,$ArgLst);
					}
				} elseif ($i===$iMax0) {
					$ErrMsg = 'Expression \''.$FctStr.'\' is invalid because \''.$x.'\' is not a method in the class \''.get_class($ObjRef).'\'.';
					return false;
				} elseif (isset($ObjRef->$x)) {
					$ObjRef = &$ObjRef->$x;
				} else {
					$ErrMsg = 'Expression \''.$FctStr.'\' is invalid because sub-item \''.$x.'\' is neither a method nor a property in the class \''.get_class($ObjRef).'\'.';
					return false;
				}
			} elseif (($i<$iMax0) && is_array($ObjRef)) {
				if (isset($ObjRef[$x])) {
					$ObjRef = &$ObjRef[$x];
				} else {
					$ErrMsg = 'Expression \''.$FctStr.'\' is invalid because sub-item \''.$x.'\' is not a existing key in the array.';
					return false;
				}
			} else {
				$ErrMsg = 'Expression \''.$FctStr.'\' is invalid because '.(($i===0)?'property ObjectRef':'sub-item \''.$x.'\'').' is not an object'.(($i<$iMax)?' or an array.':'.');
				return false;
			}
		}
		// Referencing last item
		if ($IsData) {
			$FctInfo = array('open'=>'','fetch'=>'','close'=>'');
			foreach ($FctInfo as $act=>$x) {
				$FctName = $Suff.'_'.$act;
				if (method_exists($ObjRef,$FctName)) {
					$FctInfo[$act] = array(&$ObjRef,$FctName);
				} else {
					$ErrMsg = 'Expression \''.$FctStr.'\' is invalid because method '.$FctName.' is not found.';
					return false;
				}
			}
			$FctInfo['type'] = 4;
			if (isset($this->RecheckObj) && $this->RecheckObj) $Save = false;
		} else {
			$FctInfo = array(&$ObjRef,$x);
		}
	} elseif ($IsData) {

		$IsObj = ($FctCat==='o');

		if ($IsObj && method_exists($FctObj,'tbsdb_open') && (!method_exists($FctObj,'+'))) { // '+' avoid a bug in PHP 5

			if (!method_exists($FctObj,'tbsdb_fetch')) {
				$ErrMsg = 'the expected method \'tbsdb_fetch\' is not found for the class '.$Cls.'.';
				return false;
			}
			if (!method_exists($FctObj,'tbsdb_close')) {
				$ErrMsg = 'the expected method \'tbsdb_close\' is not found for the class '.$Cls.'.';
				return false;
			}
			$FctInfo = array('type'=>5);

		}	else {

			if ($FctCat==='r') { // Resource
				$x = strtolower($FctStr);
				$x = str_replace('-','_',$x);
				$Key = '';
				$i = 0;
				$iMax = strlen($x);
				while ($i<$iMax) {
					if (($x[$i]==='_') || (($x[$i]>='a') && ($x[$i]<='z')) || (($x[$i]>='0') && ($x[$i]<='9'))) {
						$Key .= $x[$i];
						$i++;
					} else {
						$i = $iMax;
					}
				}
			} else {
				$Key = $FctStr;
			}

			$FctInfo = array('open'=>'','fetch'=>'','close'=>'');
			foreach ($FctInfo as $act=>$x) {
				$FctName = 'tbsdb_'.$Key.'_'.$act;
				if (function_exists($FctName)) {
					$FctInfo[$act] = $FctName;
				} else {
					$err = true;
					if ($act==='open') { // Try simplified key
						$p = strpos($Key,'_');
						if ($p!==false) {
							$Key2 = substr($Key,0,$p);
							$FctName2  = 'tbsdb_'.$Key2.'_'.$act;
							if (function_exists($FctName2)) {
								$err = false;
								$Key = $Key2;
								$FctInfo[$act] = $FctName2;
							}
						}
					}
					if ($err) {
						$ErrMsg = 'Data source Id \''.$FctStr.'\' is unsupported because function \''.$FctName.'\' is not found.';
						return false;
					}
				}
			}

			$FctInfo['type'] = 3;

		}

	} else {
		if ( $FctCheck && ($this->FctPrefix!=='') && (strncmp($this->FctPrefix,$FctStr,strlen($this->FctPrefix))!==0) ) {
			$ErrMsg = 'user function \''.$FctStr.'\' does not match the allowed prefix.'; return false;
		} else if (!function_exists($FctStr)) {
			$x = explode('.',$FctStr);
			if (count($x)==2) {
				if (class_exists($x[0])) {
					$FctInfo = $x;
				} else {
					$ErrMsg = 'user function \''.$FctStr.'\' is not correct because \''.$x[0].'\' is not a class name.'; return false;
				}
			} else {
				$ErrMsg = 'user function \''.$FctStr.'\' is not found.'; return false;
			}
		}
	}

	if ($Save) $this->_UserFctLst[$FctId] = $FctInfo;
	return true;

}

function meth_Misc_RunSubscript(&$CurrVal,$CurrPrm) {
// Run a subscript without any local variable damage
	return @include($this->_Subscript);
}

function meth_Misc_Charset($Charset) {
	if ($Charset==='+') return;
	$this->_CharsetFct = false;
	if (is_string($Charset)) {
		if (($Charset!=='') && ($Charset[0]==='=')) {
			$ErrMsg = false;
			$Charset = substr($Charset,1);
			if ($this->meth_Misc_UserFctCheck($Charset,'f',$ErrMsg,$ErrMsg,false)) {
				$this->_CharsetFct = true;
			} else {
				$this->meth_Misc_Alert('with charset option',$ErrMsg);
				$Charset = '';
			}
		}
	} elseif (is_array($Charset)) {
		$this->_CharsetFct = true;
	} elseif ($Charset===false) {
		$this->Protect = false;
	} else {
		$this->meth_Misc_Alert('with charset option','the option value is not a string nor an array.');
		$Charset = '';
	}
	$this->Charset = $Charset;
}

function meth_PlugIn_RunAll(&$FctBank,&$ArgLst) {
	$OkAll = true;
	foreach ($FctBank as $FctInfo) {
		$Ok = call_user_func_array($FctInfo,$ArgLst);
		if (!is_null($Ok)) $OkAll = ($OkAll && $Ok);
	}
	return $OkAll;
}

function meth_PlugIn_Install($PlugInId,$ArgLst,$Auto) {

	$ErrMsg = 'with plug-in \''.$PlugInId.'\'';

	if (class_exists($PlugInId)) {
		// Create an instance
		$IsObj = true;
		$PiRef = new $PlugInId;
		$PiRef->TBS = &$this;
		if (!method_exists($PiRef,'OnInstall')) return $this->meth_Misc_Alert($ErrMsg,'OnInstall() method is not found.');
		$FctRef = array(&$PiRef,'OnInstall');
	} else {
		$FctRef = 'tbspi_'.$PlugInId.'_OnInstall';
		if(function_exists($FctRef)) {
			$IsObj = false;
			$PiRef = true;
		} else {
			return $this->meth_Misc_Alert($ErrMsg,'no class named \''.$PlugInId.'\' is found, and no function named \''.$FctRef.'\' is found.');
		}
	}

	$this->_PlugIns[$PlugInId] = &$PiRef;

	$EventLst = call_user_func_array($FctRef,$ArgLst);
	if (is_string($EventLst)) $EventLst = explode(',',$EventLst);
	if (!is_array($EventLst)) return $this->meth_Misc_Alert($ErrMsg,'OnInstall() method does not return an array.');

	// Add activated methods
	foreach ($EventLst as $Event) {
		$Event = trim($Event);
		if (!$this->meth_PlugIn_SetEvent($PlugInId, $Event)) return false;
	}

	return true;

}

function meth_PlugIn_SetEvent($PlugInId, $Event, $NewRef='') {
// Enable or disable a plug-in event. It can be called by a plug-in, even during the OnInstall event. $NewRef can be used to change the method associated to the event.

	// Check the event's name
	if (strpos(',OnCommand,BeforeLoadTemplate,AfterLoadTemplate,BeforeShow,AfterShow,OnData,OnFormat,OnOperation,BeforeMergeBlock,OnMergeSection,OnMergeGroup,AfterMergeBlock,OnSpecialVar,OnMergeField,OnCacheField,', ','.$Event.',')===false) return $this->meth_Misc_Alert('with plug-in \''.$PlugInId.'\'','The plug-in event named \''.$Event.'\' is not supported by TinyButStrong (case-sensitive). This event may come from the OnInstall() method.');

	$PropName = '_pi'.$Event;

	if ($NewRef===false) {
		// Disable the event
		if (!isset($this->$PropName)) return false;
		$PropRef = &$this->$PropName;
		unset($PropRef[$PlugInId]);
		return true;
	}
	
	// Prepare the reference to be called
	$PiRef = &$this->_PlugIns[$PlugInId];
	if (is_object($PiRef)) {
		if ($NewRef==='') $NewRef = $Event;
		if (!method_exists($PiRef, $NewRef)) return $this->meth_Misc_Alert('with plug-in \''.$PlugInId.'\'','The plug-in event named \''.$Event.'\' is declared but its corresponding method \''.$NewRef.'\' is found.');
		$FctRef = array(&$PiRef, $NewRef);
	} else {
		$FctRef = ($NewRef==='') ? 'tbspi_'.$PlugInId.'_'.$Event : $NewRef;
		if (!function_exists($FctRef)) return $this->meth_Misc_Alert('with plug-in \''.$PlugInId.'\'','The expected function \''.$FctRef.'\' is not found.');
	}

	// Save information into the corresponding property
	if (!isset($this->$PropName)) $this->$PropName = array();
	$PropRef = &$this->$PropName;
	$PropRef[$PlugInId] = $FctRef;

	// Flags saying if a plugin is installed
	switch ($Event) {
	case 'OnCommand': break;
	case 'OnSpecialVar': break;
	case 'OnOperation': break;
	case 'OnFormat': $this->_piOnFrm_Ok = true; break;
	default: $this->_PlugIns_Ok = true; break;
	}
		
	return true;

}

static function meth_Misc_ToStr($Value) {
	if (is_string($Value)) {
		return $Value;
	} elseif(is_object($Value)) {
		if (method_exists($Value,'__toString')) {
			return $Value->__toString();
		} elseif (is_a($Value, 'DateTime')) {
			return $Value->format('c');
		}
	}
	return @(string)$Value; // (string) is faster than strval() and settype()
}

function meth_Misc_Format(&$Value,&$PrmLst) {
// This function return the formated representation of a Date/Time or numeric variable using a 'VB like' format syntax instead of the PHP syntax.

	$FrmStr = $PrmLst['frm'];
	$CheckNumeric = true;
	if (is_string($Value)) $Value = trim($Value);

	if ($FrmStr==='') return '';
	$Frm = self::f_Misc_FormatSave($FrmStr);

	// Manage Multi format strings
	if ($Frm['type']=='multi') {

		// Select the format
		if (is_numeric($Value)) {
			if (is_string($Value)) $Value = 0.0 + $Value;
			if ($Value>0) {
				$FrmStr = &$Frm[0];
			} elseif ($Value<0) {
				$FrmStr = &$Frm[1];
				if ($Frm['abs']) $Value = abs($Value);
			} else { // zero
				$FrmStr = &$Frm[2];
				$Minus = '';
			}
			$CheckNumeric = false;
		} else {
			$Value = static::meth_Misc_ToStr($Value);
			if ($Value==='') {
				return $Frm[3]; // Null value
			} else {
				$t = strtotime($Value); // We look if it's a date
				if (($t===-1) || ($t===false)) { // Date not recognized
					return $Frm[1];
				} elseif ($t===943916400) { // Date to zero
					return $Frm[2];
				} else { // It's a date
					$Value = $t;
					$FrmStr = &$Frm[0];
				}
			}
		}

		// Retrieve the correct simple format
		if ($FrmStr==='') return '';
		$Frm = self::f_Misc_FormatSave($FrmStr);

	}

	switch ($Frm['type']) {
	case 'num' :
		// NUMERIC
		if ($CheckNumeric) {
			if (is_numeric($Value)) {
				if (is_string($Value)) $Value = 0.0 + $Value;
			} else {
				return static::meth_Misc_ToStr($Value);
			}
		}
		if ($Frm['PerCent']) $Value = $Value * 100;
		$Value = number_format($Value,$Frm['DecNbr'],$Frm['DecSep'],$Frm['ThsSep']);
		if ($Frm['Pad']!==false) $Value = str_pad($Value, $Frm['Pad'], '0', STR_PAD_LEFT);
		if ($Frm['ThsRpl']!==false) $Value = str_replace($Frm['ThsSep'], $Frm['ThsRpl'], $Value);
		$Value = substr_replace($Frm['Str'],$Value,$Frm['Pos'],$Frm['Len']);
		return $Value;
		break;
	case 'date' :
		// DATE
		if (is_object($Value)) {
			$Value = static::meth_Misc_ToStr($Value);
		}
		if (is_string($Value)) {
			if ($Value==='') return '';
			$x = strtotime($Value);
			if (($x===-1) || ($x===false)) {
				if (!is_numeric($Value)) $Value = 0;
			} else {
				$Value = &$x;
			}
		} else {
			if (!is_numeric($Value)) return static::meth_Misc_ToStr($Value);
		}
		if ($Frm['loc'] || isset($PrmLst['locale'])) {
			$x = strftime($Frm['str_loc'],$Value);
			$this->meth_Conv_Str($x,false); // may have accent
			return $x;
		} else {
			return date($Frm['str_us'],$Value);
		}
		break;
	default:
		return $Frm['string'];
		break;
	}

}

// Simply update an array
static function f_Misc_UpdateArray(&$array, $numerical, $v, $d) {
	if (!is_array($v)) {
		if (is_null($v)) {
			$array = array();
			return;
		} else {
			$v = array($v=>$d);
		}
	}
	foreach ($v as $p=>$a) {
		if ($numerical===true) { // numerical keys
			if (is_string($p)) {
				// syntax: item => true/false
				$i = array_search($p, $array, true);
				if ($i===false) {
					if (!is_null($a)) $array[] = $p;
				} else {
					if (is_null($a)) array_splice($array, $i, 1);
				}
			} else {
				// syntax: i => item
				$i = array_search($a, $array, true);
				if ($i==false) $array[] = $a;
			}
		} else { // string keys
			if (is_null($a)) {
				unset($array[$p]);
			} elseif ($numerical==='frm') {
				self::f_Misc_FormatSave($a, $p);
			} else {
				$array[$p] = $a;
			}
		}
	}
}

static function f_Misc_FormatSave(&$FrmStr,$Alias='') {

	$FormatLst = &$GLOBALS['_TBS_FormatLst'];

	if (isset($FormatLst[$FrmStr])) {
		if ($Alias!='') $FormatLst[$Alias] = &$FormatLst[$FrmStr];
		return $FormatLst[$FrmStr];
	}

	if (strpos($FrmStr,'|')!==false) {

		// Multi format
		$Frm = explode('|',$FrmStr); // syntax: Postive|Negative|Zero|Null
		$FrmNbr = count($Frm);
		$Frm['abs'] = ($FrmNbr>1);
		if ($FrmNbr<3) $Frm[2] = &$Frm[0]; // zero
		if ($FrmNbr<4) $Frm[3] = ''; // null
		$Frm['type'] = 'multi';
		$FormatLst[$FrmStr] = $Frm;

	} elseif (($nPosEnd = strrpos($FrmStr,'0'))!==false) {

		// Numeric format
		$nDecSep = '.';
		$nDecNbr = 0;
		$nDecOk = true;
		$nPad = false;
		$nPadZ = 0;

		if (substr($FrmStr,$nPosEnd+1,1)==='.') {
			$nPosEnd++;
			$nPos = $nPosEnd;
			$nPadZ = 1;
		} else {
			$nPos = $nPosEnd - 1;
			while (($nPos>=0) && ($FrmStr[$nPos]==='0')) {
				$nPos--;
			}
			if (($nPos>=1) && ($FrmStr[$nPos-1]==='0')) {
				$nDecSep = $FrmStr[$nPos];
				$nDecNbr = $nPosEnd - $nPos;
			} else {
				$nDecOk = false;
			}
		}

		// Thousand separator
		$nThsSep = '';
		$nThsRpl = false;
		if (($nDecOk) && ($nPos>=5)) {
			if ((substr($FrmStr,$nPos-3,3)==='000') && ($FrmStr[$nPos-4]!=='0')) {
				$p = strrpos(substr($FrmStr,0,$nPos-4), '0');
				if ($p!==false) {
					$len = $nPos-4-$p;
					$x = substr($FrmStr, $p+1, $len);
					if ($len>1) {
						// for compatibility for number_format() with PHP < 5.4.0
						$nThsSep = ($nDecSep=='*') ? '.' : '*';
						$nThsRpl = $x;
					} else {
						$nThsSep = $x;
					}
					$nPos = $p+1;
				}
			}
		}

		// Pass next zero
		if ($nDecOk) $nPos--;
		while (($nPos>=0) && ($FrmStr[$nPos]==='0')) {
			$nPos--;
		}

		$nLen = $nPosEnd-$nPos;
		if ( ($nThsSep==='') && ($nLen>($nDecNbr+$nPadZ+1)) )	$nPad = $nLen - $nPadZ;

		// Percent
		$nPerCent = (strpos($FrmStr,'%')===false) ? false : true;

		$FormatLst[$FrmStr] = array('type'=>'num','Str'=>$FrmStr,'Pos'=>($nPos+1),'Len'=>$nLen,'ThsSep'=>$nThsSep,'ThsRpl'=>$nThsRpl,'DecSep'=>$nDecSep,'DecNbr'=>$nDecNbr,'PerCent'=>$nPerCent,'Pad'=>$nPad);

	} else {

		// Date format
		$x = $FrmStr;
		$FrmPHP = '';
		$FrmLOC = '';
		$StrIn = false;
		$Cnt = 0;
		$i = strpos($FrmStr,'(locale)');
		$Locale = ($i!==false);
		if ($Locale) $x = substr_replace($x,'',$i,8);

		$iEnd = strlen($x);
		for ($i=0;$i<$iEnd;$i++) {

			if ($StrIn) {
				// We are in a string part
				if ($x[$i]==='"') {
					if (substr($x,$i+1,1)==='"') {
						$FrmPHP .= '\\"'; // protected char
						$FrmLOC .= $x[$i];
						$i++;
					} else {
						$StrIn = false;
					}
				} else {
					$FrmPHP .= '\\'.$x[$i]; // protected char
					$FrmLOC .= $x[$i];
				}
			} else {
				if ($x[$i]==='"') {
					$StrIn = true;
				} else {
					$Cnt++;
					if     (strcasecmp(substr($x,$i,2),'hh'  )===0) { $FrmPHP .= 'H'; $FrmLOC .= '%H'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,2),'hm'  )===0) { $FrmPHP .= 'h'; $FrmLOC .= '%I'; $i += 1;} // for compatibility
					elseif (strcasecmp(substr($x,$i,1),'h'   )===0) { $FrmPHP .= 'G'; $FrmLOC .= '%H';}
					elseif (strcasecmp(substr($x,$i,2),'rr'  )===0) { $FrmPHP .= 'h'; $FrmLOC .= '%I'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,1),'r'   )===0) { $FrmPHP .= 'g'; $FrmLOC .= '%I';}
					elseif (strcasecmp(substr($x,$i,4),'ampm')===0) { $FrmPHP .= substr($x,$i,1); $FrmLOC .= '%p'; $i += 3;} // $Fmp = 'A' or 'a'
					elseif (strcasecmp(substr($x,$i,2),'nn'  )===0) { $FrmPHP .= 'i'; $FrmLOC .= '%M'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,2),'ss'  )===0) { $FrmPHP .= 's'; $FrmLOC .= '%S'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,2),'xx'  )===0) { $FrmPHP .= 'S'; $FrmLOC .= ''  ; $i += 1;}
					elseif (strcasecmp(substr($x,$i,4),'yyyy')===0) { $FrmPHP .= 'Y'; $FrmLOC .= '%Y'; $i += 3;}
					elseif (strcasecmp(substr($x,$i,2),'yy'  )===0) { $FrmPHP .= 'y'; $FrmLOC .= '%y'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,4),'mmmm')===0) { $FrmPHP .= 'F'; $FrmLOC .= '%B'; $i += 3;}
					elseif (strcasecmp(substr($x,$i,3),'mmm' )===0) { $FrmPHP .= 'M'; $FrmLOC .= '%b'; $i += 2;}
					elseif (strcasecmp(substr($x,$i,2),'mm'  )===0) { $FrmPHP .= 'm'; $FrmLOC .= '%m'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,1),'m'   )===0) { $FrmPHP .= 'n'; $FrmLOC .= '%m';}
					elseif (strcasecmp(substr($x,$i,4),'wwww')===0) { $FrmPHP .= 'l'; $FrmLOC .= '%A'; $i += 3;}
					elseif (strcasecmp(substr($x,$i,3),'www' )===0) { $FrmPHP .= 'D'; $FrmLOC .= '%a'; $i += 2;}
					elseif (strcasecmp(substr($x,$i,1),'w'   )===0) { $FrmPHP .= 'w'; $FrmLOC .= '%u';}
					elseif (strcasecmp(substr($x,$i,4),'dddd')===0) { $FrmPHP .= 'l'; $FrmLOC .= '%A'; $i += 3;}
					elseif (strcasecmp(substr($x,$i,3),'ddd' )===0) { $FrmPHP .= 'D'; $FrmLOC .= '%a'; $i += 2;}
					elseif (strcasecmp(substr($x,$i,2),'dd'  )===0) { $FrmPHP .= 'd'; $FrmLOC .= '%d'; $i += 1;}
					elseif (strcasecmp(substr($x,$i,1),'d'   )===0) { $FrmPHP .= 'j'; $FrmLOC .= '%d';}
					else {
						$FrmPHP .= '\\'.$x[$i]; // protected char
						$FrmLOC .= $x[$i]; // protected char
						$Cnt--;
					}
				}
			}

		}

		if ($Cnt>0) {
			$FormatLst[$FrmStr] = array('type'=>'date','str_us'=>$FrmPHP,'str_loc'=>$FrmLOC,'loc'=>$Locale);
		} else {
			$FormatLst[$FrmStr] = array('type'=>'else','string'=>$FrmStr);
		}

	}

	if ($Alias!='') $FormatLst[$Alias] = &$FormatLst[$FrmStr];

	return $FormatLst[$FrmStr];

}

static function f_Misc_ConvSpe(&$Loc) {
	if ($Loc->ConvMode!==2) {
		$Loc->ConvMode = 2;
		$Loc->ConvEsc = false;
		$Loc->ConvWS = false;
		$Loc->ConvJS = false;
		$Loc->ConvUrl = false;
		$Loc->ConvUtf8 = false;
	}
}

static function f_Misc_CheckArgLst(&$Str) {
	$ArgLst = array();
	if (substr($Str,-1,1)===')') {
		$pos = strpos($Str,'(');
		if ($pos!==false) {
			$ArgLst = explode(',',substr($Str,$pos+1,strlen($Str)-$pos-2));
			$Str = substr($Str,0,$pos);
		}
	}
	return $ArgLst;
}

static function f_Misc_CheckCondition($Str) {
// Check if an expression like "exrp1=expr2" is true or false.

	$StrZ = $Str; // same string but without protected data
	$Max = strlen($Str)-1;
	$p = strpos($Str,'\'');
	if ($Esc=($p!==false)) {
		$In = true;
		for ($p=$p+1;$p<=$Max;$p++) {
			if ($StrZ[$p]==='\'') {
				$In = !$In;
			} elseif ($In) {
				$StrZ[$p] = 'z';
			}
		}
	}

	// Find operator and position
	$Ope = '=';
	$Len = 1;
	$p = strpos($StrZ,$Ope);
	if ($p===false) {
		$Ope = '+';
		$p = strpos($StrZ,$Ope);
		if ($p===false) return false;
		if (($p>0) && ($StrZ[$p-1]==='-')) {
			$Ope = '-+'; $p--; $Len=2;
		} elseif (($p<$Max) && ($StrZ[$p+1]==='-')) {
			$Ope = '+-'; $Len=2;
		} else {
			return false;
		}
	} else {
		if ($p>0) {
			$x = $StrZ[$p-1];
			if ($x==='!') {
				$Ope = '!='; $p--; $Len=2;
			} elseif ($x==='~') {
				$Ope = '~='; $p--; $Len=2;
			} elseif ($p<$Max) {
				$y = $StrZ[$p+1];
				if ($y==='=') {
					$Len=2;
				} elseif (($x==='+') && ($y==='-')) {
					$Ope = '+=-'; $p--; $Len=3;
				} elseif (($x==='-') && ($y==='+')) {
					$Ope = '-=+'; $p--; $Len=3;
				}
			} else {
			}
		}
	}

	// Read values
	$Val1  = trim(substr($Str,0,$p));
	$Val2  = trim(substr($Str,$p+$Len));
	if ($Esc) {
		$Nude1 = self::f_Misc_DelDelimiter($Val1,'\'');
		$Nude2 = self::f_Misc_DelDelimiter($Val2,'\'');
	} else {
		$Nude1 = $Nude2 = false;
	}

	// Compare values
	if ($Ope==='=') {
		return (strcasecmp($Val1,$Val2)==0);
	} elseif ($Ope==='!=') {
		return (strcasecmp($Val1,$Val2)!=0);
	} elseif ($Ope==='~=') {
		return (preg_match($Val2,$Val1)>0);
	} else {
		if ($Nude1) $Val1='0'+$Val1;
		if ($Nude2) $Val2='0'+$Val2;
		if ($Ope==='+-') {
			return ($Val1>$Val2);
		} elseif ($Ope==='-+') {
			return ($Val1 < $Val2);
		} elseif ($Ope==='+=-') {
			return ($Val1 >= $Val2);
		} elseif ($Ope==='-=+') {
			return ($Val1<=$Val2);
		} else {
			return false;
		}
	}

}

static function f_Misc_DelDelimiter(&$Txt,$Delim) {
// Delete the string delimiters
	$len = strlen($Txt);
	if (($len>1) && ($Txt[0]===$Delim)) {
		if ($Txt[$len-1]===$Delim) $Txt = substr($Txt,1,$len-2);
		return false;
	} else {
		return true;
	}
}

static function f_Misc_GetFile(&$Res, &$File, $LastFile='', $IncludePath=false, $Contents=true) {
// Load the content of a file into the text variable.

	$Res = '';
	$fd = self::f_Misc_TryFile($File, false); 
	if ($fd===false) {
		if (is_array($IncludePath)) {
			foreach ($IncludePath as $d) {
				$fd = self::f_Misc_TryFile($File, $d);
				if ($fd!==false) break;
			}
		}
		if (($fd===false) && ($LastFile!='')) $fd = self::f_Misc_TryFile($File, dirname($LastFile));
		if ($fd===false) return false;
	}

	$fs = fstat($fd);
	if ($Contents) {
		// Return contents
		if (isset($fs['size'])) {
			if ($fs['size']>0) $Res = fread($fd,$fs['size']);
		} else {
			while (!feof($fd)) $Res .= fread($fd,4096);
		}
	} else {
		// Return stats
		$Res = $fs;
	}

	fclose($fd);
	return true;

}

static function f_Misc_TryFile(&$File, $Dir) {
	if ($Dir==='') return false;
	$FileSearch = ($Dir===false) ? $File : $Dir.'/'.$File;
	// 'rb' if binary for some OS. fopen() uses include_path and search on the __FILE__ directory while file_exists() doesn't.
	$f = @fopen($FileSearch, 'r', true);
	if ($f!==false) $File = $FileSearch;
	return $f;
}

static function f_Loc_PrmRead(&$Txt,$Pos,$XmlTag,$DelimChrs,$BegStr,$EndStr,&$Loc,&$PosEnd,$WithPos=false) {

	$BegLen = strlen($BegStr);
	$BegChr = $BegStr[0];
	$BegIs1 = ($BegLen===1);

	$DelimIdx = false;
	$DelimCnt = 0;
	$DelimChr = '';
	$BegCnt = 0;
	$SubName = $Loc->SubOk;

	$Status = 0; // 0: name not started, 1: name started, 2: name ended, 3: equal found, 4: value started
	$PosName = 0;
	$PosNend = 0;
	$PosVal = 0;

	// Variables for checking the loop
	$PosEnd = strpos($Txt,(string) $EndStr,$Pos);
	if ($PosEnd===false) return;
	$Continue = ($Pos<$PosEnd);

	while ($Continue) {

		$Chr = $Txt[$Pos];

		if ($DelimIdx) { // Reading in the string

			if ($Chr===$DelimChr) { // Quote found
				if ($Chr===$Txt[$Pos+1]) { // Double Quote => the string continue with un-double the quote
					$Pos++;
				} else { // Simple Quote => end of string
					$DelimIdx = false;
				}
			}

		} else { // Reading outside the string

			if ($BegCnt===0) {

				// Analyzing parameters
				$CheckChr = false;
				if (($Chr===' ') || ($Chr==="\r") || ($Chr==="\n")) {
					if ($Status===1) {
						if ($SubName && ($XmlTag===false)) {
							// Accept spaces in TBS subname.
						} else {
							$Status = 2;
							$PosNend = $Pos;
						}
					} elseif ($XmlTag && ($Status===4)) {
						self::f_Loc_PrmCompute($Txt,$Loc,$SubName,$Status,$XmlTag,$DelimChr,$DelimCnt,$PosName,$PosNend,$PosVal,$Pos,$WithPos);
						$Status = 0;
					}
				} elseif (($XmlTag===false) && ($Chr===';')) {
					self::f_Loc_PrmCompute($Txt,$Loc,$SubName,$Status,$XmlTag,$DelimChr,$DelimCnt,$PosName,$PosNend,$PosVal,$Pos,$WithPos);
					$Status = 0;
				} elseif ($Status===4) {
					$CheckChr = true;
				} elseif ($Status===3) {
					$Status = 4;
					$DelimCnt = 0;
					$PosVal = $Pos;
					$CheckChr = true;
				} elseif ($Status===2) {
					if ($Chr==='=') {
						$Status = 3;
					} elseif ($XmlTag) {
						self::f_Loc_PrmCompute($Txt,$Loc,$SubName,$Status,$XmlTag,$DelimChr,$DelimCnt,$PosName,$PosNend,$PosVal,$Pos,$WithPos);
						$Status = 1;
						$PosName = $Pos;
						$CheckChr = true;
					} else {
						$Status = 4;
						$DelimCnt = 0;
						$PosVal = $Pos;
						$CheckChr = true;
					}
				} elseif ($Status===1) {
					if ($Chr==='=') {
						$Status = 3;
						$PosNend = $Pos;
					} else {
						$CheckChr = true;
					}
				} else {
					$Status = 1;
					$PosName = $Pos;
					$CheckChr = true;
				}

				if ($CheckChr) {
					$DelimIdx = strpos($DelimChrs,(string) $Chr);
					if ($DelimIdx===false) {
						if ($Chr===$BegChr) {
							if ($BegIs1) {
								$BegCnt++;
							} elseif(substr($Txt,$Pos,$BegLen)===$BegStr) {
								$BegCnt++;
							}
						}
					} else {
						$DelimChr = $DelimChrs[$DelimIdx];
						$DelimCnt++;
						$DelimIdx = true;
					}
				}

			} else {
				if ($Chr===$BegChr) {
					if ($BegIs1) {
						$BegCnt++;
					} elseif(substr($Txt,$Pos,$BegLen)===$BegStr) {
						$BegCnt++;
					}
				}
			}

		}

		// Next char
		$Pos++;

		// We check if it's the end
		if ($Pos===$PosEnd) {
			if ($XmlTag) {
				$Continue = false;
			} elseif ($DelimIdx===false) {
				if ($BegCnt>0) {
					$BegCnt--;
				} else {
					$Continue = false;
				}
			}
			if ($Continue) {
				$PosEnd = strpos($Txt,(string) $EndStr,$PosEnd+1);
				if ($PosEnd===false) return;
			} else {
				if ($XmlTag && ($Txt[$Pos-1]==='/')) $Pos--; // In case last attribute is stuck to "/>"
				self::f_Loc_PrmCompute($Txt,$Loc,$SubName,$Status,$XmlTag,$DelimChr,$DelimCnt,$PosName,$PosNend,$PosVal,$Pos,$WithPos);
			}
		}

	}

	$PosEnd = $PosEnd + (strlen($EndStr)-1);

}

static function f_Loc_PrmCompute(&$Txt,&$Loc,&$SubName,$Status,$XmlTag,$DelimChr,$DelimCnt,$PosName,$PosNend,$PosVal,$Pos,$WithPos) {

	if ($Status===0) {
		$SubName = false;
	} else {
		if ($Status===1) {
			$x = substr($Txt,$PosName,$Pos-$PosName);
		} else {
			$x = substr($Txt,$PosName,$PosNend-$PosName);
		}
		if ($XmlTag) $x = strtolower($x);
		if ($SubName) {
			$Loc->SubName = trim($x);
			$SubName = false;
		} else {
			if ($Status===4) {
				$v = trim(substr($Txt,$PosVal,$Pos-$PosVal));
				if ($DelimCnt===1) { // Delete quotes inside the value
					if ($v[0]===$DelimChr) {
						$len = strlen($v);
						if ($v[$len-1]===$DelimChr) {
							$v = substr($v,1,$len-2);
							$v = str_replace($DelimChr.$DelimChr,$DelimChr,$v);
						}
					}
				}
			} else {
				$v = true;
			}
			if ($x==='if') {
				self::f_Loc_PrmIfThen($Loc,true,$v);
			} elseif ($x==='then') {
				self::f_Loc_PrmIfThen($Loc,false,$v);
			} else {
				$Loc->PrmLst[$x] = $v;
				if ($WithPos) $Loc->PrmPos[$x] = array($PosName,$PosNend,$PosVal,$Pos,$DelimChr,$DelimCnt);
			}
		}
	}

}

static function f_Loc_PrmIfThen(&$Loc,$IsIf,$Val) {
	$nbr = &$Loc->PrmIfNbr;
	if ($nbr===false) {
		$nbr = 0;
		$Loc->PrmIf = array();
		$Loc->PrmIfVar = array();
		$Loc->PrmThen = array();
		$Loc->PrmThenVar = array();
		$Loc->PrmElseVar = true;
	}
	if ($IsIf) {
		$nbr++;
		$Loc->PrmIf[$nbr] = $Val;
		$Loc->PrmIfVar[$nbr] = true;
	} else {
		$nbr2 = $nbr;
		if ($nbr2===false) $nbr2 = 1; // Only the first 'then' can be placed before its 'if'. This is for compatibility.
		$Loc->PrmThen[$nbr2] = $Val;
		$Loc->PrmThenVar[$nbr2] = true;
	}
}

static function f_Loc_EnlargeToStr(&$Txt,&$Loc,$StrBeg,$StrEnd) {
/*
This function enables to enlarge the pos limits of the Locator.
If the search result is not correct, $PosBeg must not change its value, and $PosEnd must be False.
This is because of the calling function.
*/

	// Search for the begining string
	$Pos = $Loc->PosBeg;
	$Ok = false;
	do {
		$Pos = strrpos(substr($Txt,0,$Pos),(string) $StrBeg[0]);
		if ($Pos!==false) {
			if (substr($Txt,$Pos,strlen($StrBeg))===$StrBeg) $Ok = true;
		}
	} while ( (!$Ok) && ($Pos!==false) );

	if ($Ok) {
		$PosEnd = strpos($Txt,(string) $StrEnd,$Loc->PosEnd + 1);
		if ($PosEnd===false) {
			$Ok = false;
		} else {
			$Loc->PosBeg = $Pos;
			$Loc->PosEnd = $PosEnd + strlen($StrEnd) - 1;
		}
	}

	return $Ok;

}

static function f_Loc_EnlargeToTag(&$Txt,&$Loc,$TagStr,$RetInnerSrc) {
//Modify $Loc, return false if tags not found, returns the inner source of tag if $RetInnerSrc=true

	$AliasLst = &$GLOBALS['_TBS_BlockAlias'];

	// Analyze string
	$Ref = 0;
	$LevelStop = 0;
	$i = 0;
	$TagFct = array();
	$TagLst = array();
	while ($TagStr!=='') {
		// get next tag
		$p = strpos($TagStr, '+');
		if ($p===false) {
			$t = $TagStr;
			$TagStr = '';
		} else {
			$t = substr($TagStr,0,$p);
			$TagStr = substr($TagStr,$p+1);
		}
 		do { // Check parentheses, relative position and single tag
 			$t = trim($t);
	 		$e = strlen($t) - 1; // pos of last char
	 		if (($e>1) && ($t[0]==='(') && ($t[$e]===')')) {
	 			if ($Ref===0) $Ref = $i;
	 			if ($Ref===$i) $LevelStop++;
	 			$t = substr($t,1,$e-1);
	 		} else {
	 			if (($e>=0) && ($t[$e]==='/')) $t = substr($t,0,$e); // for compatibilty
	 			$e = false;
	 		}
 		} while ($e!==false);
		if (isset($AliasLst[$t])) {
			$a = $AliasLst[$t];
			if (is_string($a)) {
				if ($i>999) return false; // prevent from circular alias
				$TagStr = ($TagStr==='') ? $a : $a.'+'.$TagStr;
			} else {
				$TagLst[$i] = $t;
				$TagFct[$i] = $a;
				$i++;
			}
		} else {
			$TagLst[$i] = $t;
			$TagFct[$i] = false;
			$i++;
		}
	}
	
	$TagMax = $i-1;

	// Find tags that embeds the locator
	if ($LevelStop===0) $LevelStop = 1;

	// First tag of reference
	$TagO = self::f_Loc_Enlarge_Find($Txt,$TagLst[$Ref],$TagFct[$Ref],$Loc->PosBeg-1,false,$LevelStop);
	if ($TagO===false) return false;
	$PosBeg = $TagO->PosBeg;
	$LevelStop += -$TagO->RightLevel; // RightLevel=1 only if the tag is single and embeds $Loc, otherwise it is 0 
	if ($LevelStop>0) {
		$TagC = self::f_Loc_Enlarge_Find($Txt,$TagLst[$Ref],$TagFct[$Ref],$Loc->PosEnd+1,true,-$LevelStop);
		if ($TagC==false) return false;
		$PosEnd = $TagC->PosEnd;
		$InnerLim = $TagC->PosBeg;
	} else {
		$PosEnd = $TagO->PosEnd;
		$InnerLim = $PosEnd + 1;
	}

	$RetVal = true;
	if ($RetInnerSrc) {
		$RetVal = '';
		if ($Loc->PosBeg>$TagO->PosEnd) $RetVal .= substr($Txt,$TagO->PosEnd+1,min($Loc->PosBeg,$InnerLim)-$TagO->PosEnd-1);
		if ($Loc->PosEnd<$InnerLim) $RetVal .= substr($Txt,max($Loc->PosEnd,$TagO->PosEnd)+1,$InnerLim-max($Loc->PosEnd,$TagO->PosEnd)-1);
	}

	// Other tags forward
	$TagC = true;
	for ($i=$Ref+1;$i<=$TagMax;$i++) {
		$x = $TagLst[$i];
		if (($x!=='') && ($TagC!==false)) {
			$TagC = self::f_Loc_Enlarge_Find($Txt,$x,$TagFct[$i],$PosEnd+1,true,0);
			if ($TagC!==false) $PosEnd = $TagC->PosEnd;
		}
	}

	// Other tags backward
	$TagO = true;
	for ($i=$Ref-1;$i>=0;$i--) {
		$x = $TagLst[$i];
		if (($x!=='') && ($TagO!==false)) {
			$TagO = self::f_Loc_Enlarge_Find($Txt,$x,$TagFct[$i],$PosBeg-1,false,0);
			if ($TagO!==false) $PosBeg = $TagO->PosBeg;
		}
	}

	$Loc->PosBeg = $PosBeg;
	$Loc->PosEnd = $PosEnd;
	return $RetVal;

}

static function f_Loc_Enlarge_Find($Txt, $Tag, $Fct, $Pos, $Forward, $LevelStop) {
	if ($Fct===false) {
		return self::f_Xml_FindTag($Txt,$Tag,(!$Forward),$Pos,$Forward,$LevelStop,false);
	} else {
		$p = call_user_func_array($Fct,array($Tag,$Txt,$Pos,$Forward,$LevelStop));
		if ($p===false) {
			return false;
		} else {
			return (object) array('PosBeg'=>$p, 'PosEnd'=>$p, 'RightLevel'=> 0); // it's a trick
		}	
	}
}

static function f_Loc_AttBoolean($CurrVal, $AttTrue, $AttName) {
// Return the good value for a boolean attribute
	if ($AttTrue===true) {
		if (self::meth_Misc_ToStr($CurrVal)==='') {
			return '';
		} else {
			return $AttName;
		}
	} elseif (self::meth_Misc_ToStr($CurrVal)===$AttTrue) {
		return $AttName;
	} else {
		return '';
	}
}

static function f_Xml_AttFind(&$Txt,&$Loc,$Move=false,$AttDelim=false) {
// att=div#class ; att=((div))#class ; att=+((div))#class

	$Att = $Loc->PrmLst['att'];
	$p = strrpos($Att,'#');
	if ($p===false) {
		$TagLst = '';
	} else {
		$TagLst = substr($Att,0,$p);
		$Att = substr($Att,$p+1);
	}

	$Forward = (substr($TagLst,0,1)==='+');
	if ($Forward) $TagLst = substr($TagLst,1);
	$TagLst = explode('+',$TagLst);

	$iMax = count($TagLst)-1;
	$WithPrm = false;
	$LocO = &$Loc;
	foreach ($TagLst as $i=>$Tag) {
		$LevelStop = false;
		while ((strlen($Tag)>1) && (substr($Tag,0,1)==='(') && (substr($Tag,-1,1)===')')) {
			if ($LevelStop===false) $LevelStop = 0;
			$LevelStop++;
			$Tag = trim(substr($Tag,1,strlen($Tag)-2));
		}
		if ($i==$iMax) $WithPrm = true;
		$Pos = ($Forward) ? $LocO->PosEnd+1 : $LocO->PosBeg-1;
		unset($LocO);
		$LocO = self::f_Xml_FindTag($Txt,$Tag,true,$Pos,$Forward,$LevelStop,$WithPrm,$WithPrm);
		if ($LocO===false) return false;
	}

	$Loc->AttForward = $Forward;
	$Loc->AttTagBeg = $LocO->PosBeg;
	$Loc->AttTagEnd = $LocO->PosEnd;
	$Loc->AttDelimChr = false;

	if ($Att==='.') {
		// this indicates that the TBS field is supposed to be inside an attribute's value
		foreach ($LocO->PrmPos as $a=>$p ) {
			if ( ($p[0]<$Loc->PosBeg) && ($Loc->PosEnd<$p[3]) ) $Att = $a;
		}
		if ($Att==='.') return false;
	}
		
	$AttLC = strtolower($Att);
	if (isset($LocO->PrmLst[$AttLC])) {
		// The attribute is existing
		$p = $LocO->PrmPos[$AttLC];
		$Loc->AttBeg = $p[0];
		$p[3]--; while ($Txt[$p[3]]===' ') $p[3]--; // external end of the attribute, may has an extra spaces
		$Loc->AttEnd = $p[3];
		$Loc->AttDelimCnt = $p[5];
		$Loc->AttDelimChr = $p[4];
		if (($p[1]>$p[0]) && ($p[2]>$p[1])) {
			//$Loc->AttNameEnd =  $p[1];
			$Loc->AttValBeg = $p[2];
		} else { // attribute without value
			//$Loc->AttNameEnd =  $p[3];
			$Loc->AttValBeg = false;
		}
	} else {
		// The attribute is not yet existing
		$Loc->AttDelimCnt = 0;
		$Loc->AttBeg = false;
		$Loc->AttName = $Att;
	}
	
	// Search for a delimitor
	if (($Loc->AttDelimCnt==0) && (isset($LocO->PrmPos))) {
		foreach ($LocO->PrmPos as $p) {
			if ($p[5]>0) $Loc->AttDelimChr = $p[4];
		}
	}

	if ($Move) return self::f_Xml_AttMove($Txt,$Loc,$AttDelim);

	return true;

}

static function f_Xml_AttMove(&$Txt, &$Loc, $AttDelim=false) {

  if ($AttDelim===false) $AttDelim = $Loc->AttDelimChr;
  if ($AttDelim===false) $AttDelim = '"';

	$sz = $Loc->PosEnd - $Loc->PosBeg + 1;
	$Txt = substr_replace($Txt,'',$Loc->PosBeg,$sz); // delete the current locator
	if ($Loc->AttForward) {
		$Loc->AttTagBeg += -$sz;
		$Loc->AttTagEnd += -$sz;
	} elseif ($Loc->PosBeg<$Loc->AttTagEnd) {
		$Loc->AttTagEnd += -$sz;
	}

	$InsPos = false;
	if ($Loc->AttBeg===false) {
		$InsPos = $Loc->AttTagEnd;
		if ($Txt[$InsPos-1]==='/') $InsPos--;
		if ($Txt[$InsPos-1]===' ') $InsPos--;
		$Ins1 = ' '.$Loc->AttName.'='.$AttDelim;
		$Ins2 = $AttDelim;
		$Loc->AttBeg = $InsPos + 1;
		$Loc->AttValBeg = $InsPos + strlen($Ins1) - 1;
	} else {
		if ($Loc->PosEnd<$Loc->AttBeg) $Loc->AttBeg += -$sz;
		if ($Loc->PosEnd<$Loc->AttEnd) $Loc->AttEnd += -$sz;
		if ($Loc->AttValBeg===false) {
			$InsPos = $Loc->AttEnd+1;
			$Ins1 = '='.$AttDelim;
			$Ins2 = $AttDelim;
			$Loc->AttValBeg = $InsPos+1;
		} elseif (isset($Loc->PrmLst['attadd'])) {
			$InsPos = $Loc->AttEnd;
			$Ins1 = ' ';
			$Ins2 = '';
		} else {
			// value already existing
			if ($Loc->PosEnd<$Loc->AttValBeg) $Loc->AttValBeg += -$sz;
			$PosBeg = $Loc->AttValBeg;
			$PosEnd = $Loc->AttEnd;
			if ($Loc->AttDelimCnt>0) {$PosBeg++; $PosEnd--;}
		}
	}

	if ($InsPos===false) {
		$InsLen = 0;
	} else {
		$InsTxt = $Ins1.'[]'.$Ins2;
		$InsLen = strlen($InsTxt);
		$PosBeg = $InsPos + strlen($Ins1);
		$PosEnd = $PosBeg + 1;
		$Txt = substr_replace($Txt,$InsTxt,$InsPos,0);
		$Loc->AttEnd = $InsPos + $InsLen - 1;
		$Loc->AttTagEnd += $InsLen;
	}

	$Loc->PrevPosBeg = $Loc->PosBeg;
	$Loc->PrevPosEnd = $Loc->PosEnd;
	$Loc->PosBeg = $PosBeg;
	$Loc->PosEnd = $PosEnd;
	$Loc->AttBegM = ($Txt[$Loc->AttBeg-1]===' ') ? $Loc->AttBeg-1 : $Loc->AttBeg; // for magnet=#
	$Loc->AttInsLen = $InsLen; // for CacheField

	return min($Loc->PrevPosEnd,$Loc->PosEnd); // New position to continue the search.

}

static function f_Xml_Max(&$Txt,&$Nbr,$MaxEnd) {
// Limit the number of HTML chars

	$pMax =  strlen($Txt)-1;
	$p=0;
	$n=0;
	$in = false;
	$ok = true;

	while ($ok) {
		if ($in) {
			if ($Txt[$p]===';') {
				$in = false;
				$n++;
			}
		} else {
			if ($Txt[$p]==='&') {
				$in = true;
			} else {
				$n++;
			}
		}
		if (($n>=$Nbr) || ($p>=$pMax)) {
			$ok = false;
		} else {
			$p++;
		}
	}

	if (($n>=$Nbr) && ($p<$pMax)) $Txt = substr($Txt,0,$p).$MaxEnd;

}

static function f_Xml_GetPart(&$Txt, $TagLst, $AllIfNothing=false) {
// Returns parts of the XML/HTML content, default is BODY.

	if (($TagLst===true) || ($TagLst==='')) $TagLst = 'body';

	$x = '';
	$nothing = true;
	$TagLst = explode('+',$TagLst);

	// Build a clean list of tags
	foreach ($TagLst as $i=>$t) {
		if ((substr($t,0,1)=='(') && (substr($t,-1,1)==')')) {
			$t = substr($t,1,strlen($t)-2);
			$Keep = true;
		} else {
			$Keep = false;
		}
		$TagLst[$i] = array('t'=>$t, 'k'=>$Keep, 'b'=>-1, 'e'=>-1, 's'=>false);
	}

	$PosOut = strlen($Txt);
	$Pos = 0;
	
	// Optimized search for all tag types
	do {

		// Search next positions of each tag type
		$TagMin = false;   // idx of the tag at first position
		$PosMin = $PosOut; // pos of the tag at first position
		foreach ($TagLst as $i=>$Tag) {
			if ($Tag['b']<$Pos) {
				$Loc = self::f_Xml_FindTag($Txt,$Tag['t'],true,$Pos,true,false,false);
				if ($Loc===false) {
					$Tag['b'] = $PosOut; // tag not found, no more search on this tag
				} else {
					$Tag['b'] = $Loc->PosBeg;
					$Tag['e'] = $Loc->PosEnd;
					$Tag['s'] = (substr($Txt,$Loc->PosEnd-1,1)==='/'); // true if it's a single tag
				}
				$TagLst[$i] = $Tag; // update
			}
			if ($Tag['b']<$PosMin) {
				$TagMin = $i;
				$PosMin = $Tag['b'];
			}
		}

		// Add the part of tag types
		if ($TagMin!==false) {
			$Tag = &$TagLst[$TagMin];
			$Pos = $Tag['e']+1;
			if ($Tag['s']) {
				// single tag
				if ($Tag['k']) $x .= substr($Txt,$Tag['b']  ,$Tag['e'] - $Tag['b'] + 1);
			} else {
				// search the closing tag
				$Loc = self::f_Xml_FindTag($Txt,$Tag['t'],false,$Pos,true,false,false);
				if ($Loc===false) {
					$Tag['b'] = $PosOut; // closing tag not found, no more search on this tag
				} else {
					$nothing = false;
					if ($Tag['k']) {
						$x .= substr($Txt,$Tag['b']  ,$Loc->PosEnd - $Tag['b'] + 1);
					} else {
						$x .= substr($Txt,$Tag['e']+1,$Loc->PosBeg - $Tag['e'] - 1);
					}
					$Pos = $Loc->PosEnd + 1;
				}
			}
		}

	} while ($TagMin!==false);
	
	if ($AllIfNothing && $nothing) return $Txt;
	return $x;

}

static function f_Xml_FindTagStart(&$Txt,$Tag,$Opening,$PosBeg,$Forward,$Case=true) {
/* Find the start of an XML tag.
$Case=false can be useful for HTML.
$Tag='' should work and found the start of the first tag.
$Tag='/' should work and found the start of the first closing tag.
Encapsulation levels are not feataured yet.
*/

	if ($Txt==='') return false;

	$x = '<'.(($Opening) ? '' : '/').$Tag;
	$xl = strlen($x);

	$p = $PosBeg - (($Forward) ? 1 : -1);

	if ($Case) {
		do {
			if ($Forward) $p = strpos($Txt,$x,$p+1);  else $p = strrpos(substr($Txt,0,$p+1),$x);
			if ($p===false) return false;
			if (substr($Txt,$p,$xl)!==$x) continue; // For PHP 4 only
			$z = substr($Txt,$p+$xl,1);
		} while ( ($z!==' ') && ($z!=="\r") && ($z!=="\n") && ($z!=='>') && ($z!=='/') && ($Tag!=='/') && ($Tag!=='') );
	} else {
		do {
			if ($Forward) $p = stripos($Txt,$x,$p+1);  else $p = strripos(substr($Txt,0,$p+1),$x);
			if ($p===false) return false;
			$z = substr($Txt,$p+$xl,1);
		} while ( ($z!==' ') && ($z!=="\r") && ($z!=="\n") && ($z!=='>') && ($z!=='/') && ($Tag!=='/') && ($Tag!=='') );
	}

	return $p;

}

static function f_Xml_FindTag(&$Txt,$Tag,$Opening,$PosBeg,$Forward,$LevelStop,$WithPrm,$WithPos=false) {
/* This function is a smart solution to find an XML tag.
It allows to ignore full opening/closing couple of tags that could be inserted before the searched tag.
It allows also to pass a number of encapsulations.
To ignore encapsulation and opengin/closing just set $LevelStop=false.
$Opening is used only when $LevelStop=false.
*/

	if ($Tag==='_') { // New line
		$p = self::f_Xml_FindNewLine($Txt,$PosBeg,$Forward,($LevelStop!==0));
		$Loc = new clsTbsLocator;
		$Loc->PosBeg = ($Forward) ? $PosBeg : $p;
		$Loc->PosEnd = ($Forward) ? $p : $PosBeg;
		$Loc->RightLevel = 0;
		return $Loc;
	}

	$Pos = $PosBeg + (($Forward) ? -1 : +1);
	$TagIsOpening = false;
	$TagClosing = '/'.$Tag;
	$LevelNum = 0;
	$TagOk = false;
	$PosEnd = false;
	$TagL = strlen($Tag);
	$TagClosingL = strlen($TagClosing);
	$RightLevel = 0;
	
	do {

		// Look for the next tag def
		if ($Forward) {
			$Pos = strpos($Txt,'<',$Pos+1);
		} else {
			if ($Pos<=0) {
				$Pos = false;
			} else {
				$Pos = strrpos(substr($Txt,0,$Pos - 1),'<'); // strrpos() syntax compatible with PHP 4
			}
		}

		if ($Pos!==false) {

			// Check the name of the tag
			if (strcasecmp(substr($Txt,$Pos+1,$TagL),$Tag)==0) {
				$PosX = $Pos + 1 + $TagL; // The next char
				$TagOk = true;
				$TagIsOpening = true;
			} elseif (strcasecmp(substr($Txt,$Pos+1,$TagClosingL),$TagClosing)==0) {
				$PosX = $Pos + 1 + $TagClosingL; // The next char
				$TagOk = true;
				$TagIsOpening = false;
			}

			if ($TagOk) {
				// Check the next char
				$x = $Txt[$PosX];
				if (($x===' ') || ($x==="\r") || ($x==="\n") || ($x==='>') || ($Tag==='/') || ($Tag==='')) {
					// Check the encapsulation count
					if ($LevelStop===false) { // No encaplusation check
						if ($TagIsOpening!==$Opening) $TagOk = false;
					} else { // Count the number of level
						if ($TagIsOpening) {
							$PosEnd = strpos($Txt,'>',$PosX);
							if ($PosEnd!==false) {
								if ($Txt[$PosEnd-1]==='/') {
									if (($Pos<$PosBeg) && ($PosEnd>$PosBeg)) {$RightLevel=1; $LevelNum++;}
								} else {
									$LevelNum++;
								}
							}
						} else {
							$LevelNum--;
						}
						// Check if it's the expected level
						if ($LevelNum!=$LevelStop) {
							$TagOk = false;
							$PosEnd = false;
						}
					}
				} else {
					$TagOk = false;
				}
			} //--> if ($TagOk)

		}
	} while (($Pos!==false) && ($TagOk===false));

	// Search for the end of the tag
	if ($TagOk) {
		$Loc = new clsTbsLocator;
		if ($WithPrm) {
			self::f_Loc_PrmRead($Txt,$PosX,true,'\'"','<','>',$Loc,$PosEnd,$WithPos);
		} elseif ($PosEnd===false) {
			$PosEnd = strpos($Txt,'>',$PosX);
			if ($PosEnd===false) {
				$TagOk = false;
			}
		}
	}

	// Result
	if ($TagOk) {
		$Loc->PosBeg = $Pos;
		$Loc->PosEnd = $PosEnd;
		$Loc->RightLevel = $RightLevel;
		return $Loc;
	} else {
		return false;
	}

}

static function f_Xml_FindNewLine(&$Txt,$PosBeg,$Forward,$IsRef) {

	$p = $PosBeg;
	if ($Forward) {
		$Inc = 1;
		$Inf = &$p;
		$Sup = strlen($Txt)-1;
	} else {
		$Inc = -1;
		$Inf = 0;
		$Sup = &$p;
	}

	do {
		if ($Inf>$Sup) return max($Sup,0);
		$x = $Txt[$p];
		if (($x==="\r") || ($x==="\n")) {
			$x2 = ($x==="\n") ? "\r" : "\n";
			$p0 = $p;
			if (($Inf<$Sup) && ($Txt[$p+$Inc]===$x2)) $p += $Inc; // Newline char can have two chars.
			if ($Forward) return $p; // Forward => return pos including newline char.
			if ($IsRef || ($p0!=$PosBeg)) return $p0+1; // Backwars => return pos without newline char. Ignore newline if it is the very first char of the search.
		}
		$p += $Inc;
	} while (true);

}

static function f_Xml_GetNextEntityName($Txt, $Pos, &$tag, &$PosBeg, &$p) {
/* 
 $tag : tag name
 $PosBeg : position of the tag
 $p   : position where the read has stop
 $z   : first char after the name
*/

	$tag = '';
	$PosBeg = strpos($Txt, '<', $Pos);
	
	if ($PosBeg===false) return false;
	
	// Read the name of the tag
	$go = true;
	$p = $PosBeg;
	while ($go) {
		$p++;
		$z = $Txt[$p];
		if ($go = ($z!==' ') && ($z!=="\r") && ($z!=="\n") && ($z!=='>') && ($z!=='/') ) {
			$tag .= $z;
		}
	}
	
	return true;
	
}

}
