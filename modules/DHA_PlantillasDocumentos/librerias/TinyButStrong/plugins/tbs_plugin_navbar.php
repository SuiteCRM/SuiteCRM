<?php

/*
********************************************************
TinyButStrong Plug-in: Navigation Bar
Version 1.0.6, on 2008-01-29, by Skrol29
********************************************************
*/

define('TBS_NAVBAR','tbsNavBar');

#[\AllowDynamicProperties]
class tbsNavBar {

	function OnInstall() {
		$this->Version = '1.0.6';
		return array('OnCommand');
	}

	function OnCommand($BlockLst,$Options,$PageCurr,$RecCnt=-1,$PageSize=1) {
		$BlockLst = explode(',',$BlockLst);
		foreach ($BlockLst as $BlockName) {
			$BlockName = trim($BlockName);
			$this->meth_Merge_NavigationBar($this->TBS->Source,$BlockName,$Options,$PageCurr,$RecCnt,$PageSize);
		}
	}

	function meth_Merge_NavigationBar(&$Txt,$BlockName,$Options,$PageCurr,$RecCnt,$PageSize) {

		$TBS =& $this->TBS;

		// Get block parameters
		$PosBeg = 0;
		$PrmLst = array();
		while ($Loc = $TBS->meth_Locator_FindTbs($Txt,$BlockName,$PosBeg,'.')) {
			if (isset($Loc->PrmLst['block'])) $PrmLst = array_merge($PrmLst,$Loc->PrmLst);
			$PosBeg = $Loc->PosEnd;
		}

		// Prepare options
		if (!is_array($Options)) $Options = array('navsize'=>intval($Options));
		$Options = array_merge($Options,$PrmLst);

		// Default options
		if (!isset($Options['navsize'])) $Options['navsize'] = 10;
		if (!isset($Options['navpos']))  $Options['navpos'] = 'step';
		if (!isset($Options['pagemin'])) $Options['pagemin'] = 1;

		// Check options
		if ($Options['navsize']<=0) $Options['navsize'] = 10;
		if ($PageSize<=0) $PageSize = 1;
		if ($PageCurr<$Options['pagemin']) $PageCurr = $Options['pagemin'];

		$CurrPos = 0;
		$CurrNav = array('curr'=>$PageCurr,'first'=>$Options['pagemin'],'last'=>-1,'bound'=>false);

		// Calculate displayed PageMin and PageMax
		if ($Options['navpos']=='centred') {
			$PageMin = $Options['pagemin']-1+$PageCurr - intval(floor($Options['navsize']/2));
		} else {
			// Display by block
			$PageMin = $Options['pagemin']-1+$PageCurr - ( ($PageCurr-1) % $Options['navsize']);
		}
		$PageMin = max($PageMin,$Options['pagemin']);
		$PageMax = $PageMin + $Options['navsize'] - 1;

		// Calculate previous and next pages
		$CurrNav['prev'] = $PageCurr - 1;
		if ($CurrNav['prev']<$Options['pagemin']) {
			$CurrNav['prev'] = $Options['pagemin'];
			$CurrNav['bound'] = $Options['pagemin'];
		}
		$CurrNav['next'] = $PageCurr + 1;
		if ($RecCnt>=0) {
			$PageCnt = $Options['pagemin']-1 + intval(ceil($RecCnt/$PageSize));
			$PageMax = min($PageMax,$PageCnt);
			$PageMin = max($Options['pagemin'],$PageMax-$Options['navsize']+1);
		} else {
			$PageCnt = $Options['pagemin']-1;
		}
		if ($PageCnt>=$Options['pagemin']) {
			if ($PageCurr>=$PageCnt) {
				$CurrNav['next'] = $PageCnt;
				$CurrNav['last'] = $PageCnt;
				$CurrNav['bound'] = $PageCnt;
			} else {
				$CurrNav['last'] = $PageCnt;
			}
		}

		// Merge general information
		$Pos = 0;
		while ($Loc = $TBS->meth_Locator_FindTbs($Txt,$BlockName,$Pos,'.')) {
			$Pos = $Loc->PosBeg + 1;
			$x = strtolower($Loc->SubName);
			if (isset($CurrNav[$x])) {
				$Val = $CurrNav[$x];
				if (($CurrNav['bound']!==false) and ($CurrNav[$x]==$CurrNav['bound'])) {
					if (isset($Loc->PrmLst['endpoint'])) {
						$Val = '';
					}
				}
				$TBS->meth_Locator_Replace($Txt,$Loc,$Val,false);
			}
		}

		// Prepare data to merge
		$Query = '';
		$Data = array();
		$RecSpe = 0;
		$RecCurr = 0;
		if (isset($PrmLst['bmagnet']) and ($PageMin==$PageMax)) {
			// No data to merge
		} else {
			for ($PageId=$PageMin;$PageId<=$PageMax;$PageId++) {
				$RecCurr++;
				if ($PageId==$PageCurr) $RecSpe = $RecCurr;
				$Data[] = array('page'=>$PageId);
			}
		}

		// Merge the nav bar
		$TBS->meth_Merge_Block($Txt,$BlockName,$Data,$Query,'currpage',$RecSpe);

	}

}

?>