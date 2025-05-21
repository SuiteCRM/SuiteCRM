<?php

/*
********************************************************
TinyButStrong plug-in: ByPage (requires TBS >= 3.1.0)
Version 1.0.5, on 2006-10-26, by Skrol29
********************************************************
*/

define('TBS_BYPAGE','tbsByPage');

#[\AllowDynamicProperties]
class tbsByPage {

	function OnInstall() {
		$this->Version = '1.0.5';
		$this->PageSize = 0;
		return array('OnCommand','BeforeMergeBlock','AfterMergeBlock');
	}

	function OnCommand($PageSize,$PageNum=0,$RecKnown=0) {
		// Activate ByPage Mode
		$this->PageSize = $PageSize;
		$this->PageNum = $PageNum;
		$this->RecKnown = $RecKnown;
		$this->RecNbr = 0;
	}

	function BeforeMergeBlock(&$TplSource,&$BlockBeg,&$BlockEnd,$PrmLst,&$Src) {

		if ($this->PageSize<=0) return;   // ByPage Mode not actived
		if (isset($Src->ByPage)) return;  // ByPage Mode already processed for the current Data source
		if ($Src->RecSet===false) return; // No data available

		if ($Src->RecSaved) {
			// Data is an array

			$this->RecNbr = count($Src->RecSet);
			if ($this->PageNum==-1) { // Last record
				$Reminder = $this->RecNbr % $this->PageSize;
				if ($Reminder==0) $Reminder = $this->PageSize;
				$Src->RecNumInit = $this->RecNbr - $Reminder;
			} else {
				$Src->RecNumInit = ($this->PageNum-1) * $this->PageSize;
			}

			$Src->RecSet = array_slice($Src->RecSet, $Src->RecNumInit, $this->PageSize);
			$Src->RecNbr = $Src->RecNumInit + count($Src->RecSet);

		} else {

			// Data is not an array => read records, saving the last page in $this->RecBuffer

			if ($this->PageNum==-1) {
				$RecStop = -1;
			} else {
				$RecStop = $this->PageNum * $this->PageSize;
			}

			// Init internal buffer
			unset($Src->RecBuffer);
			$Src->RecSaving = true;

			// Read records
		 	$Src->RecBuffer = array();
			$RecNum = 0;
			$Modulo = 0;
			$ModuloStop = $this->PageSize + 1;
			while (($Src->CurrRec!==false) and ($RecNum!==$RecStop)) {
				$Src->DataFetch(); // Updates $Src->RecBuffer
				if ($Src->CurrRec!==false) {
					$Modulo++;
					$RecNum++;
					if ($Modulo===$ModuloStop) {
						$Src->RecBuffer = array($Src->RecKey => $Src->CurrRec);
						$Src->RecNumInit += $this->PageSize;
						$Modulo = 1;
					}
				}
			}

			// Retreiveing the number of records
			$this->RecNbr = $RecNum;
			if ($this->RecKnown==-1) {
				$Src->RecSaving = false;
				while ($Src->CurrRec!==false) {
					$Src->DataFetch();
					if ($Src->CurrRec!==false) $this->RecNbr++;
				}
				$Src->RecSaving = true;
			}
			
			$Src->DataClose(); // Close the real recordset source

		}

		$x = '';
		$Src->DataOpen($x); // Read first record, like it is done by TBS before calling this method

		// Deactivate ByPage Mode
		$this->PageSize = 0;
		$Src->ByPage = true;

	}

	function AfterMergeBlock(&$Buffer,&$Src) {
		if (!isset($Src->ByPage)) return;
		if ($this->RecKnown==-1) $Src->RecNum = $this->RecNbr;
	}	

}

?>