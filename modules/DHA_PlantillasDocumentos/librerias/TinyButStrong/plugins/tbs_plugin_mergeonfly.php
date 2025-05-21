<?php

/*
********************************************************
TinyButStrong Plug-in: Merge On Fly
Version 1.1.1, on 2012-08-15, by Skrol29
********************************************************
*/

define('TBS_ONFLY','tbsMergeOnFly');

#[\AllowDynamicProperties]
class tbsMergeOnFly {

	function OnInstall($PackSize=10) {
		$this->Version = '1.1.0';
		$this->PackSize = $PackSize;
		$this->Encaps = 0; // encapsualtion level of blocks
		$this->IsActivated = false;
		$this->Debug = false;
		$this->CountSubRecords = false;
		return array('OnCommand','BeforeMergeBlock','OnMergeSection','AfterMergeBlock');
	}

	function UpdateActivation() {
		$this->IsActivated = ($this->PackSize>0) && ($this->Encaps==1);
	}
	
	function OnCommand($PackSize, $CountSubRecords=false) {
		if ($this->Encaps==0) {
			$this->PackSize = $PackSize;
			$this->CountSubRecords = $CountSubRecords;
		}
		$this->UpdateActivation();
	}

	function BeforeMergeBlock(&$TplSource,&$BlockBeg,&$BlockEnd,$PrmLst) {
		$this->Encaps++;
		$this->UpdateActivation();
		if ($this->IsActivated) {
			$this->Counter = 0;
			$Part2 = substr($TplSource,$BlockBeg);
			$this->TBS->Source = substr($TplSource,0,$BlockBeg);
			$this->TBS->Show(TBS_OUTPUT);
			if ($this->Debug) echo "\n *DEBUG* BeforeMergeBlock : Flush, PackSize=".$this->PackSize.", CountSubRecords=".var_export($this->CountSubRecords,true).", Counter=".$this->Counter.".\n";
			flush();
			$TplSource = $Part2;
			$BlockEnd = $BlockEnd - $BlockBeg;
			$BlockBeg = 0;
		}
	}

	function AfterMergeBlock(&$Buffer,&$DataSrc,&$LocR) {
		$this->Encaps--;
		if ($this->Encaps<=0) {
			$this->PackSize = 0; // deactivate flushing
			$this->Encaps = 0; // avoid encapsualtion errors
			$this->CountSubRecords = false; // for optimization
			if ($this->Debug) echo "\n *DEBUG* AfterMergeBlock : Deactivation.\n";
		}
		$this->UpdateActivation();
	}
	
	function OnMergeSection(&$Buffer,&$NewPart) {
		 // sub-record also count for the flusing.
		if ($this->IsActivated) {
			$this->Counter++;
			if ($this->Counter>=$this->PackSize) {
				echo $Buffer.$NewPart;
				if ($this->Debug) echo "\n *DEBUG* OnMergeSection : Flush, Counter=".$this->Counter.".\n";
				flush();
				$Buffer = '';
				$NewPart = '';
				$this->Counter = 0;
			}
		} elseif ( $this->CountSubRecords && ($this->Encaps!=0) ) {
			$this->Counter++;
		}
	}


}