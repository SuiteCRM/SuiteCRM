<?php

/*
********************************************************
TinyButStrong plug-in: HTML (requires TBS >= 3.3.0)
Version 1.0.7, on 2009-09-07, by Skrol29
Version 1.0.8, on 2013-09-30, by Skrol29
********************************************************
*/

define('TBS_HTML','clsTbsPlugInHtml');
$GLOBALS['_TBS_AutoInstallPlugIns'][] = TBS_HTML; // Auto-install

#[\AllowDynamicProperties]
class clsTbsPlugInHtml {

function OnInstall() {
	$this->Version = '1.0.8';
	return array('OnOperation');
}

function OnOperation($FieldName,&$Value,&$PrmLst,&$Source,$PosBeg,$PosEnd,&$Loc) {
	if ($PrmLst['ope']!=='html') return;
	if (isset($PrmLst['select'])) {
		$this->f_Html_MergeItems($Source,$Value,$PrmLst,$PosBeg,$PosEnd);
		return false; // Return false to avoid TBS merging the current field
	} elseif (isset($PrmLst['look'])) {
		if ($this->f_Html_IsHtml($Value)) {
			$PrmLst['look'] = '1';
			$Loc->ConvMode = false; // no conversion
		} else {
			$PrmLst['look'] = '0';
			$Loc->ConvMode = 1; // conversion to HTML
		}
	}
}

function f_Html_InsertAttribute(&$Txt,&$Attr,$Pos) {
	// Check for XHTML end characters
	if ($Txt[$Pos-1]==='/') {
		$Pos--;
		if ($Txt[$Pos-1]===' ') $Pos--;
	}
	// Insert the parameter
	$Txt = substr_replace($Txt,$Attr,$Pos,0);
}

function f_Html_MergeItems(&$Txt,$ValueLst,$PrmLst,$PosBeg,$PosEnd) {
// Select items of a list, or radio or check buttons.

	$TBS =& $this->TBS;

	if ($PrmLst['select']===true) { // Means set with no value
		$IsList = true;
		$ParentTag = 'select';
		$ItemTag = 'option';
		$ItemPrm = 'selected';
	} else {
		$IsList = false;
		$ParentTag = 'form';
		$ItemTag = 'input';
		$ItemPrm = 'checked';
	}
	
	if (is_array($ValueLst)) {
		$ValNbr = count($ValueLst);		
	} else {
		$ValueLst = array($ValueLst);
		$ValNbr = 1;
	}

	// Values in HTML
	$ValueHtmlLst = array();
	foreach ($ValueLst as $i => $v) {
		$vh = htmlspecialchars($v);
		if ($vh!=$v) $ValueHtmlLst[$vh] = $i;
	}

	$AddMissing = ($IsList and isset($PrmLst['addmissing']));
	if ($AddMissing) $Missing = $ValueLst;
	if (isset($PrmLst['selbounds'])) $ParentTag = $PrmLst['selbounds'];
	$ItemPrmZ = ' '.$ItemPrm.'="'.$ItemPrm.'"';

	$TagO = $TBS->f_Xml_FindTag($Txt,$ParentTag,true,$PosBeg-1,false,1,false);

	if ($TagO!==false) {

		$TagC = $TBS->f_Xml_FindTag($Txt,$ParentTag,false,$PosEnd+1,true,-1,false);
		if ($TagC!==false) {

			// We will work on the zone only
			$ZoneSrc = substr($Txt,$TagO->PosEnd+1,$TagC->PosBeg - $TagO->PosEnd -1);
			$PosBegZ = $PosBeg - $TagO->PosEnd - 1;
			$PosEndZ = $PosEnd - $TagO->PosEnd - 1;

			$DelTbsTag = true;
			// Save and delete the option item that contains the TBS tag
			if ($IsList) {
				// Search for the opening tag before
				$ItemLoc = $TBS->f_Xml_FindTag($ZoneSrc,$ItemTag,true,$PosBegZ,false,false,false);
				if ($ItemLoc!==false) {
					// Check if there is no closing option between the opening option and the TBS tag
					if (strpos(substr($ZoneSrc,$ItemLoc->PosEnd+1,$PosBegZ-$ItemLoc->PosEnd-1),'</')===false) {
						$DelTbsTag = false;
						// Search for the closing tag after (taking care that this closing tag is optional in some HTML version)
						$OptCPos = strpos($ZoneSrc,'<',$PosEndZ+1);
						if ($OptCPos===false) {
							$OptCPos = strlen($ZoneSrc);
						} else {
							if (($OptCPos+1<strlen($ZoneSrc)) and ($ZoneSrc[$OptCPos+1]==='/')) {
								$OptCPos = strpos($ZoneSrc,'>',$OptCPos);
								if ($OptCPos===false) {
									$OptCPos = strlen($ZoneSrc);
								} else {
									$OptCPos++;
								}
							}
						}
						$len = $OptCPos - $ItemLoc->PosBeg;
						$OptSave = substr($ZoneSrc,$ItemLoc->PosBeg,$len); // Save the item
						$PosBegS = $PosBegZ - $ItemLoc->PosBeg;
						$PosEndS = $PosEndZ - $ItemLoc->PosBeg;
						$ZoneSrc = substr_replace($ZoneSrc,'',$ItemLoc->PosBeg,$len); // Delete the item
					}
				}

			}
			
			if ($DelTbsTag) $ZoneSrc = substr_replace($ZoneSrc,'',$PosBegZ,$PosEndZ-$PosBegZ+1);

			// Now, we going to scan all of the item tags
			$Pos = 0;
			$SelNbr = 0;

			while ($ItemLoc = $TBS->f_Xml_FindTag($ZoneSrc,$ItemTag,true,$Pos,true,false,true)) {

				// we get the value of the item
				$ItemValue = false;
			
				if ($IsList) {
					// Look for the end of the item
					$OptCPos = strpos($ZoneSrc,'<',$ItemLoc->PosEnd+1);
					if ($OptCPos===false) $OptCPos = strlen($ZoneSrc);
					if (isset($ItemLoc->PrmLst['value'])) {
						$ItemValue = $ItemLoc->PrmLst['value'];
					} else { // The value of the option is its caption.
						$ItemValue = substr($ZoneSrc,$ItemLoc->PosEnd+1,$OptCPos - $ItemLoc->PosEnd - 1);
						$ItemValue = str_replace(chr(9),' ',$ItemValue);
						$ItemValue = str_replace(chr(10),' ',$ItemValue);
						$ItemValue = str_replace(chr(13),' ',$ItemValue);
						$ItemValue = trim($ItemValue);
					}
					$Pos = $OptCPos;
				} else {
					if ((isset($ItemLoc->PrmLst['name'])) and (isset($ItemLoc->PrmLst['value']))) {
						if (strcasecmp($PrmLst['select'],$ItemLoc->PrmLst['name'])==0) {
							$ItemValue = $ItemLoc->PrmLst['value'];
						}
					}
					$Pos = $ItemLoc->PosEnd;
				}

				// Check the value and select the current item 
				if ($ItemValue!==false) {
					$x = array_search($ItemValue,$ValueLst,false);
					if ( ($x===false) && (isset($ValueHtmlLst[$ItemValue])) ) {
						$x = $ValueHtmlLst[$ItemValue];
					}
					if ($x!==false) {
						if (!isset($ItemLoc->PrmLst[$ItemPrm])) {
							$this->f_Html_InsertAttribute($ZoneSrc,$ItemPrmZ,$ItemLoc->PosEnd);
							$Pos = $Pos + strlen($ItemPrmZ);
						}
						if ($AddMissing) unset($Missing[$x]);
						$SelNbr++;
						if ($IsList and ($SelNbr>=$ValNbr)) {
							// Optimization: in a list of options, values should be unique.
							$AddMissing = false;
							break;
						}
					}

				}

			} //--> while ($ItemLoc = ... ) {

			if ($AddMissing and isset($OptSave)) {
				foreach ($Missing as $x) {
					$ZoneSrc = $ZoneSrc.substr($OptSave,0,$PosBegS).$x.substr($OptSave,$PosEndS+1);
				}
			}

			$Txt = substr_replace($Txt,$ZoneSrc,$TagO->PosEnd+1,$TagC->PosBeg-$TagO->PosEnd-1);

		} //--> if ($TagC!==false) {
	} //--> if ($TagO!==false) {


}

function f_Html_IsHtml(&$Txt) {
// This function returns True if the text seems to have some HTML tags.

	// Search for opening and closing tags
	$pos = strpos($Txt,'<');
	if ( ($pos!==false) and ($pos<strlen($Txt)-1) ) {
		$pos = strpos($Txt,'>',$pos + 1);
		if ( ($pos!==false) and ($pos<strlen($Txt)-1) ) {
			$pos = strpos($Txt,'</',$pos + 1);
			if ( ($pos!==false)and ($pos<strlen($Txt)-1) ) {
				$pos = strpos($Txt,'>',$pos + 1);
				if ($pos!==false) return true;
			}
		}
	}

	// Search for special char
	$pos = strpos($Txt,'&');
	if ( ($pos!==false) and ($pos<strlen($Txt)-1) ) {
		$pos2 = strpos($Txt,';',$pos+1);
		if ($pos2!==false) {
			$x = substr($Txt,$pos+1,$pos2-$pos-1); // We extract the found text between the couple of tags
			if (strlen($x)<=10) {
				if (strpos($x,' ')===false) return true;
			}
		}
	}

	// Look for a simple tag
	$Loc1 = $this->TBS->f_Xml_FindTag($Txt,'BR',true,0,true,false,false); // line break
	if ($Loc1!==false) return true;
	$Loc1 = $this->TBS->f_Xml_FindTag($Txt,'HR',true,0,true,false,false); // horizontal line
	if ($Loc1!==false) return true;

	return false;

}

}

?>