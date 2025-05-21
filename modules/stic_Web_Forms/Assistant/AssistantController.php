<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

class stic_Web_FormsAssistantController extends stic_Web_FormsController 
{
    public $persistentData;
    public $step;
    
    /**
     * Execute the action indicated in the step variable
    */
    function doAction() 
    {
	    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Processing Class [{$this->webFormClass}] Action [{$this->action}] Step [{$this->step}] ... ");
    
	    // If the step has not been defined, we start with the first
	    if (empty($this->step)) 
	    {	
		    $this->step = 'Step1';
	    }
	    $function = "action{$this->step}";
	    
	    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Executing method [{$function}] ... ");
	    if (!method_exists($this, $function)) {
		    $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The method [{$function}] does not exist. ");
		    $this->no_action();
	    } 
	    else {
		    // Retrieve the step data (the persistence variable that is not present in the parent controller)
		    $this->getFilteredParams();
		    // Save the possible selected fields
		    $this->saveSelectedFields();
		    // Call the method responsible for managing the step
		    $this->$function();
	    }
    }
    
    /**
     * The last form creation action is common for all attendees
     */
    function actionStepDownload() 
    {
	    global $app_list_strings, $current_language;
    
	    // Scripts to include
	    $serverURL = $this->getServerURL();
	    $serverURL = rtrim ($serverURL,"/"); // Remove the last slash if there is already one, to avoid errors if the siteurl
	    $this->view_object_map['FORM']['SCRIPTS'] = array (
		    getJSPath("{$serverURL}/cache/include/javascript/sugar_grp1_jquery.js"),
		    getJSPath("{$serverURL}/cache/include/javascript/sugar_grp1_yui.js"),
		    getJSPath("{$serverURL}/cache/include/javascript/sugar_grp1.js"),
	    );
		$this->view_object_map['FORM']['SCRIPTS_DEFER'] = [];
		$recaptchaConfig = $this->getRecaptchaConfiguration();
		if($recaptchaConfig != null) {
			// Set reCAPTCHA script reference, by version
			if($recaptchaConfig['VERSION']=="2") {
				$this->view_object_map['FORM']['SCRIPTS_DEFER'][] = "https://www.google.com/recaptcha/api.js";
			}
			if($recaptchaConfig['VERSION']=="3") {
				$this->view_object_map['FORM']['SCRIPTS'][] = "https://www.google.com/recaptcha/api.js?render=".$recaptchaConfig['WEBKEY'];
			}
		}
    
	    // CSS to include
	    $this->view_object_map['FORM']['CSS'] = array (
		    getJSPath("{$serverURL}/cache/themes/SuiteP/css/Stic/style.css")
	    );
    
	    // Regenerate the text area
	    $this->bodyHTML = $this->replaceInputText2TextArea($this->bodyHTML);

		// Add reCAPTCHA widget for version 2
		if($recaptchaConfig != null && $recaptchaConfig['VERSION']=="2") {
			$this->bodyHTML .= "<div class='g-recaptcha' data-sitekey='". $recaptchaConfig['WEBKEY'] ."'></div>";
		}

		// Add hidden input if there are multiple reCAPTCHAs
		if($recaptchaConfig != null && $recaptchaConfig['NAME']!="") {
			$this->bodyHTML .= "<input name='stic-recaptcha-id' id='stic-recaptcha-id' type='hidden' value='". $recaptchaConfig['NAME'] ."' />";
		}
    
	    // Clean the editor's modifications in the original code
	    require_once('include/SugarTinyMCE.php');
	    $SugarTiny =  new SugarTinyMCE();
		
	    // Save the html and header. Form creation is done in the view
	    $this->view_object_map['LANGHEADER'] = get_language_header();
	    $this->view_object_map['HTML'] = $this->bodyHTML;
    
	    // Map the stored data in case they are needed in the view layer
	    $this->view_object_map['PERSISTENT_DATA'] = $this->persistentData;
    
	    $this->view = 'webformsstep7';
    }
    
	/**
	 * Returns the selected recaptcha configuration
	 * A reCAPTCHA configuration is an array:
     * ['NAME'] -> The name for the reCAPTCHA configuration (empty if is main configuration)
     * ['KEY'] -> Private Key for the reCAPTCHA configuration
     * ['WEBKEY'] -> Public Web Key for the reCAPTCHA configuration
     * ['VERSION'] -> reCAPTCHA version for the configuration
	 */
	function getRecaptchaConfiguration(){
		$recaptchaConfiguration = null;

		if($this->persistentData['include_recaptcha']) {
			// Get selected reCAPTCHA configuration
			if(isset($this->persistentData['recaptcha_selected']) && $this->persistentData['recaptcha_selected']!='') {
				$index = intval($this->persistentData['recaptcha_selected']);
				$key = $this->persistentData['recaptcha_configKeys'][$index];
				$recaptchaConfiguration = $this->persistentData['recaptcha_configs'][$key];
			}
		}
		return $recaptchaConfiguration;
	}
	
    /**
     * Reconstitute the textsareas that have been changed by input text to display them in the editor
     * @param String $bodyHTML
     * @return String
     */
    function replaceInputText2TextArea($bodyHTML) 
    {
	    // Begin replacing text input tags that have been marked with text area tags get array of text areas strings to process
	    $bodyHTML = html_entity_decode($bodyHTML,ENT_QUOTES);
	        
		while (strpos($bodyHTML, "ta_replace") !== false)
		{
            //define the marker edges of the sub string to process (opening and closing tag brackets)
            $marker = strpos($bodyHTML, "ta_replace");
            $startBorder = strpos($bodyHTML, "input", $marker) - 1;// to account for opening '<' char;
            $endBorder = strpos($bodyHTML, '>', $startBorder); // get the closing tag after marker ">";
            
            //extract the input tag string
            $workingStr = substr($bodyHTML, $marker-3, $endBorder-($marker-3) );
            
            //replace input markup with text areas markups
            $newStr = str_replace('input','textarea',$workingStr);
            $newStr = str_replace("type='text'", ' ', $newStr);
            $newStr = $newStr . '></textarea';
            
            //replace the marker with generic term
            $newStr = str_replace('ta_replace', 'sugarslot', $newStr);
            
            //merge the processed string back into bodyhtml string
            $bodyHTML = str_replace($workingStr , $newStr, $bodyHTML);
		}
		
	    // End replacing marked text inputs with text area tags
	    return $bodyHTML;
    }
    
    /**
     * Save the selected fields received via request
     */
    function saveSelectedFields() 
    {
        $selectedFields = $this->persistentData['SELECTED_FIELDS'] ?? null;        
        if (empty($selectedFields)) 
        {
            $selectedFields = array();
		}     
		   
        // If the selected module cannot be set, it may be because we come from an action that is not field selection
        if (empty($this->selection_module_name)) {
			$GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Cannot set the module to map the fields");
        } 
        else {
		    // Restarts the fields of the two columns of the received module
		    $selectedFields[$this->selection_module_name] = array (
			    'COL1_FIELDS' => empty($this->colsFirst) ? array() : $this->colsFirst,
			    'COL2_FIELDS' => empty($this->colsSecond) ? array(): $this->colsSecond
		    );
        }
        $this->persistentData['SELECTED_FIELDS'] = $selectedFields;
    }
    
    /**
     * Check the consistency of the saved data as selected fields
	 * It is necessary in case the user initially indicates that he wants to include modules that he does not include later
	 * and at some point I have selected fields from a module that finally does not include
	 * @param $modules Array of valid modules
	 */
	function ensureSelectedFields($modules) 
	{
        $selectedFields = $this->persistentData['SELECTED_FIELDS'];
        
        if (empty($selectedFields)) 
        {
	        $selectedFields = array();
        }
        
        $ensuredSelectedFields = array();
		foreach ($modules as $module) 
		{
	        if (isset($selectedFields[$module])) 
            {
                $ensuredSelectedFields[$module]	= $selectedFields[$module];
	        }
        }
        $this->persistentData['SELECTED_FIELDS'] = $ensuredSelectedFields;
    }
    
    /**
     * Performs the necessary field mapping for the specified step
	 * @param $currentStep
	 * @param $nextStep
	 * @param $backStep
	 */
	function mapStepNavigation($currentStep, $nextStep, $backStep = '') 
	{
    	$this->view_object_map['URL'] = "index.php";
    	$this->view_object_map['ACTION'] = 'assistant';
    	$this->view_object_map['MODULE'] = $this->module; //'stic_Web_Forms';
    	$this->view_object_map['RETURN_MODULE'] = $this->return_module;
    	$this->view_object_map['RETURN_ID'] = $this->return_id;
    	$this->view_object_map['RETURN_ACTION'] = $this->return_action;
    	$this->view_object_map['PERSISTENT_DATA'] = $this->persistentData;
    	$this->view_object_map['PREV_STEP'] = $currentStep;
    	$this->view_object_map['BACK_STEP'] = empty($backStep) ? $this->return_action : $backStep;
    	$this->view_object_map['STEP'] = $nextStep;
    	$this->view_object_map['WEBFORMCLASS'] = $this->webFormClass;
    }
    
    /**
     * Returns the filtered input data
    	* @param Array $defaultValues
    	* @param Const $filter
    	* @return Array
    	*/
    function getFilteredParams ($defaultValues = null, $filter = FILTER_DEFAULT) 
    {
    	$ret = parent::getFilteredParams();	// Call the parent class method
    
    	// Treat the persistence field
    	if (! empty($this->persistentData)) {
			// Ensure $this->persistentData is correct
			if(strpos($this->persistentData, '"') !== false) {
				$this->persistentData = htmlentities($this->persistentData);
			}
    		$this->persistentData = json_decode(html_entity_decode($this->persistentData),true);
    	} 
    	else {
    		$this->persistentData = array();
    	}
    	return $ret;
    }
    
    /**
     * Save the indicated Request parameters in persistentData
    */
    function saveRequestParams ($paramsToSave) 
    {
		foreach($paramsToSave as $paramName => $persistentName) 
		{
    		if (isset($this->$paramName)) 
    		{
    			$this->persistentData[$persistentName] = $this->$paramName;
    		}
    	}
    }
    
    /**
     * Prepare the selected fields for rendering on the web form
	 * @param unknown $inRequiredFields		Indicate which fields should be forced as necessary or remove their mandatory property (array ('module' => 'field' => true/false))
	 * @param unknown $outBoolFields		Returns an array with the Boolean fields of the form
	 * @param unknown $outRequiredFields	Returns an array with the required fields of the form (the forced ones plus the module's own)
	 */
    function prepareFieldsToResultForm ($inRequiredFields, &$outBoolFields, &$outRequiredFields) 
    {
    	global $timedate, $app_list_strings;
    	$this->view_object_map['FORM']['FIELDSET'] = array();
    
    	// Initialize the output parameters
    	$outRequiredFields = array();
    	$outBoolFields = array();
    
    	// Retrieve data from fields to display
		foreach ($this->persistentData['SELECTED_FIELDS'] as $module => $cols) 
		{
    		$fieldSet = array (
    			"NAME" => $module,
    			"LABEL" => $app_list_strings['moduleList'][$module]
    		);
		    
    		// Retrieve the module bean
    		$bean = BeanFactory::getBean($module);
    
    		// Generate the fieldset columns
    		$fieldSet['ROWS'] = array();
    		$fieldsCount = 0; // Field counter per module.
    
    		// Go through the columns of the module
    		foreach($cols as $col => $fields) 
    		{
    			$row = 0;	// Row Index
    			$formCol = ('COL1_FIELDS' == $col ? 0 : 1);	// Index of the column for the form
    
    			// Go through the column fields
    			foreach($fields as $field) 
    			{
    				$formField = array();
    				// Field can be an array or a string
    				if (is_array($field))  // If it is an array it allows overwriting the default definition of the field
    				{ 
    					$fieldName = $field['name'];
    					$forceOptions = $field['options'] ?? null;
    					$formField['hidden'] = $field['hidden'] ?? null;
    					$formField['script'] = $field['script'] ?? null;
    				} 
    				else {
    					$fieldName = $field;
    				}
    
    				// Check that the field is defined in the bean
    				if (! empty($bean->field_defs[$fieldName])) 
    				{
    					$formField['name'] = "{$module}___{$fieldName}";	// Generate a new name concatenating the module name
    					$formField['DEF'] = $bean->field_defs[$fieldName];
    
    					switch ($formField['DEF']['type']) 
    					{
    						// If it is a date field, save the date format
    						case 'date':
    						case 'datetime':
    						case 'datetimecombo':
    							$formField['dateformat'] = $timedate->get_cal_date_format();
    							break;
						    
    						// If it is an options field, load the list of available options
    						case 'enum':
    						case 'multienum':
    						case 'radioenum': 	
    							$formField['options'] = isset($forceOptions) ? $forceOptions : $app_list_strings[$formField['DEF']['options']];
    							$formField['selected_options'] = array();
    							break;
    
    						// If the field is Boolean, check if it exists in the list of Boolean fields, otherwise add it
    						case 'bool':
    							if (! in_array($formField['name'], $outBoolFields)) 
    							{
    								array_push($outBoolFields, $formField['name']);
    							}
    							break;
    					}
    
    					$formField['label'] = preg_replace('/:$/', '', translate($formField['DEF']['vname'], $module)).":";
    
    					// If the field appears the list of required fields, change its definition
    					if (isset($inRequiredFields[$module][$fieldName])) 
    					{
    						$formField['DEF']['required'] = $inRequiredFields[$module][$fieldName];
    					}
    
    					// If the field is required and was not included, it is included in the output array
    					if (isset($formField['DEF']['required']) && $formField['DEF']['required'] && ! in_array($formField['name'], $outRequiredFields)) 
    					{
    						array_push($outRequiredFields, $formField['name']);
    					}
    				}
    
    				// Add the treated field to the fields array
    				$fieldSet['ROWS'][$row][$formCol] = array('FIELD' => $formField);
    
    				// Add a field to the counter
    				$fieldsCount++;
    
    				// Advance a row
    				$row++;
    			}
    		}
    
    		// If the fieldset has fields, add it to the fieldset list
    		if ($fieldsCount) 
    		{
    			array_push($this->view_object_map['FORM']['FIELDSET'], $fieldSet);
    		}
    	}
    }
    
    /**
     * Extract from the list of available fields the fields indicated in the colFields list and return an array with the extracted values
    	* @param unknown $availableFields
    	* @param unknown $colFields
    	* @return array;
    	*/
	function extractFromAvailableFields(&$availableFields, $extract) 
	{
    	$ret = array();
    	$newAvailable = array();
    	if (empty($extract) || ! is_array($extract)) {
    		$GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There are no values ​​to extract.");
    	} 
    	else {
    		$searchString = ",".implode(",", $extract).",";
			foreach($availableFields as $key => $field) 
			{
    			// Field 1 contains the name of the field. SearchString contains the names of fields to search separated by commas.
    			// We will look for the name of the field (adding the commas) in the string, if it finds the value, it means that it is one of the fields searched
    			if (strpos($searchString, ",{$field[1]},") !== false) {
    				array_push($ret, $field); // Save the array to return
    			} 
    			else {
    				array_push($newAvailable, $field); // Copy the field to the new array
    			}
    		}
    		$availableFields = $newAvailable;
    	}
    	return $ret;
    }
    
    /**
     * Function to return the array of fields to show in form
    	* @param $bean  bean Bean object to access fielddefs
    	* @param $requiredFields (Optional) Array with required fields
    	* @return array
    	*/
	function getFieldsToShow($bean, $requiredFields = null) 
	{
    	global $app_strings;
    	$fields = array();
    
    	if (empty($requiredFields)) 
    	{
    		$requiredFields = array();
    	}
    
		if (empty($forceOptionalFields)) 
		{
			$forceOptionalFields = array();
		} 
    
    	foreach($bean->field_defs as $field_def) 
    	{
    		$emailFields = $field_def['name'] == 'email1' || $field_def['name']== 'email2';
    
    		// Conditions of exclusion from the list by type
    		if (($field_def['type'] == 'relate' && empty($field_def['custom_type'])) 
    			|| $field_def['type'] == 'id' 
    			|| $field_def['type'] == 'assigned_user_name' 
    			|| $field_def['type'] == 'link' 
    			|| $field_def['type'] == 'currency_id' 
    			|| $field_def['type'] == 'image' 
    			|| $field_def['type'] == 'iframe' 
    			|| $field_def['type'] == 'wysiwyg' 
    			|| (isset($field_def['source']) && $field_def['source'] == 'non-db' && !$emailFields)
    		   ) 
    		{
    			$GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Field [{$field_def['name']}] excluded by type [{$field_def['type']}]");
    			continue;
    		}
    
    		// Conditions of exclusion from the list by field name
    		if ($field_def['name'] == 'deleted' 
    			|| $field_def['name'] == 'converted' 
    			|| $field_def['name'] == 'date_entered' 
    			|| $field_def['name'] == 'date_modified' 
    			|| $field_def['name'] == 'modified_user_id' 
    			|| $field_def['name'] == 'assigned_user_id' 
    			|| $field_def['name'] == 'created_by' 
    			|| $field_def['name'] == 'team_id'
    		   ) 
    		{
    			$GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Field [{$field_def['name']}] excluded by name.");
    			continue;
    		}
	
    		// Conditions of exclusion from the list by field no editable 
			if ((in_array('studio', $field_def))
				&& ((!empty($field_def['studio']) && $field_def['studio'] === false) || ($field_def['studio']['view'] ?? true) === false)
			   )
    		{
    			$GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Field [{$field_def['name']}] excluded by field no editable.");
    			continue;
    		}
    

    		$field_def['vname'] = preg_replace('/:$/','',translate($field_def['vname'], $bean->module_name));
    
    		$colArr = array();
			$GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Field [" . ($field_def['name'] ?? 'undefined') . "] - Type [" . ($field_def['type'] ?? 'undefined') . "] - FieldDefRequird [" . ($field_def['required'] ?? 'undefined') . "] - Required [" . ($requiredFields[$field_def['name'] ?? ''] ?? 'undefined') . "]");

    		$colsName=$field_def['vname'];
    
    		// If the field name appears in the array of required fields, it has priority over the field definition
    		if (isset($requiredFields[$field_def['name']])) {
    			$required = $requiredFields[$field_def['name']];
    		} 
    		else {
    			$required = ! empty($field_def['required']);
    		}
		    
    		if ($required) 
    		{
    			$colsName.=" ".$app_strings['LBL_REQUIRED_SYMBOL'];
    			$colArr[2]=true;
    		}
    		$colArr[0]=$colsName;
    		$colArr[1]=$field_def['name'];
    		array_push($fields, $colArr);
    	}
    	return $fields;
    }
    
    /**
     * Map the fields to display in the field selection grid
     */
	function mapModuleFields($bean, $requiredFields = null, $hideFields = null) 
	{
    	// Indicates the module for which we are working
    	$this->view_object_map['SELECTION_MODULE_NAME'] = $bean->module_name;
    
    	// Retrieve the list of available fields
    	$availableFields = $this->getFieldsToShow($bean, $requiredFields);
    
		// If there are fields to delete we delete them		
		if ($bean->module_name == 'stic_Registrations'){
			$hideFields[] = "registration_date";
		}
    	$this->extractFromAvailableFields($availableFields, $hideFields);
    
    	// Retrieve the persistence list (for navigation by the assistant)
    	// If the field list exists, it retrieves the data of each column
    	$cols1Array = array();
    	$cols2Array = array();
    	if (!empty($this->persistentData['SELECTED_FIELDS'][$bean->module_name])) 
    	{
    		$cols1Array = $this->extractFromAvailableFields($availableFields, $this->persistentData['SELECTED_FIELDS'][$bean->module_name]['COL1_FIELDS']);
    
    		$cols2Array = $this->extractFromAvailableFields($availableFields, $this->persistentData['SELECTED_FIELDS'][$bean->module_name]['COL2_FIELDS']);
    	} else {
			// Extracting the required fields pf the availables and add to the column 1
			$extractRequiredFields = array();
			foreach ($availableFields as $key=>$value) 
			{
				if (!empty($value[2])){
					array_push($extractRequiredFields, $value[1]);
				}
			}
			$cols1Array = $this->extractFromAvailableFields($availableFields, $extractRequiredFields);
		}
    
    	$this->view_object_map['COL1_FIELDS'] = $cols1Array;
    	$this->view_object_map['COL2_FIELDS'] = $cols2Array;
    	$this->view_object_map['AVAILABLE_FIELDS'] = $availableFields;
    }
    
    /**
     * Formats as json an array for its output as the value of a hidden html field
     * @param unknown $data
     */
    public static function formatJsonData2HiddenField($data) 
    {
    	return htmlspecialchars(urlencode(json_encode($data)));
    }
}
