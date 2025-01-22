<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


/**
 * EdtiViewMetaParser.php
 * This is a utility file that attempts to provide support for parsing pre 5.0 SugarCRM
 * EditView.html files and produce a best guess editviewdefs.php file equivalent.
 *
 * @author Collin Lee
 */

require_once('include/SugarFields/Parsers/MetaParser.php');

class EditViewMetaParser extends MetaParser
{
    public function __construct()
    {
        $this->mView = 'EditView';
    }




    /**
     * parse
     *
     * @param string $filePath The file path of the HTML file to parse
     * @param array $vardefs The module's vardefs
     * @param string $moduleDir The module's directory
     * @param boolean $merge  value indicating whether or not to merge the parsed contents
     * @param array|null $masterCopy The file path of the mater copy of the metadata file to merge against
     * @return string format of metadata contents
     **/
    public function parse($filePath, $vardefs = array(), $moduleDir = '', $merge=false, $masterCopy=null)
    {
        global $app_strings;
        $contents = file_get_contents($filePath);
        $contents = $this->trimHTML($contents);
        $contents = $this->stripFlavorTags($contents);
        $moduleName = '';

        $contents = $this->fixDuplicateTrTags($contents);
        $contents = $this->fixRowsWithMissingTr($contents);

        $tables = $this->getElementsByType("table", $contents);
        $formElements = $this->getFormElements($tables[0]);
        $hiddenInputs = array();
        foreach ($formElements as $elem) {
            $type = $this->getTagAttribute("type", $elem);
            if (preg_match('/hidden/si', $type)) {
                $name = $this->getTagAttribute("name", $elem);
                $value = $this->getTagAttribute("value", $elem);
                $hiddenInputs[$name] = $value;
            }
        }

        // Get the second table in the page and onward
        $tables = array_slice($tables, 1);
        $panels = array();
        $tableCount = 0;
        $addedElements = array();
        $maxTableCountNum = 0;
        $tableCount = 0;
        foreach ($tables as $table) {
            $table = $this->fixTablesWithMissingTr($table);
            $toptr = $this->getElementsByType("tr", $table);
            foreach ($toptr as $tr) {
                $tabledata = $this->getElementsByType("table", $tr);
                $data = array();
                $panelKey = $tableCount == 0 ? "default" : '';
                foreach ($tabledata as $t) {
                    $vals = array_values($this->getElementsByType("tr", $t));
                    if (preg_match_all('/<h4[^>]*?>.*?(\{MOD\.|\{APP\.)(LBL_[^\}]*?)[\}].*?<\/h4>/s', $vals[0], $matches, PREG_SET_ORDER)) {
                        array_shift($vals);
                        $panelKey = count($matches[0]) == 3 ? strtolower($matches[0][2]) : $panelKey;
                    }

                    //If $panelKey is empty use the maxTableCountNum value
                    if (empty($panelKey)) {
                        $panels[$maxTableCountNum++] = $vals;
                    } else {
                        $panels[$panelKey] = $vals;
                    }
                } //foreach
                $tableCount++;
            } //foreach;
        } //foreach

   foreach ($panels as $id=>$tablerows) {
       $metarow = array();

       foreach ($tablerows as $trow) {
           $emptyCount = 0;
           $tablecolumns = $this->getElementsByType("td", $trow);
           $col = array();
           $slot = 0;

           foreach ($tablecolumns as $tcols) {
               $hasRequiredLabel = false;

               //Get the sugar attribute value in the span elements of each table row
               $sugarAttrLabel = $this->getTagAttribute("sugar", $tcols, "'^slot[^b]+$'");

               //If there was no sugar attribute, try id (some versions of EditView.html used this instead)
               if (empty($sugarAttrLabel)) {
                   $sugarAttrLabel = $this->getTagAttribute("id", $tcols, "'^slot[^b]+$'");
               }

               //Check if this field is required
               if (!empty($sugarAttrLabel)) {
                   $hasRequiredLabel = $this->hasRequiredSpanLabel($tcols);
               }

               $sugarAttrValue = $this->getTagAttribute("sugar", $tcols, "'slot[0-9]+b$'");

               //If there was no sugar attribute, try id (some versions of EditView.html used this instead)
               if (empty($sugarAttrValue)) {
                   $sugarAttrValue = $this->getTagAttribute("id", $tcols, "'slot[0-9]+b$'");
               }

               // If there wasn't any slot numbering/lettering then just default to expect label->vallue pairs

               if(count($sugarAttrValue) === 0){
                   $sugarAttrValue = $slot % 2 === 1;
               }

               $slot++;

               if ($sugarAttrValue) {
                   $spanValue = $this->getElementValue("span", $tcols);

                   if (empty($spanValue)) {
                       $spanValue = $this->getElementValue("slot", $tcols);
                   }

                   if (empty($spanValue)) {
                       $spanValue = $this->getElementValue("td", $tcols);
                   }

                   //Get all the editable form elements' names
                   $formElementNames = $this->getFormElementsNames($spanValue);
                   $customField = $this->getCustomField($formElementNames);

                   $name = '';
                   $fields = null;
                   $customCode = null;

                   if (!empty($customField)) {
                       // If it's a custom field we just set the name
                       $name = $customField;
                   } else {
                       if (empty($formElementNames) && preg_match_all('/[\{]([^\}]*?)[\}]/s', $spanValue, $matches, PREG_SET_ORDER)) {
                           // We are here if the $spanValue did not contain a form element for editing.
                           // We will assume that it is read only (since there were no edit form elements)


                           // If there is more than one matching {} value then try to find the right one to key off
                           // based on vardefs.php file.  Also, use the entire spanValue as customCode
                           if (count($matches) > 1) {
                               $name = $matches[0][1];
                               $customCode = $spanValue;
                               foreach ($matches as $pair) {
                                   if (preg_match("/^(mod[\.]|app[\.]).*?/i", $pair[1])) {
                                       $customCode = str_replace($pair[1], '$'.strtoupper($pair[1]), $customCode);
                                   } else {
                                       if (!empty($vardefs[$pair[1]])) {
                                           $name = $pair[1];
                                           $customCode = str_replace($pair[1], '$fields.'.strtolower($pair[1]).'.value', $customCode);
                                       } else {
                                           $phpName = $this->findAssignedVariableName($pair[1], $filePath);
                                           $customCode = str_replace($pair[1], '$fields.'.strtolower($phpName).'.value', $customCode);
                                       } //if-else
                                   }
                               } //foreach
                           } else {
                               //If it is only a label, skip
                               if (preg_match("/^(mod[\.]|app[\.]).*?/i", $matches[0][1])) {
                                   continue;
                               }
                               $name = strtolower($matches[0][1]);
                           }
                       } else {
                           if (is_array($formElementNames)) {
                               if (count($formElementNames) == 1) {
                                   if (!empty($vardefs[$formElementNames[0]])) {
                                       $name = $formElementNames[0];
                                   } else {
                                       // Try to use the EdtiView.php file to find author's intent
                                       $name = $this->findAssignedVariableName($formElementNames[0], $filePath);

                                       //If it's still empty, just use the entire block as customCode
                                       if (empty($vardefs[$name])) {
                                           //Replace any { characters just in case
                                           $customCode = str_replace('{', '{$', $spanValue);
                                       }
                                   } //if-else
                               } else {
                                   //If it is an Array of form elements, it is likely the _id and _name relate field combo
                                   $relateName = $this->getRelateFieldName($formElementNames);
                                   if (!empty($relateName)) {
                                       $name = $relateName;
                                   } else {
                                       //One last attempt to scan $formElementNames for one vardef field only
                                       $name = $this->findSingleVardefElement($formElementNames, $vardefs);
                                       if (empty($name)) {
                                           $fields = array();
                                           $name = $formElementNames[0];
                                           foreach ($formElementNames as $elementName) {
                                               if (isset($vardefs[$elementName])) {
                                                   $fields[] = $elementName;
                                               } else {
                                                   $fields[] = $this->findAssignedVariableName($elementName, $filePath);
                                               } //if-else
                                           } //foreach
                                       } //if
                                   } //if-else
                               } //if-else
                           }
                       }
                   }

                   // Build the entry
                   if (preg_match("/<textarea/si", $spanValue)) {
                       //special case for textarea form elements (add the displayParams)
                       $displayParams = array();
                       $displayParams['rows'] = $this->getTagAttribute("rows", $spanValue);
                       $displayParams['cols'] = $this->getTagAttribute("cols", $spanValue);

                       if (!empty($displayParams['rows']) && !empty($displayParams['cols'])) {
                           $field = array();
                           $field['name'] = $name;
                           $field['displayParams'] = $displayParams;
                       } else {
                           $field = $name;
                       }
                   } else {
                       if (isset($fields) || isset($customCode)) {
                           $field = array();
                           $field['name'] = $name;
                           if (isset($fields)) {
                               $field['fields'] = $fields;
                           }
                           if (isset($customCode)) {
                               $field['customCode'] = $customCode;
                               $field['description'] = 'This field was auto generated';
                           }
                       } else {
                           $emptyCount = $name == '' ? $emptyCount + 1 : $emptyCount;
                           $field = $name;
                       }
                   } //if-else if-else block

                   $addedField = is_array($field) ? $field['name'] : $field;
                   if (empty($addedField) || empty($addedElements[$addedField])) {
                       //Add the meta-data definition for required fields
                       if ($hasRequiredLabel) {
                           if (is_array($field)) {
                               if (isset($field['displayParams']) && is_array($field['displayParams'])) {
                                   $field['displayParams']['required']=true;
                               } else {
                                   $field['displayParams'] = array('required'=>true);
                               }
                           } else {
                               $field = array('name'=>strtolower($field), 'displayParams'=>array('required'=>true));
                           }
                       }
                       $col[] = is_array($field) ? $field : strtolower($field);
                       $addedElements[$addedField] = $addedField;
                   }
               } //if($sugarAttValue)
           } //foreach

           // One last final check.  If $emptyCount does not equal Array $col count, don't add
           if ($emptyCount != count($col)) {
               if ($hasRequiredLabel) {
                   if (is_array($col)) {
                       if (isset($col['displayParams'])) {
                           $col['displayParams']['required']=true;
                       } else {
                           $col['displayParams']=array('required'=>true);
                       }
                   } else {
                       $col = array('name'=>strtolower($col), 'displayParams'=>array('required'=>true));
                   }
               }

               $metarow[] = $col;
           } //if
       } //foreach

       $panels[$id] = $metarow;
   } //foreach

        $this->mCustomPanels = $panels;
        $panels = $this->applyPreRules($moduleDir, $panels);

        $templateMeta = array();
        if ($merge && !empty($masterCopy) && file_exists($masterCopy)) {
            $panels = $this->mergePanels($panels, $vardefs, $moduleDir, $masterCopy);
            $templateMeta = $this->mergeTemplateMeta($templateMeta, $moduleDir, $masterCopy);
        }
        $panels = $this->applyRules($moduleDir, $panels);
        return $this->createFileContents($moduleDir, $panels, $templateMeta);
    }
}
