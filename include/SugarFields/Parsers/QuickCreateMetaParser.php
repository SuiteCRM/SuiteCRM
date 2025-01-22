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
 * QuickCreate.html files and produce a best guess editviewdefs.php file equivalent.
 *
 * @author Collin Lee
 */

require_once('include/SugarFields/Parsers/MetaParser.php');

class QuickCreateMetaParser extends MetaParser
{
    public function __construct()
    {
        $this->mView = 'QuickCreate';
    }




    /**
     * parse
     *
     * @param $filePath The file path of the HTML file to parse
     * @param $vardefs The module's vardefs
     * @param $moduleDir The module's directory
     * @param $merge boolean value indicating whether or not to merge the parsed contents
     * @param $masterCopy The file path of the mater copy of the metadata file to merge against
     * @return String format of metadata contents
     **/
    public function parse($filePath, $vardefs = array(), $moduleDir = '', $merge=false, $masterCopy=null)
    {
        global $app_strings;
        $contents = file_get_contents($filePath);

        // The contents are not well formed so we add this section to make it easier to parse
        $contents = $this->trimHTML($contents) . '</td></tr></table>';
        $moduleName = '';

        $forms = $this->getElementsByType("form", $contents);
        $tables = $this->getElementsByType("table", $forms[0] . "</td></tr></table>");
        $mainrow = $this->getElementsByType("tr", $tables[1]);
        $rows = substr($mainrow[0], strpos($mainrow[0], "</tr>"));
        $tablerows = $this->getElementsByType("tr", $rows);

        foreach ($tablerows as $trow) {
            $emptyCount = 0;
            $tablecolumns = $this->getElementsByType("td", $trow);
            $col = array();
            $slot = 0;

            foreach ($tablecolumns as $tcols) {
                $sugarAttrLabel = $this->getTagAttribute("sugar", $tcols, "'^slot[^b]+$'");
                $sugarAttrValue = $this->getTagAttribute("sugar", $tcols, "'slot[0-9]+b$'");

                // If there wasn't any slot numbering/lettering then just default to expect label->vallue pairs
                $sugarAttrLabel = count($sugarAttrLabel) != 0 ? $sugarAttrLabel : ($slot % 2 == 0) ? true : false;
                $sugarAttrValue = count($sugarAttrValue) != 0 ? $sugarAttrValue : ($slot % 2 == 1) ? true : false;

                $slot++;

                if ($sugarAttrValue) {
                    $spanValue = strtolower($this->getElementValue("span", $tcols));
                    if (empty($spanValue)) {
                        $spanValue = strtolower($this->getElementValue("slot", $tcols));
                    }
                    if (empty($spanValue)) {
                        $spanValue = strtolower($this->getElementValue("td", $tcols));
                    }

                    //Get all the editable form elements' names
                    $formElementNames = $this->getFormElementsNames($spanValue);
                    $customField = $this->getCustomField($formElementNames);

                    $name = '';
                    $readOnly = false;
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
                                    if (preg_match("/^(mod[\.]|app[\.]).*?/s", $pair[1])) {
                                        $customCode = str_replace($pair[1], '$'.strtoupper($pair[1]), $customCode);
                                    } else {
                                        if (!empty($vardefs[$pair[1]])) {
                                            $name = $pair[1];
                                            $customCode = str_replace($pair[1], '$fields.'.$pair[1].'.value', $customCode);
                                        }
                                    }
                                } //foreach
                            } else {
                                //If it is only a label, skip
                                if (preg_match("/^(mod[\.]|app[\.]).*?/s", $matches[0][1])) {
                                    continue;
                                } else {
                                    if (preg_match("/^[\$].*?/s", $matches[0][1])) {
                                        $name = '{' . strtoupper($matches[0][1]) . '}';
                                    } else {
                                        $name = $matches[0][1];
                                    }
                                }
                            }

                            $readOnly = true;
                        } else {
                            if (is_array($formElementNames)) {
                                if (count($formElementNames) == 1) {
                                    if (!empty($vardefs[$formElementNames[0]])) {
                                        $name = $formElementNames[0];
                                    }
                                } else {
                                    $fields = array();
                                    foreach ($formElementNames as $elementName) {
                                        // What we are doing here is saying that we will add all your fields assuming
                                        // there are none that are of type relate or link.  However, if we find such a type
                                        // we'll take the first one found and assume that is the field you want (the SugarFields
                                        // library will handle rendering the popup and select and clear buttons for you).
                                        if (isset($vardefs[$elementName])) {
                                            $type = $vardefs[$elementName]['type'];
                                            if ($type != 'relate' && $type != 'link') {
                                                $fields[] = $elementName;
                                                $name = $elementName;
                                            } else {
                                                unset($fields);
                                                $name = $elementName;
                                                break;
                                            }
                                        }
                                    }
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
                        $col[] = $field;
                    } else {
                        if ($readOnly) {
                            $field = array();
                            $field['name'] = $name;
                            $field['type'] = 'readonly';
                            if (isset($customCode)) {
                                $field['customCode'] = $customCode;
                            } //if
                            $col[] = $field;
                        } else {
                            if (isset($fields) || isset($customCode)) {
                                $field = array();
                                $field['name'] = $name;
                                if (isset($fields)) {
                                    $field['fields'] = $fields;
                                }
                                if (isset($customCode)) {
                                    $field['customCode'] = $customCode;
                                }

                                $col[] = $field;
                            } else {
                                $emptyCount = $name == '' ? $emptyCount + 1 : $emptyCount;
                                $col[] = $name;
                            }
                        }
                    } //if-else if-else block
                } //if($sugarAttrValue)
            } //foreach

       // One last final check.  If $emptyCount does not equal Array $col count, don't add
            if ($emptyCount != count($col)) {
                $metarow[] = $col;
            } //if
        } //foreach

   $templateMeta = array();
        $templateMeta['form']['buttons'] = 'button';

        preg_match_all("/(<input[^>]*?)>/si", $tables[0], $matches);
        $buttons = array();
        foreach ($matches[0] as $button) {
            $buttons[] = array('customCode'=>$button);
        }
        $templateMeta['form']['buttons'] = $buttons;

        $formElements = $this->getFormElements($contents);
        $hiddenInputs = array();
        foreach ($formElements as $elem) {
            $type = $this->getTagAttribute("type", $elem);
            if (preg_match('/hidden/si', $type, $matches)) {
                $name = $this->getTagAttribute("name", $elem);
                $value = $this->getTagAttribute("value", $elem);
                $index = stripos($value, '$REQUEST');
                $value =  !empty($index) ? '$smarty.request.' . substr($value, 10) : $value;
                $hiddenInputs[] = '<input id="' . $name . '" name="' . $name . '" value="' . $value . '">';
            }
        } //foreach

        $templateMeta['form']['hidden'] = $hiddenInputs;
        $templateMeta['widths'] = array(array('label' => '10', 'field' => '30'),  array('label' => '10', 'field' => '30'));
        $templateMeta['maxColumns'] = 2;

        $panels = array();
        $panels['default'] = $metarow;
        $panels = $this->appplyRules($moduleDir, $panels);
        return $this->createFileContents($moduleDir, $panels, $templateMeta);
    }
}
