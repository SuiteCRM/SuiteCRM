<?php
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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class WebToLeadFormBuilder
{

    // ---- html outputs ----

    private static function getFormStartHTML($suiteGrp1Js, $webPostUrl, $webFormHeader, $webFormDescription)
    {
        $formSel = 'form#WebToLeadForm';
        $html = <<<HTML
<style type="text/css">
$formSel, $formSel * {margin: 0; padding: 0; border: none; color: #333; font-size: 12px; line-height: 1.6em; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;}
$formSel {float: left; border: 1px solid #ccc; margin: 10px;}
$formSel h1 {font-size: 32px; font-weight: bold; background-color: rgb(60, 141, 188); color: rgb(247, 247, 247); padding: 10px 20px;}
$formSel h2 {font-size: 24px; font-weight: bold; background-color: rgb(60, 141, 188); color: rgb(247, 247, 247); padding: 10px 20px;}
$formSel h3 {font-size: 12px; font-weight: bold; padding: 10px 20px;}
$formSel h4 {font-size: 10px; font-weight: bold; padding: 10px 20px;}
$formSel h5 {font-size: 8px; font-weight: bold; padding: 10px 20px;}
$formSel h6 {font-size: 6px; font-weight: bold; padding: 10px 20px;}
$formSel p {padding: 10px 20px;}
$formSel input,
$formSel select,
$formSel textarea {border: 1px solid #ccc; display: block; float: left; min-width: 170px; padding: 5px;}
$formSel select {background-color: white;}
$formSel input[type="button"],
$formSel input[type="submit"] {display: inline; float: none; padding: 5px 10px; width: auto; min-width: auto;}
$formSel input[type="checkbox"],
$formSel input[type="radio"] {width: 18px; min-width: auto;}
$formSel div.col {display: block; float: left; width: 330px; padding: 10px 20px;}
$formSel div.clear {display: block; float: none; clear: both; height: 0px; overflow: hidden;}
$formSel div.center {text-align: center;}
$formSel div.buttons {padding: 10px 0; border-top: 1px solid #ccc; background-color: #f7f7f7}
$formSel label {display: block; float: left; width: 160px; font-weight: bold;}
$formSel span.required {color: #FF0000;}
</style>
<!-- TODO ???
<script type="text/javascript" src='$suiteGrp1Js'></script>
-->
<form action='$webPostUrl' name='WebToLeadForm' method='POST' id='WebToLeadForm'>
    <h2>$webFormHeader</h2>
    <p>$webFormDescription</p>
HTML;
        return $html;
    }

    private static function getFormFooterHTML($webFormFooter, $webFormSubmitLabel, $webFormCampaign, $webRedirectURL, $webAssignedUser, $booleanFields, $moduleDir)
    {
        $webFormCampaignInput = $webFormCampaign ? "<input type='hidden' id='campaign_id' name='campaign_id' value='$webFormCampaign'>" : '';
        $webRedirectURLInput = $webRedirectURL ? "<input type='hidden' id='redirect_url' name='redirect_url' value='$webRedirectURL'>" : '';
        $webAssignedUserInput = $webAssignedUser ? "<input type='hidden' id='assigned_user_id' name='assigned_user_id' value='$webAssignedUser'>" : '';
        $booleanFieldsInput = $booleanFields ? "<input type='hidden' id='bool_id' name='bool_id' value='$booleanFields'>" : '';
        $moduleDirInput = "<input type='hidden' id='moduleDir' name='moduleDir' value='$moduleDir'>";


        $html = <<<HTML
$webFormFooter
<div class="row center buttons">
    <input type="submit" onclick="submit_form();" class="button" name="Submit" value="$webFormSubmitLabel" />
    <div class="clear">&nbsp;</div>
</div>
$webFormCampaignInput
$webRedirectURLInput
$webAssignedUserInput
$booleanFieldsInput
$moduleDirInput
HTML;
        return $html;
    }

    private static function getFormFinishHTML($webFormRequiredFieldsMsg)
    {
        $html = <<<HTML
</form>
<script type='text/javascript'>
    function submit_form() {
        if (typeof(validateCaptchaAndSubmit) != 'undefined') {
            validateCaptchaAndSubmit();
        } else {
            check_webtolead_fields();
            //document.WebToLeadForm.submit();
        }
    }

    function check_webtolead_fields() {
        if (document.getElementById('bool_id') != null) {
            var reqs = document.getElementById('bool_id').value;
            bools = reqs.substring(0, reqs.lastIndexOf(';'));
            var bool_fields = new Array();
            var bool_fields = bools.split(';');
            nbr_fields = bool_fields.length;
            for (var i = 0; i < nbr_fields; i++) {
                if (document.getElementById(bool_fields[i]).value == 'on') {
                    document.getElementById(bool_fields[i]).value = 1;
                } else {
                    document.getElementById(bool_fields[i]).value = 0;
                }
            }
        }
    }
</script>
HTML;
        return $html;
    }

    private static function getRowStartHTML()
    {
        return '<div class="row">';
    }

    private static function getRowFinishHTML()
    {
        return '    <div class="clear">&nbsp;</div>
                </div>';
    }

    private static function getColFieldStartHTML()
    {
        return '<div class="col">';
    }

    private static function getColFieldFinishHTML()
    {
        return '</div>';
    }

    // -- fields

    private static function getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol)
    {
        $html = '<label>' . $fieldLabel . ($fieldRequired ? "<span class='required'>$webRequiredSymbol</span>" : '') . '</label>';
        return $html;
    }

    private static function getFieldEmptyHTML()
    {
        $html = '&nbsp;';
        return $html;
    }

    // enums

    private static function getFieldEnumHTML($lead, $fieldName, $appListStringsFieldOptions, $fieldRequired, $fieldLabel, $webRequiredSymbol, $colsField)
    {
        $html = '';

        $leadOptions = get_select_options_with_id($appListStringsFieldOptions, !empty($lead->$fieldName) ? unencodeMultienum($lead->$fieldName) : '');

        $html .= self::getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol);

        if (isset($lead->field_defs[$colsField]['isMultiSelect']) && $lead->field_defs[$colsField]['isMultiSelect'] ==1) {
            $html .= self::getFieldEnumMultiSelectHTML($fieldName, $leadOptions, $fieldRequired);
        } elseif (ifRadioButton($lead->field_defs[$colsField]['name'])) {
            $html .= self::getFieldEnumRadioGroupHTML($appListStringsFieldOptions, $lead, $fieldName, $colsField, $fieldRequired);
        } else {
            $html .= self::getFieldEnumSelectHTML($fieldName, $leadOptions, $fieldRequired);
        }
        return $html;
    }

    private static function getFieldEnumMultiSelectHTML($fieldName, $leadOptions, $fieldRequired)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html = "<select id=\"$fieldName\" multiple=\"true\" name=\"{$fieldName}[]\" tabindex=\"1\"$_required>$leadOptions</select>";
        return $html;
    }

    private static function getFieldEnumRadioGroupHTML($appListStringsFieldOptions, $lead, $fieldName, $colsField, $fieldRequired)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html = '';
        foreach ($appListStringsFieldOptions as $field_option_key => $field_option) {
            if ($field_option != null) {
                if (!empty($lead->$fieldName) && in_array($field_option_key, unencodeMultienum($lead->$fieldName))) {
                    $_checked = ' checked';
                } else {
                    $_checked = '';
                }
                $html .="<input id=\"{$colsField}_$field_option_key\" name=\"$colsField\" value=\"$field_option_key\" type=\"radio\"$_checked$_required>";
                // todo ??? -->
                $html .="<span ='document.getElementById('".$lead->field_defs[$colsField]."_$field_option_key').checked =true style='cursor:default'; onmousedown='return false;'>$field_option</span><br>";
            }
        }
        return $html;
    }

    private static function getFieldEnumSelectHTML($fieldName, $leadOptions, $fieldRequired)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html = "<select id=\"$fieldName\" name=\"$fieldName\" tabindex=\"1\"$_required>$leadOptions</select>";
        return $html;
    }

    // bool

    private static function getFieldBoolHTML($fieldName, $fieldRequired, $fieldLabel, $webRequiredSymbol)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html = self::getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol);
        $html .= "<input type=\"checkbox\" id=\"$fieldName\" name=\"$fieldName\"$_required>";
        return $html;
    }

    // date

    private static function getFieldDateHTML($fieldName, $fieldRequired, $fieldLabel, $webRequiredSymbol)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html = self::getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol);
        $html .= "<input type=\"date\" id=\"{$fieldName}\" name=\"{$fieldName}\"$_required/>";
        return $html;
    }

    // char strings

    /**
     *
     * @param string $fieldName
     * @param string $fieldLabel
     * @param bool $fieldRequired
     * @param string $webRequiredSymbol
     * @return string
     */
    private static function getFieldCharsHTML($fieldName, $fieldLabel, $fieldRequired, $webRequiredSymbol)
    {
        $isRequired = $fieldName == 'last_name' || $fieldRequired;
        $_required = $isRequired ? ' required' : '';
        $html = self::getFieldLabelHTML($fieldLabel, $isRequired, $webRequiredSymbol);
        $_type = $fieldName == 'email1' || $fieldName == 'email2' ? 'email' : 'text';
        $html .= "<input id=\"$fieldName\" name=\"$fieldName\" type=\"$_type\"$_required>";

        if ($_type == 'email') {
            $html .= self::getOptInCheckboxHTML($fieldName);
        }

        return $html;
    }

    // text

    private static function getFieldTextHTML($fieldName, $fieldLabel, $fieldRequired, $webRequiredSymbol)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html  = self::getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol);
        $html .= "<span class='ta_replace'><input id=\"$fieldName\" name=\"$fieldName\" type=\"text\"$_required></span>";
        return $html;
    }

    // relate

    private static function getFieldRelateHTML($fieldName, $fieldLabel, $fieldRequired, $webRequiredSymbol)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html  = self::getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol);
        $html .= "<span><input id=\"$fieldName\" name=\"$fieldName\" type=\"text\"$_required></span>";
        return $html;
    }

    // email

    /**
     *
     * @param string $fieldName
     * @param string $fieldRequired
     * @param string $fieldLabel
     * @param string $webRequiredSymbol
     * @return string
     */
    private static function getFieldEmailHTML($fieldName, $fieldRequired, $fieldLabel, $webRequiredSymbol)
    {
        $_required = $fieldRequired ? ' required' : '';
        $html = self::getFieldLabelHTML($fieldLabel, $fieldRequired, $webRequiredSymbol);
        $html .= "<input id=\"$fieldName\" name=\"$fieldName\" type=\"email\"$_required>";

        $html .= self::getOptInCheckboxHTML($fieldName);

        return $html;
    }

    /**
     *
     * @param string $fieldName
     * @return string
     */
    private static function getOptInCheckboxHTML($fieldName)
    {
        $configurator = new Configurator();
        if (
            !isset($configurator->config['email_enable_confirm_opt_in'])
            || $configurator->config['email_enable_confirm_opt_in'] === SugarEmailAddress::COI_STAT_DISABLED
        ) {
            return '';
        }

        $tpl = new Sugar_Smarty();
        $tpl->assign('fieldName', $fieldName);
        $html = $tpl->fetch('modules/Campaigns/WebToLeadFormBuilderOptInCheckbox.tpl');

        return $html;
    }

    // ----------------

    private static function getFormTwoColumns($request, $formCols)
    {
        $columns = null;
        $colsFirst = isset($request[$formCols[0]]) ? $request[$formCols[0]] : null;
        $colsSecond = isset($request[$formCols[1]]) ? $request[$formCols[1]] : null;

        $countFirst = is_countable($colsFirst) ? count($colsFirst) : 0;
        $countSecond = is_countable($colsSecond) ? count($colsSecond) : 0;

        if (!empty($colsFirst) && !empty($colsSecond)) {
            if ($countFirst < $countSecond) {
                $columns= $countSecond;
            }
            if ($countFirst > $countSecond || $countFirst === $countSecond) {
                $columns= $countFirst;
            }
        } else {
            if (!empty($colsFirst)) {
                $columns= $countFirst;
            } else {
                if (!empty($colsSecond)) {
                    $columns= $countSecond;
                }
            }
        }
        return $columns;
    }

    private static function getBooleanFields($boolFields)
    {
        $boolean_fields='';
        if ($boolFields != null) {
            foreach ($boolFields as $boo) {
                $boolean_fields=$boolean_fields.$boo.';';
            }
        }
        return $boolean_fields;
    }

    private static function getArrayOfFieldInfo($lead, $colsField, &$requiredFields)
    {
        $field_vname= preg_replace('/:$/', '', (string) translate($lead->field_defs[$colsField]['vname'], $lead->module_dir));
        $field_name= $colsField;
        $field_label = $field_vname .": ";
        if (isset($lead->field_defs[$colsField]['custom_type']) && $lead->field_defs[$colsField]['custom_type'] != null) {
            $field_type= $lead->field_defs[$colsField]['custom_type'];
        } else {
            $field_type= $lead->field_defs[$colsField]['type'];
        }

        //bug: 47574 - make sure, that webtolead_email1 field has same required attribute as email1 field
        if ($colsField == 'webtolead_email1' && isset($lead->field_defs['email1']) && isset($lead->field_defs['email1']['required'])) {
            $lead->field_defs['webtolead_email1']['required'] = $lead->field_defs['email1']['required'];
        }

        $field_required = '';
        if (isset($lead->field_defs[$colsField]['required']) && $lead->field_defs[$colsField]['required'] != null
            && $lead->field_defs[$colsField]['required'] != 0) {
            $field_required = $lead->field_defs[$colsField]['required'];
            if (! in_array($lead->field_defs[$colsField]['name'], $requiredFields)) {
                array_push($requiredFields, $lead->field_defs[$colsField]['name']);
            }
        }
        if ($lead->field_defs[$colsField]['name']=='last_name') {
            if (! in_array($lead->field_defs[$colsField]['name'], $requiredFields)) {
                array_push($requiredFields, $lead->field_defs[$colsField]['name']);
            }
        }
        $field_options = null;
        if ($field_type=='multienum' || $field_type=='enum' || $field_type=='radioenum') {
            $field_options= $lead->field_defs[$colsField]['options'];
        }
        return array($field_name, $field_label, $field_type, $field_required, $field_options);
    }

    // --------------- generate form ------------------

    public static function generate(
        $request,
        $lead,
        $moduleDir,
        $siteURL,
        $webPostURL,
        $webFormHeader,
        $webFormDescription,
        $appListStrings,
        $webRequiredSymbol,
        $webFormFooter,
        $webFormSubmitLabel,
        $webFormCampaign,
        $webRedirectURL,
        $webAssignedUser,
        $webFormRequiredFieldsMsg,
        $formCols = array('colsFirst', 'colsSecond')
                                    ) {
        $sugarGrp1Js = getJSPath($siteURL.'/cache/include/javascript/sugar_grp1.js');

        $Web_To_Lead_Form_html = self::getFormStartHTML(
            $sugarGrp1Js,
            $webPostURL,
            $webFormHeader,
            $webFormDescription
        );

        $columns = self::getFormTwoColumns($request, $formCols);


        $required_fields = array();
        $bool_fields = array();
        for ($i= 0; $i<$columns;$i++) {
            $colsFields = array();
            foreach ($formCols as $k => $formCol) {
                $colsFields[$k] = !empty($request[$formCol][$i]) ? $request[$formCol][$i] : null;
            }

            if ($colsFieldCount = is_countable($formCols) ? count($formCols) : 0) {
                $colHtml = '';
                $foundField = false;
                for ($j = 0; $j < $colsFieldCount; $j++) {
                    $colHtml .= self::getColFieldStartHTML();

                    if (isset($lead->field_defs[$colsFields[$j]]) && $lead->field_defs[$colsFields[$j]] != null) {
                        list($field_name, $field_label, $field_type, $field_required, $field_options) = self::getArrayOfFieldInfo($lead, $colsFields[$j], $required_fields);

                        if ($field_type == 'multienum' || $field_type == 'enum' || $field_type == 'radioenum') {
                            $colHtml .= self::getFieldEnumHTML($lead, $field_name, $appListStrings[$field_options], $field_required, $field_label, $webRequiredSymbol, $colsFields[$j]);
                            $foundField = true;
                        } elseif ($field_type == 'bool') {
                            $colHtml .= self::getFieldBoolHTML($field_name, $field_required, $field_label, $webRequiredSymbol);
                            $foundField = true;
                            if (!in_array($lead->field_defs[$colsFields[$j]]['name'], $bool_fields)) {
                                array_push($bool_fields, $lead->field_defs[$colsFields[$j]]['name']);
                            }
                        } elseif ($field_type == 'date') {
                            $colHtml .= self::getFieldDateHTML($field_name, $field_required, $field_label, $webRequiredSymbol);
                            $foundField = true;
                        } elseif ($field_type == 'varchar' || $field_type == 'name' || $field_type == 'phone' || $field_type == 'currency' || $field_type == 'url' || $field_type == 'int') {
                            $colHtml .= self::getFieldCharsHTML($field_name, $field_label, $field_required, $webRequiredSymbol);
                            $foundField = true;
                        } elseif ($field_type == 'text') {
                            $colHtml .= self::getFieldTextHTML($field_name, $field_label, $field_required && false, $webRequiredSymbol);
                            $foundField = true;
                        } elseif ($field_type == 'relate' && $field_name == 'account_name') {
                            $colHtml .= self::getFieldRelateHTML($field_name, $field_label, $field_required && false, $webRequiredSymbol);
                            $foundField = true;
                        } elseif ($field_type == 'email') {
                            $colHtml .= self::getFieldEmailHTML();
                            $foundField = true;
                        } else {
                            $colHtml .= self::getFieldEmptyHTML();
                        }
                    } else {
                        $colHtml .= self::getFieldEmptyHTML();
                    }

                    $colHtml .= self::getColFieldFinishHTML();
                }

                if ($foundField) {
                    $Web_To_Lead_Form_html .= self::getRowStartHTML();
                    $Web_To_Lead_Form_html .= $colHtml;
                    $Web_To_Lead_Form_html .= self:: getRowFinishHTML();
                }
            }
        }


        $booleanFields = self::getBooleanFields(isset($bool_fields) ? $bool_fields : null);

        $Web_To_Lead_Form_html .= self::getFormFooterHTML(
            $webFormFooter,
            $webFormSubmitLabel,
            $webFormCampaign,
            $webRedirectURL,
            $webAssignedUser,
            $booleanFields,
            $moduleDir
        );

        $Web_To_Lead_Form_html .= self::getFormFinishHTML($webFormRequiredFieldsMsg);

        return $Web_To_Lead_Form_html;
    }
}
