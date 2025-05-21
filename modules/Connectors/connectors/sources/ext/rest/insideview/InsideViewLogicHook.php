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


#[\AllowDynamicProperties]
class InsideViewLogicHook
{
    public const URL_BASE = 'https://my.insideview.com/iv/crm/';

    protected function handleFieldMap($bean, $mapping)
    {
        $outArray = array();
        foreach ($mapping as $dest => $src) {
            // Initialize it to an empty string, so it is always set
            $outArray[$dest] = '';

            if (is_array($src)) {
                // The source can be any one of a number of fields
                // we must go deeper.
                foreach ($src as $src2) {
                    if (isset($bean->$src2)) {
                        $outArray[$dest] = $bean->$src2;
                        break;
                    }
                }
            } else {
                if (isset($bean->$src)) {
                    $outArray[$dest] = $bean->$src;
                }
            }
        }

        $outStr = '';
        foreach ($outArray as $k => $v) {
            $outStr .= $k.'='.rawurlencode(html_entity_decode((string) $v, ENT_QUOTES)).'&';
        }
        
        $outStr = rtrim($outStr, '&');
        
        return $outStr;
    }

    protected function getAccountFrameUrl($bean, $extraUrl)
    {
        $url = self::URL_BASE.'analyseAccount.do?crm_context=account&';
        $fieldMap = array('crm_account_name'=>'name',
                          'crm_account_id'=>'id',
                          'crm_account_website'=>'website',
                          'crm_account_ticker'=>'ticker_symbol',
                          'crm_account_city'=>array('primary_address_city', 'secondary_address_city', 'billing_address_city', 'shipping_address_city'),
                          'crm_account_state'=>array('primary_address_state', 'secondary_address_state', 'billing_address_state', 'shipping_address_state'),
                          'crm_account_country'=>array('primary_address_country', 'secondary_address_country', 'billing_address_country', 'shipping_address_country'),
                          'crm_account_postalcode'=>array('primary_address_postalcode', 'secondary_address_postalcode', 'billing_address_postalcode', 'shipping_address_postalcode')
        );
        
        $url .= $this->handleFieldMap($bean, $fieldMap).'&'.$extraUrl;
        
        return $url;
    }

    protected function getOpportunityFrameUrl($bean, $extraUrl)
    {
        $url = self::URL_BASE.'analyseAccount.do?crm_context=opportunity&';
        $fieldMap = array('crm_account_name'=>'account_name',
                          'crm_account_id'=>'account_id',
                          'crm_opportunity_id'=>'id',
        );
        
        $url .= $this->handleFieldMap($bean, $fieldMap).'&'.$extraUrl;
        
        return $url;
    }
    protected function getLeadFrameUrl($bean, $extraUrl)
    {
        $url = self::URL_BASE.'analyseAccount.do?crm_context=lead&';
        $fieldMap = array('crm_lead_id'=>'id',
                          'crm_lead_firstname'=>'first_name',
                          'crm_lead_lastname'=>'last_name',
                          'crm_lead_title'=>'title',
                          'crm_account_id'=>'id',
                          'crm_account_name'=>'account_name',
                          'crm_account_website'=>'website',
        );
        
        $url .= $this->handleFieldMap($bean, $fieldMap).'&'.$extraUrl;
        
        return $url;
    }
    protected function getContactFrameUrl($bean, $extraUrl)
    {
        $url = self::URL_BASE.'analyseExecutive.do?crm_context=contact&';
        $fieldMap = array('crm_object_id'=>'id',
                          'crm_fn'=>'first_name',
                          'crm_ln'=>'last_name',
                          'crm_email'=>'email',
                          'crm_account_id'=>'account_id',
                          'crm_account_name'=>'account_name',
        );
        
        $url .= $this->handleFieldMap($bean, $fieldMap).'&'.$extraUrl;
        
        return $url;
    }


    public function showFrame($event, $args)
    {
        if ($GLOBALS['app']->controller->action != 'DetailView') {
            return;
        }
        require_once('include/connectors/utils/ConnectorUtils.php');

        $bean = $GLOBALS['app']->controller->bean;

        // Build the base arguments
        static $userFieldMap = array('crm_user_id' => 'id',
                                     'crm_user_fn' => 'first_name',
                                     'crm_user_ln' => 'last_name',
                                     'crm_user_email' => 'email1',
        );


        if ($GLOBALS['current_user']->id != '1') {
            $extraUrl = $this->handleFieldMap($GLOBALS['current_user'], $userFieldMap);
        } else {
            // Need some extra code here for the '1' admin user
            $myUserFieldMap = $userFieldMap;
            unset($myUserFieldMap['crm_user_id']);
            $extraUrl = 'crm_user_id='.urlencode($GLOBALS['sugar_config']['unique_key']).'&'.$this->handleFieldMap($GLOBALS['current_user'], $myUserFieldMap);
        }
        $extraUrl .= '&crm_org_id='.urlencode($GLOBALS['sugar_config']['unique_key'])
            .'&crm_org_name='.(!empty($GLOBALS['system_config']->settings['system_name']) ? urlencode($GLOBALS['system_config']->settings['system_name']) : '')
            .'&crm_server_url='.urlencode($GLOBALS['sugar_config']['site_url'])
            .'&crm_session_id=&crm_version=v62&crm_deploy_id=3&crm_size=400&is_embed_version=true';
        
        // Use the per-module functions to build the frame
        if (is_a($bean, 'Account')) {
            $url = $this->getAccountFrameUrl($bean, $extraUrl);
        } else {
            if (is_a($bean, 'Contact')) {
                $url = $this->getContactFrameUrl($bean, $extraUrl);
            } else {
                if (is_a($bean, 'Lead')) {
                    $url = $this->getLeadFrameUrl($bean, $extraUrl);
                } else {
                    if (is_a($bean, 'Opportunity')) {
                        $url = $this->getOpportunityFrameUrl($bean, $extraUrl);
                    } else {
                        $url = '';
                    }
                }
            }
        }

        if ($url != '') {
            // Check if the user should be shown the frame or not
            $smarty = new Sugar_Smarty();
            $tplName = 'modules/Connectors/connectors/sources/ext/rest/insideview/tpls/InsideView.tpl';
            require_once('include/connectors/utils/ConnectorUtils.php');
            $connector_language = ConnectorUtils::getConnectorStrings('ext_rest_insideview');
            $smarty->assign('connector_language', $connector_language);
            $smarty->assign('logo', getWebPath('modules/Connectors/connectors/sources/ext/rest/insideview/images/insideview.png'));
            $smarty->assign('video', getWebPath('modules/Connectors/connectors/sources/ext/rest/insideview/images/video.png'));

            $smarty->assign('close', getWebPath('modules/Connectors/connectors/sources/ext/rest/insideview/images/close.png'));
            $smarty->assign('logo_expanded', getWebPath('modules/Connectors/connectors/sources/ext/rest/insideview/images/insideview_expanded.png'));
            $smarty->assign('logo_collapsed', getWebPath('modules/Connectors/connectors/sources/ext/rest/insideview/images/insideview_collapsed.png'));

            $smarty->assign('AJAX_URL', $url);
            $smarty->assign('APP', $GLOBALS['app_strings']);

            if ($GLOBALS['current_user']->getPreference('allowInsideView', 'Connectors') != 1) {
                $smarty->assign('showInsideView', false);
            } else {
                $smarty->assign('showInsideView', true);
                $smarty->assign('URL', $url);
                //echo "<div id='insideViewDiv' style='width:100%;height:400px;overflow:hidden'><iframe id='insideViewFrame' src='$url' style='border:0px; width:100%;height:480px;overflow:hidden'></iframe></div>";
            }
            echo $smarty->fetch($tplName);
        }
    }
}
