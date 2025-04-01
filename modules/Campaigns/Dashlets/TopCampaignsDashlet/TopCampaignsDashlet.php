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




require_once('include/Dashlets/Dashlet.php');

#[\AllowDynamicProperties]
class TopCampaignsDashlet extends Dashlet
{
    protected $top_campaigns = array();
    
    /**
     * Constructor
     *
     * @see Dashlet::Dashlet()
     */
    public function __construct($id, $def = null)
    {
        global $current_user, $app_strings;
        parent::__construct($id);
        $this->isConfigurable = true;
        $this->isRefreshable = true;

        if (empty($def['title'])) {
            $this->title = translate('LBL_TOP_CAMPAIGNS', 'Campaigns');
        } else {
            $this->title = $def['title'];
        }
        
        if (isset($def['autoRefresh'])) {
            $this->autoRefresh = $def['autoRefresh'];
        }
        
        $this->seedBean = BeanFactory::newBean('Opportunities');

        $qry = "SELECT C.name AS campaign_name, SUM(O.amount) AS revenue, C.id as campaign_id " .
               "FROM campaigns C, opportunities O " .
               "WHERE C.id = O.campaign_id " .
               "AND O.sales_stage = 'Closed Won' " .
               "AND O.deleted = 0 " .
               "GROUP BY C.name,C.id ORDER BY revenue desc";

        $result = $this->seedBean->db->limitQuery($qry, 0, 10);
        $row = $this->seedBean->db->fetchByAssoc($result);

        while ($row != null) {
            array_push($this->top_campaigns, $row);
            $row = $this->seedBean->db->fetchByAssoc($result);
        }
    }
    
    /**
     * @see Dashlet::display()
     */
    public function display()
    {
        $ss = new Sugar_Smarty();
        $ss->assign('lbl_campaign_name', translate('LBL_TOP_CAMPAIGNS_NAME', 'Campaigns'));
        $ss->assign('lbl_revenue', translate('LBL_TOP_CAMPAIGNS_REVENUE', 'Campaigns'));
        $ss->assign('top_campaigns', $this->top_campaigns);
        
        return parent::display() . $ss->fetch('modules/Campaigns/Dashlets/TopCampaignsDashlet/TopCampaignsDashlet.tpl');
    }
    
    /**
     * @see Dashlet::displayOptions()
     */
    public function displayOptions()
    {
        $ss = new Sugar_Smarty();
        $ss->assign('titleLBL', translate('LBL_DASHLET_OPT_TITLE', 'Home'));
        $ss->assign('title', $this->title);
        $ss->assign('id', $this->id);
        $ss->assign('saveLBL', $GLOBALS['app_strings']['LBL_SAVE_BUTTON_LABEL']);
        if ($this->isAutoRefreshable()) {
            $ss->assign('isRefreshable', true);
            $ss->assign('autoRefresh', $GLOBALS['app_strings']['LBL_DASHLET_CONFIGURE_AUTOREFRESH']);
            $ss->assign('autoRefreshOptions', $this->getAutoRefreshOptions());
            $ss->assign('autoRefreshSelect', $this->autoRefresh);
        }
        
        return $ss->fetch('modules/Opportunities/Dashlets/MyClosedOpportunitiesDashlet/MyClosedOpportunitiesDashletConfigure.tpl');
    }

    /**
     * @see Dashlet::saveOptions()
     */
    public function saveOptions($req)
    {
        $options = array();
        
        if (isset($req['title'])) {
            $options['title'] = $req['title'];
        }
        $options['autoRefresh'] = empty($req['autoRefresh']) ? '0' : $req['autoRefresh'];
        
        return $options;
    }
}
