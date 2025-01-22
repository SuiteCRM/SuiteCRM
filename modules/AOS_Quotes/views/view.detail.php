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


/**
 * Class AOS_QuotesViewDetail
 */
#[\AllowDynamicProperties]
class AOS_QuotesViewDetail extends ViewDetail
{
    /**
     * @var AOS_Quotes $bean;
     */
    public $bean;

    /**
     * AOS_QuotesViewDetail constructor.
     */
    public function __construct()
    {
        parent::__construct();
    }

    public function display()
    {
        $this->populateQuoteTemplates();
        $this->displayPopupHtml();
        parent::display();
    }

    protected function populateQuoteTemplates()
    {
        global $app_list_strings;

        $sql = "SELECT id, name FROM aos_pdf_templates WHERE deleted=0 AND type='AOS_Quotes' AND active = 1";

        $res = $this->bean->db->query($sql);

        $app_list_strings['template_ddown_c_list'] = array();
        while ($row = $this->bean->db->fetchByAssoc($res)) {
            if ($row['id']) {
                $app_list_strings['template_ddown_c_list'][$row['id']] = $row['name'];
            }
        }
    }

    protected function displayPopupHtml()
    {
        global $app_list_strings, $app_strings, $mod_strings;
        $templatesList = array_keys($app_list_strings['template_ddown_c_list']);
        $template = new Sugar_Smarty();
        $template->assign('APP_LIST_STRINGS', $app_list_strings);
        $template->assign('APP', $app_strings);
        $template->assign('MOD', $mod_strings);
        $template->assign('FOCUS', $this->bean);
        $template->assign('TEMPLATES', $templatesList);

        if ($templatesList) {
            $template->assign('TOTAL_TEMPLATES', count($templatesList));
            foreach ($templatesList as $t => $templatesListItem) {
                $templatesList[$t] = str_replace('^', '', $templatesListItem);
            }
            echo $template->fetch('modules/AOS_Quotes/templates/showPopupWithTemplates.tpl');
        } else {
            echo $template->fetch('modules/AOS_Quotes/templates/showPopupWithOutTemplates.tpl');
        }
    }
}
