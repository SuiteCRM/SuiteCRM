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

require_once('include/DetailView/DetailView2.php');

#[\AllowDynamicProperties]
class EmailsDetailView extends DetailView2
{
    /**
     * @var Email $focus
     */
    public $focus;

    /**
     * @var string $formName
     */
    public $formName = 'EmailsDetailView';

    /**
     * @param String $module
     * @param null $focus
     * @param null $metadataFile
     * @param string $tpl
     * @param bool $createFocus
     * @param string $metadataFileName
     */
    public function setup(
        $module,
        $focus  = null,
        $metadataFile = null,
        $tpl = 'include/DetailView/DetailView.tpl',
        $createFocus = true,
        $metadataFileName = 'detail'
    ) {
        parent::setup($module, $focus, $metadataFile, $tpl, $createFocus, $metadataFileName);
    }

    /**
     * @inheritdoc
     * @see DetailView2::populateBean()
     */
    public function populateBean($request = array())
    {
        parent::populateBean($request);
        $this->populateFields();
    }

    /**
     * Handles fields special fields like from
     * @return void
     */
    public function populateFields()
    {
        if (empty($this->focus)) {
            return;
        }

        if (empty($this->focus->from_addr_name)) {
            $this->focus->from_addr_name = $this->focus->from_addr;
        }
        //Prevent Email address to be handled as HTML tag
        $this->focus->from_addr_name = html_entity_decode($this->focus->from_addr_name);
    }

    /**
     * @param bool $showTitle boolean value indicating whether or not to show a title on the resulting page
     * @param bool $ajaxSave boolean value indicating whether or not the operation is an Ajax save request
     * @return string display for view as HTML
     */
    public function display($showTitle = true, $ajaxSave = false)
    {
        return parent::display($showTitle, $ajaxSave);
    }
}
