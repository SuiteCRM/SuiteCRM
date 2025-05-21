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

require_once('include/ListView/ListViewSmarty.php');
require_once('modules/Emails/include/ListView/ListViewDataEmails.php');

#[\AllowDynamicProperties]
class ListViewSmartyEmails extends ListViewSmarty
{
    /**
     * Constructor
     */
    public function __construct()
    {
        parent::__construct();
        $this->ss = new Sugar_Smarty();
        $this->lvd = new ListViewDataEmails();
        $this->searchColumns = array();
    }

    /**
     * @inheritdoc
     * @param SugarBean $seed
     * @param File $file
     * @param string $where
     * @param array $params
     * @param int $offset
     * @param int $limit
     * @param array $filter_fields
     * @param string $id_field
     * @param null $id
     * @return bool
     *
     */
    public function setup(
        $seed,
        $file,
        $where,
        $params = array(),
        $offset = 0,
        $limit = -1,
        $filter_fields = array(),
        $id_field = 'id',
        $id = null
    ) {
        $this->should_process = true;
        if (isset($seed->module_dir) && !$this->shouldProcess($seed->module_dir)) {
            return false;
        }
        if (isset($params['export'])) {
            $this->export = $params['export'];
        }
        if (!empty($params['multiSelectPopup'])) {
            $this->multi_select_popup = $params['multiSelectPopup'];
        }
        if (!empty($params['massupdate']) && $params['massupdate'] != false) {
            $this->show_mass_update_form = true;
            $this->mass = $this->getMassUpdate();
            $this->mass->setSugarBean($seed);
            if (!empty($params['handleMassupdate']) || !isset($params['handleMassupdate'])) {
                $this->mass->handleMassUpdate();
            }
        }

        $this->seed = $seed;

        $filter_fields = $this->setupFilterFields($filter_fields);

        $data = $this->lvd->getListViewData(
            $seed,
            $where,
            $offset,
            $limit,
            $filter_fields,
            $params,
            $id_field,
            true,
            $id
        );

        $this->fillDisplayColumnsWithVardefs();

        $this->process($file, $data, $seed->object_name);

        return true;
    }
}
