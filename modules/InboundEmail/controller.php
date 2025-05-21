<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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
class InboundEmailController extends SugarController
{
    public function action_EditView() {
        $this->view = 'edit';
        if (!empty($this->bean) && !empty($_REQUEST['is_personal'])) {
            $this->bean->is_personal = isTrue($_REQUEST['is_personal'] ?? false);
        }

        if (empty($_REQUEST['record']) && isTrue($_REQUEST['is_personal'] ?? false)) {
            $this->hasAccess = true;
            return;
        }

        if (!empty($this->bean) && isTrue($this->bean->is_personal) && $this->bean->hasAccessToPersonalAccount()) {
            $this->hasAccess = true;
        }
    }

    public function action_SetDefault()
    {
        global $current_user;
        $outbound_id = empty($_REQUEST['record']) ? "" : $_REQUEST['record'];
        $ie = BeanFactory::newBean('InboundEmail');

        $ownerId = $this->bean->created_by ?? '';
        if (empty($ownerId)) {
            $ownerId = $current_user->id;
        }

        $owner = BeanFactory::getBean('Users', $ownerId);

        if($ownerId === $current_user->id || is_admin($current_user)){
            $ie->setUsersDefaultOutboundServerId($owner, $outbound_id);
        }

        $module = (!empty($this->return_module) ? $this->return_module : $this->module);
        $action = (!empty($this->return_action) ? $this->return_action : 'DetailView');
        $id = (!empty($this->return_id) ? $this->return_id : $outbound_id);

        $url = "index.php?module=" . $module . "&action=" . $action . "&record=" . $id;
        $this->set_redirect($url);
    }
}
