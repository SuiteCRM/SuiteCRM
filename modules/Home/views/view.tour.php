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

require_once('include/MVC/View/SugarView.php');

#[\AllowDynamicProperties]
class HomeViewTour extends SugarView
{
    public function display()
    {
        global $sugar_flavor;
        global $current_user;
        $mod_strings = return_module_language($GLOBALS['current_language'], 'Home');
        $this->ss->assign('mod', $mod_strings);
        $this->ss->assign("sugarFlavor", $sugar_flavor);

        //check the upgrade history to see if this instance has been upgraded, if so then present the calendar url message
        //if no upgrade history exists then we can assume this is an install and we do not show the calendar message
        $uh = new UpgradeHistory();
        $upgrade = (is_countable($uh->getAll()) ? count($uh->getAll()) : 0)>0 ? true : false;
        if ($upgrade) {
            //create the url with the user id and scrolltocal flag.  This will be passed into language string
            $urlForString = $mod_strings['LBL_TOUR_CALENDAR_URL_1'];
            $urlForString .= '<br><a href="index.php?module=Users&action=EditView&record='.$current_user->id.'&scrollToCal=true" target="_blank">';
            $urlForString .= $mod_strings['LBL_TOUR_CALENDAR_URL_2'].'</a>';
            $this->ss->assign('view_calendar_url', $urlForString);
        }
        $this->ss->display('modules/Home/tour.tpl');
    }
}
