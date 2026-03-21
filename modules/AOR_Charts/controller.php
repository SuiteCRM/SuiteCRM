<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */


#[\AllowDynamicProperties]
class AOR_ChartsController extends SugarController
{
    protected function action_getImageMap()
    {
        ob_start();
        global $current_user;
        if (!isset($_REQUEST['imageMapId'])) {
            return;
        }
        require_once 'modules/AOR_Charts/lib/pChart/pChart.php';
        $img = new pImage(100, 100);
        $imageMapDir = create_cache_directory('modules/AOR_Charts/ImageMap/'.$current_user->id.'/');
        $id = $current_user->id."-".(int)$_REQUEST['imageMapId'];
        ob_clean();
        $img->dumpImageMap($id, IMAGE_MAP_STORAGE_FILE, $id, $imageMapDir);
    }
}
