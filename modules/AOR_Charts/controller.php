<?php
/**
 * Advanced OpenReports, SugarCRM Reporting.
 * @package Advanced OpenReports for SugarCRM
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility <info@salesagility.com>
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
