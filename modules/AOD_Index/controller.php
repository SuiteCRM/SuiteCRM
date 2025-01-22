<?php
 /**
 *
 *
 * @package
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
 * @author SalesAgility Ltd <support@salesagility.com>
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * @deprecated since v7.12.0
 * Class AOD_IndexController
 */
#[\AllowDynamicProperties]
class AOD_IndexController extends SugarController
{
    protected $action_remap = array('index'=>'indexdata');

    /**
     * @deprecated since v7.12.0
     */
    public function action_indexdata()
    {
        $this->view = 'indexdata';
    }

    /**
     * @deprecated since v7.12.0
     */
    public function action_optimise()
    {
        set_time_limit(6000);
        $index = BeanFactory::getBean("AOD_Index")->getIndex();
        $index->optimise();
        SugarApplication::redirect('index.php?module=AOD_Index');
    }
}
