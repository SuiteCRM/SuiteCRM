<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
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
 * @copyright Andrew Mclaughlan 2014
 * @author Andrew Mclaughlan <andrew@mclaughlan.info>
 */

/**
 * delete_project_tasks.php
 * Used to delete a project's related tasks after a project is deleted
 */
#[\AllowDynamicProperties]
class delete_project_tasks
{
    public function delete_tasks(&$bean, $event, $arguments)
    {
        $db = DBManagerFactory::getInstance();
        $Task = BeanFactory::getBean('ProjectTask');
        $tasks = $Task->get_full_list("order_number", "project_task.project_id = '".$bean->id."'");

        foreach ($tasks as $task) {
            $query = "UPDATE project_task SET deleted ='1' WHERE id ='".$task->id."'";
            $update = $db->query($query);
        }
    }
}
