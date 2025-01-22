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

/**
 * Class CaseEventsHook.
 */
#[\AllowDynamicProperties]
class CaseEventsHook
{
    private static $diffFields = array(
        array('field' => 'priority', 'display_field' => 'priority', 'display_name' => 'Priority'),
        array('field' => 'status', 'display_field' => 'status', 'display_name' => 'Status'),
        array(
            'field'         => 'assigned_user_id',
            'display_field' => 'assigned_user_name',
            'display_name'  => 'Assigned User'
        ),
        array('field' => 'type', 'display_field' => 'type', 'display_name' => 'Type'),
    );

    /**
     * @param SugarBean $old
     * @param SugarBean $new
     *
     * @return array
     */
    private function compareBeans($old, $new)
    {
        $events = array();
        foreach (self::$diffFields as $field) {
            $fieldName = $field['field'];
            $displayField = $field['display_field'];
            $name = $field['display_name'];
            if ((isset($old->$fieldName) ? $old->$fieldName : null) !==
                (isset($new->$fieldName) ? $new->$fieldName : null)
            ) {
                $event = BeanFactory::newBean('AOP_Case_Events');
                $oldDisplay = $old->$displayField ?? '';
                $newDisplay = $new->$displayField ?? '';
                $desc = $name . ' changed from ' . $oldDisplay . ' to ' . $newDisplay . '.';
                $event->name = $desc;
                $event->description = $desc;
                $event->case_id = $new->id;
                $events[] = $event;
            }
        }

        return $events;
    }

    /**
     * @param SugarBean $bean
     */
    public function saveUpdate($bean)
    {
        if (!$bean->id) {
            //New case so do nothing.
            return;
        }
        if (isset($_REQUEST['module']) && $_REQUEST['module'] === 'Import') {
            return;
        }
        $oldBean = BeanFactory::newBean('Cases');
        $oldBean->retrieve($bean->id);
        $events = $this->compareBeans($oldBean, $bean);
        foreach ($events as $event) {
            $event->save();
        }
    }
}
