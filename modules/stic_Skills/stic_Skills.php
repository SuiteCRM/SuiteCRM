<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

#[\AllowDynamicProperties]
class stic_Skills extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Skills';
    public $object_name = 'stic_Skills';
    public $table_name = 'stic_skills';
    public $importable = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $SecurityGroups;
    public $skill;
    public $type;
    public $level;
    public $language;
    public $other;
    public $written;
    public $oral;
    public $certificate;
    public $certificate_date;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }
    /**
     * Override the bean's save function to assign an auto-incrementing value to the code field when a new record is created
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = false)
    {
        global $app_list_strings;

        $this->fillName();

        if ($this->type == 'language') {
            $this->skill = $app_list_strings['stic_skills_types_list'][$this->type];
        }
        // Save the bean
        parent::save($check_notify);

    }

    protected function fillName()
    {
        // Auto name
        if (empty($this->name)) {
            global $app_list_strings;
            include_once 'SticInclude/Utils.php';

            $contactName = '';
            $contactBean = BeanFactory::getBean('Contacts', $this->stic_skills_contactscontacts_ida);
            if ($contactBean) {
                $contactName = $contactBean->first_name . ' ' . $contactBean->last_name;
            }

            if ($this->type === 'language') {
                $this->name = $contactName . ' - ' .
                    $app_list_strings['stic_skills_types_list'][$this->type] . ' - ';
                if ($this->language === 'other') {
                    $this->name .= $this->other;
                } else {
                    $this->name .= $app_list_strings['stic_skills_languages_list'][$this->language];
                }
            } else {
                $this->name = $contactName . ' - ' .
                $app_list_strings['stic_skills_types_list'][$this->type] . ' - ' .
                $this->skill;
            }

        }
    }
}
