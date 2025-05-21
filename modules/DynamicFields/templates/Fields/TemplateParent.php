<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

require_once('modules/DynamicFields/templates/Fields/TemplateEnum.php');

require_once('modules/DynamicFields/templates/Fields/TemplateId.php');
require_once('modules/DynamicFields/templates/Fields/TemplateParentType.php');
class TemplateParent extends TemplateEnum
{
    public $max_size = 25;
    public $type='parent';

    public function get_field_def()
    {
        $def = parent::get_field_def();
        $def['type_name'] = 'parent_type';
        $def['id_name'] = 'parent_id';
        $def['parent_type'] = 'record_type_display';
        $def['source'] = 'non-db';
        $def['studio'] = 'visible';
        $def['resetFieldInStudio'] = 'true';
        return $def;
    }

    public function delete($df)
    {
        parent::delete($df);
        //currency id
        $parent_type = new TemplateText();
        $parent_type->name = 'parent_type';
        $parent_type->delete($df);

        $parent_id = new TemplateId();
        $parent_id->name = 'parent_id';
        $parent_id->delete($df);
    }

    public function save($df)
    {
        $this->ext1 = 'parent_type_display';
        $this->name = 'parent_name';
        $this->default_value = '';
        parent::save($df); // always save because we may have updates

        //save parent_type
        $parent_type = new TemplateParentType();
        $parent_type->name = 'parent_type';
        $parent_type->vname = 'LBL_PARENT_TYPE';
        $parent_type->label = $parent_type->vname;
        $parent_type->len = 255;
        $parent_type->importable = $this->importable;
        $parent_type->save($df);

        //save parent_name
        $parent_id = new TemplateId();
        $parent_id->name = 'parent_id';
        $parent_id->vname = 'LBL_PARENT_ID';
        $parent_id->label = $parent_id->vname;
        $parent_id->len = 36;
        $parent_id->importable = $this->importable;
        $parent_id->save($df);
    }

    public function get_db_add_alter_table($table)
    {
        return '';
    }
    /**
     * mysql requires the datatype caluse in the alter statment.it will be no-op anyway.
     */
    public function get_db_modify_alter_table($table)
    {
        return '';
    }
}
