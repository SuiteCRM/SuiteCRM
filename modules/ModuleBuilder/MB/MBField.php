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

#[\AllowDynamicProperties]
class MBField
{
    public $type = 'varchar';
    public $name = false;
    public $label = false;
    public $vname = false;
    public $options = false;
    public $length = false;
    public $error = '';
    public $required = false;
    public $reportable = true;
    public $default = 'MSI1';
    public $comment = '';
    
    
    
    public function getFieldVardef()
    {
        if (empty($this->name)) {
            $this->error = 'A name is required to create a field';
            return false;
        }
        if (empty($this->label)) {
            $this->label = $this->name;
        }
        $this->name = strtolower($this->getDBName($this->name));
        $vardef = array();
        $vardef['name']=$this->name;
        if (empty($this->vname)) {
            $this->vname = 'LBL_' . strtoupper($this->name);
        }
        $vardef['vname'] = $this->addLabel();
        if (!empty($this->required)) {
            $vardef['required'] = $this->required;
        }
        if (empty($this->reportable)) {
            $vardef['reportable'] = false;
        }
        if (!empty($this->comment)) {
            $vardef['comment'] = $this->comment;
        }
        if ($this->default !== 'MSI1') {
            $vardef['default'] = $this->default;
        }
        switch ($this->type) {
            case 'date':
            case 'datetime':
            case 'float':
            case 'int':
                $vardef['type']=$this->type;
                return $vardef;
            case 'bool':
                $vardef['type'] = 'bool';
                $vardef['default'] = (empty($vardef['default']))?0:1;
                return $vardef;
            case 'enum':
                $vardef['type']='enum';
                if (empty($this->options)) {
                    $this->options = $this->name . '_list';
                }
                $vardef['options'] = $this->addDropdown();
                return $vardef;
            default:
                $vardef['type']='varchar';
                return $vardef;
            
        }
    }
    
    public function addDropDown()
    {
        return $this->options;
    }
    
    public function addLabel()
    {
        return $this->vname;
    }
}
