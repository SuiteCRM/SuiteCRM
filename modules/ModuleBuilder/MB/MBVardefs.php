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
class MBVardefs
{
    public $templates = array();
    public $iTemplates = array();
    public $vardefs = array();
    public $vardef = array();
    public $path = '';
    public $name = '';
    public $errors = array();

    public function __construct($name, $path, $key_name)
    {
        $this->path = $path;
        $this->name = $name;
        $this->key_name = $key_name;
        $this->load();
    }

    public function loadTemplate($by_group, $template, $file)
    {
        $module = $this->name;
        $table_name = $this->name;
        $object_name = $this->key_name;
        $_object_name = strtolower($this->key_name);

        // required by the vardef template for team security in SugarObjects
        $table_name = strtolower($module);

        if (file_exists($file)) {
            include($file);
            if (isset($vardefs)) {
                if ($by_group) {
                    $this->vardefs['fields'] [$template]= $vardefs['fields'];
                } else {
                    $this->vardefs['fields']= array_merge($this->vardefs['fields'], $vardefs['fields']);
                    if (!empty($vardefs['relationships'])) {
                        $this->vardefs['relationships']= array_merge($this->vardefs['relationships'], $vardefs['relationships']);
                    }
                }
            }
        }
        //Bug40450 - Extra 'Name' field in a File type module in module builder
        if (array_key_exists('file', $this->templates)) {
            unset($this->vardefs['fields']['name']);
            unset($this->vardefs['fields']['file']['name']);
        }
    }

    public function mergeVardefs($by_group=false)
    {
        $this->vardefs = array(
                    'fields'=>array(),
                    'relationships'=>array(),
        );
        //		$object_name = $this->key_name;
        //		$_object_name = strtolower($this->name);
        $module_name = $this->name;
        $this->loadTemplate($by_group, 'basic', MB_TEMPLATES . '/basic/vardefs.php');
        foreach ($this->iTemplates as $template=>$val) {
            $file = MB_IMPLEMENTS . '/' . $template . '/vardefs.php';
            $this->loadTemplate($by_group, $template, $file);
        }
        foreach ($this->templates as $template=>$val) {
            if ($template == 'basic') {
                continue;
            }
            $file = MB_TEMPLATES . '/' . $template . '/vardefs.php';
            $this->loadTemplate($by_group, $template, $file);
        }

        if ($by_group) {
            $this->vardefs['fields'][$this->name] = $this->vardef['fields'];
        } else {
            $this->vardefs['fields'] = array_merge($this->vardefs['fields'], $this->vardef['fields']);
        }
    }

    public function updateVardefs($by_group=false)
    {
        $this->mergeVardefs($by_group);
    }


    public function getVardefs()
    {
        return $this->vardefs;
    }

    public function getVardef()
    {
        return $this->vardef;
    }


    public function addFieldVardef($vardef)
    {
        if (!isset($vardef['default']) || strlen((string) $vardef['default']) == 0) {
            unset($vardef['default']);
        }
        $this->vardef['fields'][$vardef['name']] = $vardef;
    }

    public function deleteField($field)
    {
        unset($this->vardef['fields'][$field->name]);
    }

    public function save()
    {
        $header = file_get_contents('modules/ModuleBuilder/MB/header.php');
        write_array_to_file('vardefs', $this->vardef, $this->path . '/vardefs.php', 'w', $header);
    }

    public function build($path)
    {
        $header = file_get_contents('modules/ModuleBuilder/MB/header.php');
        write_array_to_file('dictionary["' . $this->name . '"]', $this->getVardefs(), $path . '/vardefs.php', 'w', $header);
    }
    public function load()
    {
        $this->vardef = array('fields'=>array(), 'relationships'=>array());
        if (file_exists($this->path . '/vardefs.php')) {
            include($this->path. '/vardefs.php');
            $this->vardef = $vardefs;
        }
    }
}
