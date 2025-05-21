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

/**
 * Generic formatter
 * @api
 */
#[\AllowDynamicProperties]
class default_formatter
{
    protected $_ss;
    protected $_component;
    protected $_tplFileName;
    protected $_module;
    protected $_hoverField;

    public function __construct()
    {
    }

    public function getDetailViewFormat()
    {
        $source = $this->_component->getSource();
        $class = get_class($source);
        $dir = str_replace('_', '/', $class);
        $config = $source->getConfig();
        $this->_ss->assign('config', $config);
        $this->_ss->assign('source', $class);
        $this->_ss->assign('module', $this->_module);
        $mapping = $source->getMapping();
        $mapping = !empty($mapping['beans'][$this->_module]) ? implode(',', array_values($mapping['beans'][$this->_module])) : '';
        $this->_ss->assign('mapping', $mapping);

        if (file_exists("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl")) {
            return $this->_ss->fetch("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl");
        } else {
            if (file_exists("modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl")) {
                return $this->_ss->fetch("modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl");
            } else {
                if (file_exists("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl")) {
                    return $this->_ss->fetch("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl");
                } else {
                    if (file_exists("modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl")) {
                        return $this->_ss->fetch("modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl");
                    } else {
                        if (preg_match('/_soap_/', $class)) {
                            return $this->_ss->fetch("include/connectors/formatters/ext/soap/tpls/default.tpl");
                        } else {
                            return $this->_ss->fetch("include/connectors/formatters/ext/rest/tpls/default.tpl");
                        }
                    }
                }
            }
        }
    }

    public function getEditViewFormat()
    {
        return '';
    }

    public function getListViewFormat()
    {
        return '';
    }

    public function getSearchFormFormat()
    {
        return '';
    }

    protected function fetchSmarty()
    {
        $source = $this->_component->getSource();
        $class = get_class($source);
        $dir = str_replace('_', '/', $class);
        $config = $source->getConfig();
        $this->_ss->assign('config', $config);
        $this->_ss->assign('source', $class);
        $this->_ss->assign('module', $this->_module);
        if (file_exists("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl")) {
            return $this->_ss->fetch("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl");
        } else {
            if (file_exists("modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl")) {
                return $this->_ss->fetch("modules/Connectors/connectors/formatters/{$dir}/tpls/{$this->_module}.tpl");
            } else {
                if (file_exists("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl")) {
                    return $this->_ss->fetch("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl");
                } else {
                    return $this->_ss->fetch("modules/Connectors/connectors/formatters/{$dir}/tpls/default.tpl");
                }
            }
        }
    }

    public function getSourceMapping()
    {
        $source = $this->_component->getSource();
        $mapping = $source->getMapping();
        return $mapping;
    }

    public function setSmarty($smarty)
    {
        $this->_ss = $smarty;
    }

    public function getSmarty()
    {
        return $this->_ss;
    }

    public function setComponent($component)
    {
        $this->_component = $component;
    }

    public function getComponent()
    {
        return $this->_component;
    }

    public function getTplFileName()
    {
        return $this->tplFileName;
    }

    public function setTplFileName($tplFileName)
    {
        $this->tplFileName = $tplFileName;
    }

    public function setModule($module)
    {
        $this->_module = $module;
    }

    public function getModule()
    {
        return $this->_module;
    }

    public function getIconFilePath()
    {
        return '';
    }
}
