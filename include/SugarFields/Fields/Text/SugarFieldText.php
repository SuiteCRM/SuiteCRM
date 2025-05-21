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

require_once('include/SugarFields/Fields/Base/SugarFieldBase.php');
require_once 'include/clean.php';
class SugarFieldText extends SugarFieldBase
{
    public function getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (!isset($displayParams['nl2br'])) {
            $displayParams['nl2br'] = true;
        }

        $editor = $vardef['editor'] ?? '';

        if (!isset($displayParams['htmlescape']) && $editor != "html") {
            $displayParams['htmlescape'] = true;
        }

        if (!isset($displayParams['url2html'])) {
            $displayParams['url2html'] = true;
        }

        return parent::getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
    }

    public function getClassicEditView($field_id='description', $value='', $prefix='', $rich_text=false, $maxlength='', $tabindex=1, $cols=80, $rows=4)
    {
        $this->ss->assign('prefix', $prefix);
        $this->ss->assign('field_id', $field_id);
        $this->ss->assign('value', $value);
        $this->ss->assign('tabindex', $tabindex);

        $displayParams = array();
        $displayParams['textonly'] = $rich_text ? false : true;
        $displayParams['maxlength'] = $maxlength;
        $displayParams['rows'] = $rows;
        $displayParams['cols'] = $cols;

        $this->ss->assign('displayParams', $displayParams);

        if (isset($GLOBALS['current_user'])) {
            $height = $GLOBALS['current_user']->getPreference('text_editor_height');
            $width = $GLOBALS['current_user']->getPreference('text_editor_width');
            $height = isset($height) ? $height : '300px';
            $width = isset($width) ? $width : '95%';
            $this->ss->assign('RICH_TEXT_EDITOR_HEIGHT', $height);
            $this->ss->assign('RICH_TEXT_EDITOR_WIDTH', $width);
        } else {
            $this->ss->assign('RICH_TEXT_EDITOR_HEIGHT', '100px');
            $this->ss->assign('RICH_TEXT_EDITOR_WIDTH', '95%');
        }

        return $this->ss->fetch($this->findTemplate('ClassicEditView'));
    }

    public function setup($parentFieldArray, $vardef, $displayParams, $tabindex, $twopass = true)
    {
        parent::setup($parentFieldArray, $vardef, $displayParams, $tabindex, $twopass);
        $initiate = "";

        if (isset($vardef['editor']) && $vardef['editor'] === "html") {
            if (!isset($displayParams['htmlescape'])) {
                $displayParams['htmlescape'] = false;
            }

            if ($_REQUEST['action'] === "EditView") {
                $form_name = $displayParams['formName'] ?? '';

                if (!empty($this->ss->_tpl_vars['displayParams']['formName'])) {
                    $form_name = $this->ss->_tpl_vars['displayParams']['formName'];
                }

                $config = [];
                $config['height'] = 250;
                $config['menubar'] = false;
                $config['plugins'] = 'code, table, link, image, wordcount';

                if ($form_name !== '') {
                    $config['selector'] = "#{$form_name} " . "#" . $vardef['name'];
                } else {
                    $config['selector'] = "#" . $vardef['name'];
                }

                $config['toolbar1'] = 'fontselect | fontsizeselect | bold italic underline | forecolor backcolor | styleselect | outdent indent | link image | code table';

                $jsConfig = json_encode($config);
                $initiate = '<script type="text/javascript"> tinyMCE.init(' . $jsConfig . ')</script>';
            }
        }

        $this->ss->assign("tinymce", $initiate);
    }
}
