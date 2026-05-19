<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

require_once('include/SugarFields/Fields/Text/SugarFieldText.php');
class CustomSugarFieldText extends SugarFieldText
{
    /**
     * Fixing DetailView for Text fields with HTML editor, to avoid double escaping and double nl2br when the field is rendered in DetailView.
     * https://github.com/SinergiaTIC/SinergiaCRM/pull/1113
     */
    public function getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        $editor = $vardef['editor'] ?? '';

        if (!isset($displayParams['nl2br'])) {
            $displayParams['nl2br'] = ($editor != "html");
        }

        if (!isset($displayParams['htmlescape']) && $editor != "html") {
            $displayParams['htmlescape'] = true;
        }

        if (!isset($displayParams['url2html'])) {
            $displayParams['url2html'] = ($editor != "html");
        }

        return parent::getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
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

                $userLang = explode('_', $_SESSION['authenticated_user_language'])[0];
                $config = [];
                if ($form_name !== '') {
                    $config['selector'] = "#{$form_name} " . "#" . $vardef['name'];
                } else {
                    $config['selector'] = "#" . $vardef['name'];
                }

                $config['width'] = '80%';
                $config['height'] = '480';
                $config['language'] = $userLang;
                $config['language_url'] = 'SticInclude/vendor/tinymce/langs/' . $userLang . '.js';
                $config['toolbar1'] = 'code undo redo | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent';
                $config['toolbar2'] = 'print preview media | forecolor backcolor | image | emoticons | table | link | fontselect fontsizeselect';
                $config['resize'] = 'both';
                $config['code_dialog_height'] = 600;
                $config['code_dialog_width'] = 650;
                $config['plugins'] = [ 'fullpage advlist autolink lists link image charmap print preview hr anchor pagebreak 
                    searchreplace wordcount visualblocks visualchars code fullscreen
                    insertdatetime media nonbreaking save table contextmenu directionality
                    emoticons template paste textcolor colorpicker textpattern imagetools'
                ];
                $config['extended_valid_elements'] = 'style,html[xmlns],style[dir|lang|media|title|type],hr[class|width|size|noshade],@[class|style]';
                $config['custom_elements'] = 'style,link,~link';
                $config['content_style'] = "@import url('https://fonts.googleapis.com/css2?family=Open+Sans')";
                $config['font_formats'] = 'Andale Mono=andale mono,times;Arial=arial,helvetica,sans-serif;Arial Black=arial black,avant garde;Book Antiqua=book antiqua,palatino;Comic Sans MS=comic sans ms,sans-serif;Courier New=courier new,courier;Georgia=georgia,palatino;Helvetica=helvetica;Impact=impact,chicago;Open Sans=Open Sans, sans-serif;Symbol=symbol;Tahoma=tahoma,arial,helvetica,sans-serif;Terminal=terminal,monaco;Times New Roman=times new roman,times;Trebuchet MS=trebuchet ms,geneva;Verdana=verdana,geneva;Webdings=webdings;Wingdings=wingdings,zapf dingbats';
                $config['entity_encoding'] = 'raw';
                $config['convert_urls'] = false;

                $jsConfig = json_encode($config);
                $initiate = '<script type="text/javascript"> tinyMCE.init(' . $jsConfig . ')</script>';
            }
        }

        $this->ss->assign("tinymce", $initiate);
    }
}
