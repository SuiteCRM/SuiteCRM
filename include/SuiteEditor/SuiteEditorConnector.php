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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorInterface.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorSettings.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorSettingsForDirectHTML.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorDirectHTML.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorSettingsForTinyMCE.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorTinyMCE.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorSettingsForMozaik.php');
include_once get_custom_file_if_exists('include/SuiteEditor/SuiteEditorMozaik.php');

/**
 * Class SuiteEditor
 *
 * User Preference Editor connector class for different kind of editors
 * typically for Email Templates but any HTML or text editing..
 */
class SuiteEditorConnector
{
    public static function getSuiteSettings($html, $width)
    {
        $settings = [
            'contents' => $html,
            'textareaId' => 'body_text',
            'elementId' => 'email_template_editor',
            'width' => $width,
            'clickHandler' => "function(e){
                onClickTemplateBody();
            }",
        ];

        if($_REQUEST["module"] == "Campaigns"){
            //use loadtemplate() to populate template body on TinyMCE initialisation rather than page load for campaigns
                $settings['tinyMCESetup'] = "{
                setup: function(editor) {
                    editor.on('focus', function(e){
                        onClickTemplateBody();
                    });
                    editor.on('init', function(e){
                        loadtemplate();
                    });
                },
                height : '480',
                plugins: ['code', 'table', 'link', 'image'],
                toolbar: ['fontselect | fontsizeselect | bold italic underline | forecolor backcolor | styleselect | outdent indent | link image'],
                convert_urls: false,
            }";
        }else{
            //default TinyMCESetup settings
            $settings['tinyMCESetup'] = "{
                setup: function(editor) {
                    editor.on('focus', function(e){
                        onClickTemplateBody();
                    });
                },
                height : '480',
                plugins: ['code', 'table', 'link', 'image'],
                toolbar: ['fontselect | fontsizeselect | bold italic underline | forecolor backcolor | styleselect | outdent indent | link image'],
                convert_urls: false,
            }";
        }



        return $settings;
    }

    /**
     * return an output HTML of user selected editor for templates
     * based on current user preferences
     *
     * @param null $settings (optional) extends of selected editor default settings
     * @throws Exception unknown or incorrect editor
     * @return string HTML output of editor
     */
    public static function getHtml($settings = null)
    {
        global $current_user;

        switch ($current_user->getEditorType()) {

            case 'none':
                $editor = new SuiteEditorDirectHTML();
                $settings = new SuiteEditorSettingsForDirectHTML($settings);
                break;

            case 'tinymce':
                $editor = new SuiteEditorTinyMCE();
                $settings = new SuiteEditorSettingsForTinyMCE($settings);
                break;

            case 'mozaik':
                $editor = new SuiteEditorMozaik();
                $settings = new SuiteEditorSettingsForMozaik($settings);
                break;

            // new editor type should be possible to store in
            // user preferences but in this file for
            // add more type use the syntax bellow... for e.g:
            //
            //case 'your_awesome_editor':
            //    $editor = new SuiteEditorAwesome(); // where the editor class should implements SuiteEditorInterface
            //    break;

            default:
                throw new Exception('unknown editor type: '.$current_user->getEditorType());
        }

        // just make sure the type of editor implements a SuiteEditorInterface..

        if (!($editor instanceof SuiteEditorInterface)) {
            throw new Exception("class $editor is not a SuiteEditorInterface");
        }

        // rendering the editor output HTML

        $editor->setup($settings);

        $smarty = new Sugar_Smarty();
        $smarty->assign('editor', $editor->getHtml());
        return $smarty->fetch(get_custom_file_if_exists('include/SuiteEditor/tpls/SuiteEditorConnector.tpl'));
    }
}
