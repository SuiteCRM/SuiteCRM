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

include 'include/SuiteEditor/SuiteEditorConnector.php';
class SticSuiteEditorConnector extends SuiteEditorConnector 
{
    public static function getSticSuiteSettings($html, $width) 
    {
        $userLang = explode('_', $_SESSION['authenticated_user_language'])[0];
        $settings = array(
            'contents' => $html,
            'textareaId' => 'body_text',
            'elementId' => 'email_template_editor',
            'clickHandler' => "function(e){
                onClickTemplateBody();
            }",
        );

        if ($_REQUEST["module"] == "Campaigns") 
        {   // module Campaigns
            $settings['tinyMCESetup'] = "{
                setup: function(editor) {
                    editor.on('focus', function(e){
                        onClickTemplateBody();
                    });
                    editor.on('init', function(e){
                        loadtemplate();
                    });
                },
                width: '80%',
                height : '480',
                language: '{$userLang}',
                language_url: 'SticInclude/vendor/tinymce/langs/{$userLang}.js',
                toolbar1: 'code undo redo | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent',
                toolbar2: 'print preview media | forecolor backcolor | image | emoticons | table | link | fontselect fontsizeselect',
                resize: 'both',   
                code_dialog_height: 600,
                code_dialog_width: 650,
                plugins: [
                    'fullpage advlist autolink lists link image charmap print preview hr anchor pagebreak',
                    'searchreplace wordcount visualblocks visualchars code fullscreen',
                    'insertdatetime media nonbreaking save table contextmenu directionality',
                    'emoticons template paste textcolor colorpicker textpattern imagetools'
                ],                   
                extended_valid_elements: 'style,html[xmlns],style[dir|lang|media|title|type],hr[class|width|size|noshade],@[class|style]',
                custom_elements: 'style,link,~link',     
                content_style:\"@import url('https://fonts.googleapis.com/css2?family=Open+Sans');\",
                font_formats:'Andale Mono=andale mono,times;Arial=arial,helvetica,sans-serif;Arial Black=arial black,avant garde;Book Antiqua=book antiqua,palatino;Comic Sans MS=comic sans ms,sans-serif;Courier New=courier new,courier;Georgia=georgia,palatino;Helvetica=helvetica;Impact=impact,chicago;Open Sans=Open Sans, sans-serif;Symbol=symbol;Tahoma=tahoma,arial,helvetica,sans-serif;Terminal=terminal,monaco;Times New Roman=times new roman,times;Trebuchet MS=trebuchet ms,geneva;Verdana=verdana,geneva;Webdings=webdings;Wingdings=wingdings,zapf dingbats',                
                entity_encoding: 'raw',
                convert_urls: false,
            }";
        } 
        else 
        {   // module Email Templates
            $settings['tinyMCESetup'] = "{
                setup: function(editor) {
                    editor.on('focus', function(e){
                        onClickTemplateBody();
                    });
                },
                width: '80%',
                height : '480',
                language: '{$userLang}',
                language_url: 'SticInclude/vendor/tinymce/langs/{$userLang}.js',
                toolbar1: 'code undo redo | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent',
                toolbar2: 'print preview media | forecolor backcolor | image | emoticons | table | link | fontselect fontsizeselect',
                resize: 'both',   
                code_dialog_height: 600,
                code_dialog_width: 650,
                plugins: [
                    'fullpage advlist autolink lists link image charmap print preview hr anchor pagebreak',
                    'searchreplace wordcount visualblocks visualchars code fullscreen',
                    'insertdatetime media nonbreaking save table contextmenu directionality',
                    'emoticons template paste textcolor colorpicker textpattern imagetools'
                ],                   
                extended_valid_elements: 'style,html[xmlns],style[dir|lang|media|title|type],hr[class|width|size|noshade],@[class|style]',
                custom_elements: 'style,link,~link',     
                content_style:\"@import url('https://fonts.googleapis.com/css2?family=Open+Sans');\",
                font_formats:'Andale Mono=andale mono,times;Arial=arial,helvetica,sans-serif;Arial Black=arial black,avant garde;Book Antiqua=book antiqua,palatino;Comic Sans MS=comic sans ms,sans-serif;Courier New=courier new,courier;Georgia=georgia,palatino;Helvetica=helvetica;Impact=impact,chicago;Open Sans=Open Sans, sans-serif;Symbol=symbol;Tahoma=tahoma,arial,helvetica,sans-serif;Terminal=terminal,monaco;Times New Roman=times new roman,times;Trebuchet MS=trebuchet ms,geneva;Verdana=verdana,geneva;Webdings=webdings;Wingdings=wingdings,zapf dingbats',                
                entity_encoding: 'raw',
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
    public static function getSticHtml($settings = null) {
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
