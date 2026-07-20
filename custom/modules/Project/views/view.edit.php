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

require_once 'modules/Project/views/view.edit.php';
require_once 'SticInclude/Views.php';

class CustomProjectViewEdit extends ProjectViewEdit {
    public function __construct() {
        parent::__construct();
        $this->useForSubpanel = true;

        // Since the suite base modules name the bean in the singular, we configure in the view the name of the module in the plural. This property will be used by the SticViews class to load the language files
        $this->moduleName = 'Project';
    }

    public function preDisplay() {

        parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here you custom code

        $cancelCustomCode = '{if !empty($smarty.request.return_action) && $smarty.request.return_action == "ProjectTemplatesDetailView" && (!empty($fields.id.value) || !empty($smarty.request.return_id)) }<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" accessKey="{$APP.LBL_CANCEL_BUTTON_KEY}" class="button" onclick="SUGAR.ajaxUI.loadContent(\'index.php?action=ProjectTemplatesDetailView&module={$smarty.request.return_module|default:\'Project\'|escape:\'url\'}&record={$smarty.request.return_id|default:$fields.id.value|escape:\'url\'}\'); return false;" type="button" name="button" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" id="CANCEL{$place}"> {elseif !empty($smarty.request.return_action) && $smarty.request.return_action == "DetailView" && (!empty($fields.id.value) || !empty($smarty.request.return_id)) }<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" accessKey="{$APP.LBL_CANCEL_BUTTON_KEY}" class="button" onclick="SUGAR.ajaxUI.loadContent(\'index.php?action=DetailView&module={$smarty.request.return_module|default:\'Project\'|escape:\'url\'}&record={$smarty.request.return_id|default:$fields.id.value|escape:\'url\'}\'); return false;" type="button" name="button" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" id="CANCEL{$place}"> {elseif $is_template}<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" accessKey="{$APP.LBL_CANCEL_BUTTON_KEY}" class="button" onclick="SUGAR.ajaxUI.loadContent(\'index.php?action=ProjectTemplatesListView&module={$smarty.request.return_module|default:\'Project\'|escape:\'url\'}\'); return false;" type="button" name="button" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" id="CANCEL{$place}"> {elseif !empty($fields.id.value) || !empty($smarty.request.return_id) || !empty($smarty.request.record)}<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" accessKey="{$APP.LBL_CANCEL_BUTTON_KEY}" class="button" onclick="SUGAR.ajaxUI.loadContent(\'index.php?action=DetailView&module=Project&record={$smarty.request.return_id|default:$smarty.request.record|default:$fields.id.value|escape:\'url\'}\'); return false;" type="button" name="button" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" id="CANCEL{$place}"> {else}<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" accessKey="{$APP.LBL_CANCEL_BUTTON_KEY}" class="button" onclick="SUGAR.ajaxUI.loadContent(\'index.php?action=index&module=Project\'); return false;" type="button" name="button" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" id="CANCEL{$place}"> {/if}';

        // Replace the Cancel button definition in the form buttons array
        $buttons = $this->ev->defs['templateMeta']['form']['buttons'] ?? null;
        if (is_array($buttons)) {
            foreach ($buttons as $index => $button) {
                // Detect Cancel button by id or label
                $isCancelButton = false;
                if ($button === 'CANCEL') {
                    $isCancelButton = true;
                } elseif (is_array($button)) {
                    if (!empty($button['customCode']) && strpos($button['customCode'], 'LBL_CANCEL_BUTTON_LABEL') !== false) {
                        $isCancelButton = true;
                    } elseif (!empty($button['value']) && $button['value'] === 'Cancel') {
                        $isCancelButton = true;
                    }
                }

                if ($isCancelButton) {
                    // Override Cancel rendering with custom ajaxUI navigation
                    $this->ev->defs['templateMeta']['form']['buttons'][$index] = array(
                        'customCode' => $cancelCustomCode,
                    );
                    break;
                }
            }
        }
    }

    public function display() {

        parent::display();

        SticViews::display($this);

        echo getVersionedScript("custom/modules/Project/SticUtils.js");

        // Write here you custom code
    }
}