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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once "modules/stic_AWF_Forms/core/includes.php"; 
require_once "modules/stic_AWF_Forms/core/FormHtmlGeneratorService.php";

class FormRenderService {

    /**
     * @param string $recordId Record ID (required if configData is empty)
     * @param bool $isPreview Whether this is preview mode
     * @param array|null $configData Direct configuration (for Live Preview)
     * @return string Generated HTML
     * @throws Exception
     */
    public function render(string $recordId, bool $isPreview, ?array $configData = null): string
    {
        // Get ConfigData
        if ($configData === null) {
            if (empty($recordId)) {
                throw new Exception("Record ID missing for DB generation.");
            }

            $bean = BeanFactory::getBean('stic_AWF_Forms', $recordId);
            if (!$bean || empty($bean->id)) {
                throw new Exception("Form not found with id: {$recordId}");
            }

            $jsonConfig = $bean->configuration;
            $jsonConfig = html_entity_decode($jsonConfig, ENT_QUOTES, 'UTF-8');
            $configData = json_decode($jsonConfig, true);

            if (json_last_error() !== JSON_ERROR_NONE) {
                $errorMsg = json_last_error_msg();
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid JSON: (ID: $recordId): $errorMsg");
                throw new Exception("Form Configuration is corrupted: $errorMsg");
            }
            
            if (!is_array($configData)) {
                $configData = [];
            }

            // CRM form type: detect form_type for legacy forms
            require_once "modules/stic_AWF_Forms/Utils.php";
            stic_AWF_FormsUtils::detectAndSaveFormType($bean, $configData);
        }

        // Process ConfigData
        try {
            $formConfig = FormConfig::fromJsonArray($configData);
            if (!$isPreview && !empty($_GET)) {
                $this->prefillFieldsFromRequest($formConfig, $_GET);
            }
        } catch (\Throwable $e) {
            throw new Exception("Error processing config Form: " . $e->getMessage());
        }

        // Define action url (submit url)
        global $sugar_config;
        $siteUrl = rtrim($sugar_config['site_url'], '/');
        $actionUrl = "{$siteUrl}/index.php?entryPoint=stic_AWF_responseHandler&id={$recordId}";

        // Generate url and return it
        $generator = new FormHtmlGeneratorService();
        $html = $generator->generate($formConfig, $recordId, $actionUrl, $isPreview);
        return $html;
    }

    /**
     * Prefill form fields from request data (GET or POST)
     * @param FormConfig $config
     * @param array $requestData
     * @return void
     */
    private function prefillFieldsFromRequest(FormConfig $config, array $requestData): void {
        foreach ($config->data_blocks as $block) {
            foreach ($block->fields as $field) {
                // Look for matches by field name (e.g., "email", "first_name")
                // Also support the full format "BlockName_fieldname" when disambiguation is needed
                $val = null;
                
                if (isset($requestData[$field->name])) {
                    $val = $requestData[$field->name];
                } elseif (isset($requestData[$field->getPhpKey()])) {
                    $val = $requestData[$field->getPhpKey()];
                }

                if ($val !== null) {
                    // Basic sanitize to avoid XSS when injecting into input value=""
                    $safeVal = htmlspecialchars($val, ENT_QUOTES, 'UTF-8');
                    // Assign the value to the corresponding field
                    $field->value = $safeVal;
                    $field->value_text = $safeVal;
                }
            }
        }
    }
}