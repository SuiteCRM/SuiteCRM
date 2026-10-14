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

include_once "modules/stic_AWF_Forms/actions/coreActions.php";

/**
 * MaxDocumentSizeValidatorAction
 *
 * Validates that an uploaded file does not exceed a configurable maximum size.
 * Client-side JS validation checks the File object size.
 * Server-side enforcement is handled by SaveDocumentBlockAction.
 */
class MaxDocumentSizeValidatorAction extends ValidatorActionDefinition {
    public function __construct() {
        $this->isActive = true;
        $this->baseLabel = 'LBL_MAX_DOCUMENT_SIZE_VALIDATOR_ACTION';
        $this->supportedDataTypes = [ActionDataType::FILE];
    }

    public function getParameters(): array {
        $paramMaxSize = new ActionParameterDefinition();
        $paramMaxSize->name = 'max_size_mb';
        $paramMaxSize->text = $this->translate('MAX_SIZE_MB_TEXT');
        $paramMaxSize->type = ActionParameterType::VALUE;
        $paramMaxSize->dataType = ActionDataType::INTEGER;
        $paramMaxSize->required = false;
        $paramMaxSize->defaultValue = 2; // Default to 2 MB
        return [$paramMaxSize];
    }

    /**
     * Returns rules to automatically apply this validation.
     * Can filter by field type (vardef type) editor in form (subtype_in_form), or by name pattern (regex).
     * @return array ex: ['types' => ['email'], 'subtypes_in_form' => ['text_email'], 'name_patterns' => ['/^email/i']]
     */
    public function getAutoApplyRules(): array {
        return [
            'types' => ['file'], // File type
        ];
    }

    public function getDefaultErrorMessage(): string {
        return $this->translate('ERROR_MESSAGE_TEXT');
    }

    public function getValidationJS(): string {
        return <<<JS
(value, params, formElement) => {
    if (!value) return true;
    
    // Locate the actual file input
    const inputEl = Array.from(formElement.querySelectorAll('input[type="file"]')).find(i => i.value === value);
    if (!inputEl || !inputEl.files || !inputEl.files[0]) return true;
    
    const file = inputEl.files[0];
    const maxMb = parseInt(params.max_size_mb || 2, 10);
    
    return file.size <= maxMb * 1024 * 1024;
}
JS;
    }

    public function validateBackend(mixed $value, array $params): bool {
        if (empty($_FILES)) {
            return true;
        }
        
        $maxMb = (int)($params['max_size_mb'] ?? 2);
        
        foreach ($_FILES as $file) {
            if (is_array($file) && $file['error'] === UPLOAD_ERR_OK) {
                if ($file['size'] > $maxMb * 1024 * 1024) {
                    return false; // File size bypass detected on backend
                }
            }
        }
        return true;
    }
}
