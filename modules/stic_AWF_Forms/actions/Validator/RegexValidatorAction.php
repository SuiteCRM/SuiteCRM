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
 * RegexValidatorAction
 *
 * Action that validates a field according to a Regex expression
 */
class RegexValidatorAction extends ValidatorActionDefinition {
    public function __construct() {
        $this->isActive = true;
        $this->baseLabel = 'LBL_REGEX_VALIDATOR_ACTION';
        $this->supportedDataTypes = [ActionDataType::TEXT, ActionDataType::TEXTAREA, ActionDataType::EMAIL, ActionDataType::TEL, ActionDataType::URL];
    }

    public function getParameters(): array {
        $paramPattern = new ActionParameterDefinition();
        $paramPattern->name = 'pattern';
        $paramPattern->text = $this->translate('PATTERN_TEXT'); 
        $paramPattern->type = ActionParameterType::VALUE;
        $paramPattern->required = true;
        return [$paramPattern];
    }


    public function getDefaultErrorMessage(): string {
        return $this->translate('ERROR_MESSAGE_TEXT');
    }

    public function getValidationJS(): string {
        return "(value, params, formElement) => {
            if (!value) return true; 
            if (!params.pattern) return true;
            try {
                const regex = new RegExp(params.pattern);
                return regex.test(value);
            } catch(e) {
                console.error('Invalid Regex', e);
                return true; 
            }
        }";
    }

    public function validateBackend(mixed $value, array $params): bool {
        if (empty($value)) return true;
        if (empty($params['pattern'])) return true;

        $pattern = trim($params['pattern']);

        // Check if the pattern already has valid delimiters (e.g., starts and ends with '/')
        // If not, automatically add them for compatibility with PHP PCRE
        if (!preg_match('/^\/.*\/[a-z]*$/i', $pattern)) {
            $pattern = '/' . str_replace('/', '\/', $pattern) . '/';
        }

        // Use the @ operator to suppress warnings from preg_match
        $matchResult = @preg_match($pattern, (string)$value);
        return $matchResult === 1;
    }
}