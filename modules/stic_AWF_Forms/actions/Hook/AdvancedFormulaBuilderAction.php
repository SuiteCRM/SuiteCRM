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
 * AdvancedFormulaBuilderAction
 *
 * Action that allows selecting multiple source fields (FIELD_LIST), adding their
 * values in real time in the browser, writing the result to a field and optionally
 * classifying the result by ranges in a label field.
 */
class AdvancedFormulaBuilderAction extends HookActionDefinition implements IFrontendAction
{
    public function __construct()
    {
        $this->isActive = true;
        $this->isUserSelectable = true;
        $this->category = 'data';
        $this->baseLabel = 'LBL_ADVANCED_FORMULA_BUILDER_ACTION';
    }

    /**
     * Defines the parameters that will be displayed in the wizard.
     */
    public function getParameters(): array
    {
        // 1. Numeric result field (target field)
        $paramTargetNum = new ActionParameterDefinition();
        $paramTargetNum->name = 'target_field';
        $paramTargetNum->text = $this->translate('TARGET_FIELD_TEXT');
        $paramTargetNum->description = $this->translate('TARGET_FIELD_DESC');
        $paramTargetNum->type = ActionParameterType::FIELD;
        $paramTargetNum->required = true;

        // 2. List of source fields (multiple selection)
        $paramSourceFields = new ActionParameterDefinition();
        $paramSourceFields->name = 'source_fields';
        $paramSourceFields->text = $this->translate('SOURCE_FIELDS_TEXT');
        $paramSourceFields->description = $this->translate('SOURCE_FIELDS_DESC');
        $paramSourceFields->type = ActionParameterType::FIELD_LIST;
        $paramSourceFields->required = true;

        // 3. Rounding decimals
        $paramDecimals = new ActionParameterDefinition();
        $paramDecimals->name = 'decimals';
        $paramDecimals->text = $this->translate('DECIMALS_TEXT');
        $paramDecimals->description = $this->translate('DECIMALS_DESC');
        $paramDecimals->type = ActionParameterType::VALUE;
        $paramDecimals->dataType = ActionDataType::INTEGER;
        $paramDecimals->defaultValue = '2';
        $paramDecimals->required = false;

        // 4. Classification / label field (optional)
        $paramTargetLabel = new ActionParameterDefinition();
        $paramTargetLabel->name = 'target_label_field';
        $paramTargetLabel->text = $this->translate('TARGET_LABEL_FIELD_TEXT');
        $paramTargetLabel->description = $this->translate('TARGET_LABEL_FIELD_DESC');
        $paramTargetLabel->type = ActionParameterType::FIELD;
        $paramTargetLabel->required = false;

        // 5. Range definition (optional)
        $paramRanges = new ActionParameterDefinition();
        $paramRanges->name = 'ranges_config';
        $paramRanges->text = $this->translate('RANGES_CONFIG_TEXT');
        $paramRanges->description = $this->translate('RANGES_CONFIG_DESC');
        $paramRanges->type = ActionParameterType::VALUE;
        $paramRanges->dataType = ActionDataType::TEXTAREA;
        $paramRanges->required = false;

        return [
            $paramTargetNum,
            $paramSourceFields,
            $paramDecimals,
            $paramTargetLabel,
            $paramRanges
        ];
    }

    /**
     * The calculation and classification are handled in the browser.
     */
    public function execute(ExecutionContext $context, FormAction $actionConfig): ActionResult
    {
        return new ActionResult(ResultStatus::OK, $actionConfig, $this->translate('EXECUTE_MESSAGE'));
    }

    /**
     * Returns the assets (CSS/JS) that should be injected into the frontend for this action.
     *
     * @param array $params Action parameters (unresolved)
     * @param FormConfig|null $formConfig Form configuration (if applicable)
     * @param string $formId Form ID (if applicable)
     * @return array Structure: ['script' => ['console.log("hi")'], 'css' => [], 'html' => []]
     */
    public function getFrontendAssets(array $params, ?FormConfig $formConfig = null, string $formId = ''): array
    {
        $sourceFields = [];
        $targetField = '';
        $decimals = 2;
        $targetLabelField = '';
        $rangesConfig = '';

        foreach ($params as $p) {
            if ($p->name === 'source_fields') {
                $sourceFields = is_array($p->value) ? $p->value : explode(',', (string)$p->value);
            }
            if ($p->name === 'target_field') {
                $targetField = (string)$p->value;
            }
            if ($p->name === 'decimals') {
                $decimals = (int)$p->value;
            }
            if ($p->name === 'target_label_field') {
                $targetLabelField = (string)$p->value;
            }
            if ($p->name === 'ranges_config') {
                $rangesConfig = (string)$p->value;
            }
        }

        // Make sure the target fields are excluded from the list of source fields for safety
        $sourceFields = array_filter(array_map('trim', $sourceFields), function ($f) use ($targetField, $targetLabelField) {
            return !empty($f) && $f !== $targetField && $f !== $targetLabelField;
        });

        if (empty($sourceFields) || empty($targetField)) {
            return [];
        }

        $jsSourceFields = json_encode(array_values($sourceFields));
        $jsTargetField = json_encode($targetField);
        $jsDecimals = (int)$decimals;
        $jsTargetLabel = json_encode($targetLabelField);
        $jsRanges = json_encode($rangesConfig);

        $script = <<<JS
        document.addEventListener('DOMContentLoaded', function() {
            const form = document.querySelector('form.needs-validation') || document.querySelector('form');
            if (!form) return;

            const sourceFieldsList = {$jsSourceFields};
            const targetName = {$jsTargetField};
            const decimals = {$jsDecimals};
            const targetLabelName = {$jsTargetLabel};
            const rangesRaw = {$jsRanges};

            function parseRanges(raw) {
                if (!raw) return [];
                const ranges = [];
                const lines = raw.split(/[\\r\\n|]+/);

                lines.forEach(line => {
                    const str = line.trim();
                    if (!str || !str.includes(':')) return;

                    const colonIdx = str.indexOf(':');
                    const key = str.slice(0, colonIdx).trim();
                    const label = str.slice(colonIdx + 1).trim();

                    if (key.includes('-')) {
                        const parts = key.split('-').map(p => parseFloat(p.trim()));
                        if (!isNaN(parts[0]) && !isNaN(parts[1])) {
                            ranges.push({ min: parts[0], max: parts[1], label });
                        }
                    } else if (key.startsWith('>=')) {
                        const val = parseFloat(key.slice(2).trim());
                        if (!isNaN(val)) ranges.push({ min: val, max: Infinity, label });
                    } else if (key.startsWith('<=')) {
                        const val = parseFloat(key.slice(2).trim());
                        if (!isNaN(val)) ranges.push({ min: -Infinity, max: val, label });
                    } else {
                        const val = parseFloat(key);
                        if (!isNaN(val)) ranges.push({ min: val, max: val, label });
                    }
                });
                return ranges;
            }

            const parsedRanges = parseRanges(rangesRaw);
            const sourceFieldsSet = new Set(sourceFieldsList);

            function getFieldValue(name) {
                // Support for selected radio buttons/checkboxes
                const checkedEl = form.querySelector('input[name="' + name + '"]:checked');
                if (checkedEl) {
                    const v = parseFloat(checkedEl.value);
                    return isNaN(v) ? 0 : v;
                }

                // Support for direct text or dropdown fields
                const directEl = form.elements[name];
                if (directEl && directEl.value !== undefined && directEl.value !== '') {
                    const v = parseFloat(directEl.value);
                    return isNaN(v) ? 0 : v;
                }

                return 0;
            }

            let isCalculating = false;

            function recalculate() {
                if (isCalculating) return;
                isCalculating = true;

                try {
                    let total = 0;

                    // Add all selected source fields
                    sourceFieldsList.forEach(name => {
                        total += getFieldValue(name);
                    });

                    total = Number(total.toFixed(decimals));

                    // Update the numeric target field
                    const target = form.elements[targetName];
                    if (target && target.value != total) {
                        target.value = total;
                        target.dispatchEvent(new Event('input', { bubbles: true }));
                        target.dispatchEvent(new Event('change', { bubbles: true }));
                    }

                    // Classification by ranges (optional)
                    if (targetLabelName) {
                        let matchedLabel = '';
                        for (let i = 0; i < parsedRanges.length; i++) {
                            const r = parsedRanges[i];
                            if (total >= r.min && total <= r.max) {
                                matchedLabel = r.label;
                                break;
                            }
                        }

                        const targetLabel = form.elements[targetLabelName];
                        if (targetLabel && targetLabel.value !== matchedLabel) {
                            targetLabel.value = matchedLabel;
                            targetLabel.dispatchEvent(new Event('input', { bubbles: true }));
                            targetLabel.dispatchEvent(new Event('change', { bubbles: true }));
                        }
                    }
                } finally {
                    isCalculating = false;
                }
            }

            // Listen for changes in the form elements
            form.addEventListener('input', function(e) {
                if (isCalculating) return;
                const cleanName = (e.target.name || '').replace('[]', '');
                if (sourceFieldsSet.has(cleanName)) recalculate();
            });

            form.addEventListener('change', function(e) {
                if (isCalculating) return;
                const cleanName = (e.target.name || '').replace('[]', '');
                if (sourceFieldsSet.has(cleanName)) recalculate();
            });

            // Run the initial calculation on load
            recalculate();
        });
JS;

        return ['script' => [$script]];
    }
}
