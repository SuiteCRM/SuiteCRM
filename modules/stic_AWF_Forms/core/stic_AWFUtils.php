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

class stic_AWFUtils {
    /**
     * Converts a form string value to the correct PHP type based on the CRM field type.
     * @param mixed $valueToCast The value to convert
     * @param ?string $crmFieldType The CRM field type
     * @param ExecutionContext $context The execution context
     * @return mixed The converted value
     */
    public static function castCrmValue(mixed $valueToCast, ?string $crmFieldType, ExecutionContext $context): mixed {
        
        if (is_array($valueToCast)) {
            // If it's an array from a multiselect: Array -> encoded multienum string
            if ($crmFieldType === 'multienum') {
                return encodeMultienumValue($valueToCast);
            }
            // If it's an array with subarrays/objects, store as JSON
            $firstElement = reset($valueToCast);
            if (is_array($firstElement) || is_object($firstElement)) {
                return json_encode($valueToCast, JSON_UNESCAPED_UNICODE);
            }
            // For other array types (not multienum), convert to comma-separated string
            return implode(',', $valueToCast);
        }

        // If no type is defined, treat as text
        if ($crmFieldType === null) {
            $crmFieldType = 'text';
        }

        switch ($crmFieldType) {
            // Boolean
            case 'bool':
            case 'checkbox':
                $lowerValue = strtolower(trim((string)$valueToCast));
                return !($lowerValue === 'false' || $lowerValue === '0' || $lowerValue === 'off' || $lowerValue === '');

            // Numerics
            case 'int':
                return (int)$valueToCast;
            
            case 'float':
            case 'double':
            case 'decimal':
            case 'currency': 
                return (float)$valueToCast;

            // Dates and times
            case 'date':
            case 'time':
            case 'datetime':
            case 'datetimecombo':
                $baseTimestamp = (int)$context->submissionTimestamp;
                // strtotime also handles "today", "+1 day", etc.
                $parsedTime = @strtotime($valueToCast, $baseTimestamp);
                
                if ($parsedTime === false) {
                    $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Can not parse date '{$valueToCast}'.");
                    return null;
                }
                try {
                    $dateTimeObj = new \DateTime();
                    $dateTimeObj->setTimestamp($parsedTime);
                    return $dateTimeObj;
                } catch (\Exception $e) { return null; }

            // Strings
            case 'varchar':
            case 'text':
            case 'relate':
            case 'enum':
            case 'multienum':
            case 'tel':
            case 'url':
            case 'email':
            case 'text':
            default:
                return (string)$valueToCast;
        }
    }

    /** 
     * Generate a summary in HTML with all the data of the form based on the Layout. 
     * 
     * @param ExecutionContext $context The context that contains the data. 
     * @param array $options Display options ['title' => string, 'includeCss' => bool] 
     * @return string An HTML string with the summary table. 
     */
    public static function generateSummaryHtml(ExecutionContext $context, array $options = []): string
    {
        $theme = $context->formConfig->layout->theme;
        $layout = $context->formConfig->layout;
        $formData = $context->formData;

        // Default options
        $title = $options['title'] ?? '';
        $includeCss = $options['includeCss'] ?? true;

        $fontFamily = 'system-ui, -apple-system, sans-serif';
        $fontSize = 16;
        $textColor = '#212529';
        $borderColor = '#dee2e6';
        
        if ($includeCss) {
            $fontFamily = $theme->font_family ?? $fontFamily;
            $fontSize = $theme->font_size ?? $fontSize;
            $textColor = $theme->text_color ?? $textColor;
            $borderColor = $theme->border_color ?? $borderColor;
        }
        $formWidth = $theme->form_width ?? '800px';
        
        $html = '';

        $html .= "<div style=\"font-family: {$fontFamily};font-size: {$fontSize}px;color: {$textColor};width: {$formWidth};margin: 0 auto;line-height: 1.5;\">";
        if ($title != '') {
            $html .= "<h1>" . htmlspecialchars($title) . "</h1>";
        }
        $html .= "<div>";

        // Iterate through sections (layout)
        foreach ($layout->structure as $section) {
            $html .= "<div style=\"margin-bottom: 25px; border: 1px solid {$borderColor}; border-radius: 6px; overflow: hidden; background-color: #fff;\">";

            // If the section has a title and should be displayed
            if ($section->showTitle && !empty($section->title)) {
                $html .= "<div style=\"color: {$theme->text_color};background-color: {$theme->page_bg_color};padding: 8px 12px;margin: 0;font-size: 1.2em;font-weight: bold;border-bottom: 2px solid {$borderColor};\">" . htmlspecialchars($section->title) . "</div>";
            }
            $html .= "<table style=\"width: 100%;border-collapse: collapse;margin-bottom: 0px;\">";
            $hasFields = false;

            // Elements of the section (data blocks)
            foreach ($section->elements as $element) {
                if ($element->type !== 'datablock') continue;

                $block = $context->getDataBlockById($element->ref_id);
                if (!$block) continue;

                foreach ($block->fields as $fieldDef) {
                    // Only show visible fields in the form
                    if ($fieldDef->type_field === DataBlockFieldType::FIXED) {
                        continue;
                    }
                    // If it has no label, it is not displayed
                    if (empty($fieldDef->label)) {
                        continue;
                    }

                    $formKey = "{$block->name}.{$fieldDef->name}";
                    if ($fieldDef->type_field === DataBlockFieldType::UNLINKED) {
                        $formKey = "_detached.{$formKey}";
                    }
                    $formKey = str_replace('.', '_', $formKey);
                    
                    // Value to display
                    $value = $formData[$formKey] ?? '';
                    $isHtmlValue = false; // Flag to indicate if the value contains HTML (for proper escaping)

                    // Render rating fields with icons
                    if ($fieldDef->type_in_form === 'rating') { 
                        $rawNum = (int)$value;
                        $subtype = !empty($fieldDef->subtype_in_form) ? $fieldDef->subtype_in_form : 'rating_stars';
                        $isHtmlValue = true; 
                        
                        if ($value === '' || $value === null) {
                            $displayValue = '<em style="color:#9aa0a6;">[ '.translate('LBL_EMPTY', 'stic_AWF_Responses').' ]</em>';
                        } else {
                            $displayValue = '<div style="display:inline-flex; gap:10px; align-items:center; font-size:1.5em; line-height:1;">';
                            
                            // Stars, emojis, thumbs or lights
                            if (in_array($subtype, ['rating_stars', 'rating_emoji', 'rating_thumbs', 'rating_lights'])) {
                                $options = [];
                                $isCumulative = false;
                                $isLight = false;
                                $baseColors = []; 
                                
                                // Get official icons and colors from Iconify API based on the subtype
                                if ($subtype === 'rating_stars') {
                                    $isCumulative = true;
                                    for ($i = 1; $i <= 5; $i++) {
                                        $options[$i] = ['empty' => 'bi:star', 'fill' => 'bi:star-fill'];
                                        $baseColors[$i] = 'ffc107'; // Yellow for all stars
                                    }
                                } elseif ($subtype === 'rating_emoji') {
                                    $options = [
                                        1 => ['empty' => 'bi:emoji-angry',   'fill' => 'bi:emoji-angry-fill'],
                                        2 => ['empty' => 'bi:emoji-frown',   'fill' => 'bi:emoji-frown-fill'],
                                        3 => ['empty' => 'bi:emoji-neutral', 'fill' => 'bi:emoji-neutral-fill'],
                                        4 => ['empty' => 'bi:emoji-smile',   'fill' => 'bi:emoji-smile-fill'],
                                        5 => ['empty' => 'bi:emoji-laughing','fill' => 'bi:emoji-laughing-fill']
                                    ];
                                    $baseColors = [1 => 'dc3545', 2 => 'fd7e14', 3 => 'ffc107', 4 => '20c997', 5 => '198754'];
                                } elseif ($subtype === 'rating_thumbs') {
                                    $options = [
                                        1 => ['empty' => 'bi:hand-thumbs-down', 'fill' => 'bi:hand-thumbs-down-fill'],
                                        5 => ['empty' => 'bi:hand-thumbs-up',   'fill' => 'bi:hand-thumbs-up-fill']
                                    ];
                                    $baseColors = [1 => 'dc3545', 5 => '198754'];
                                } elseif ($subtype === 'rating_lights') {
                                    $isLight = true;
                                    $options = [
                                        1 => ['empty' => 'bi:circle-fill', 'fill' => 'bi:circle-fill'],
                                        3 => ['empty' => 'bi:circle-fill', 'fill' => 'bi:circle-fill'],
                                        5 => ['empty' => 'bi:circle-fill', 'fill' => 'bi:circle-fill']
                                    ];
                                    $baseColors = [1 => 'dc3545', 3 => 'ffc107', 5 => '198754'];
                                }
                                $displayValue = '<div style="display:inline-flex; gap:12px; align-items:center;">';

                                // Render each icon
                                foreach ($options as $val => $iconNames) {
                                    $active = $isCumulative ? ($val <= $rawNum) : ($val === $rawNum);
                                    
                                    $iconName = $active ? $iconNames['fill'] : $iconNames['empty'];
                                    $targetColor = $baseColors[$val];

                                    if ($active) {
                                        $colorHex = $targetColor;
                                        $scale = 'transform: scale(1.6);';
                                    } else {
                                        $colorHex = $isLight ? $targetColor : 'adb5bd';
                                        $alfa = $isLight ? '20' : '80'; 
                                        $colorHex .= $alfa;
                                        $scale = '';
                                    }
                                    
                                    $imgUrl = "https://api.iconify.design/{$iconName}.svg?color=%23{$colorHex}";
                                    $displayValue .= "<img src='{$imgUrl}' style='{$scale} display:inline-block; vertical-align:middle; margin-right:6px;' alt='rating' />";
                                }
                                $displayValue .= '</div>';
                            }

                            // NPS (0 to 10)
                            elseif ($subtype === 'rating_nps') {
                                $displayValue = '<div style="display:flex; width:100%; gap:2px;">';
                                for ($i = 0; $i <= 10; $i++) {
                                    $active = ($i === $rawNum);
                                    if ($active) {
                                        if ($i <= 6) { $bg = '#dc3545'; $color = '#fff'; $border = '#dc3545'; }
                                        elseif ($i <= 8) { $bg = '#ffc107'; $color = '#212529'; $border = '#ffc107'; }
                                        else { $bg = '#198754'; $color = '#fff'; $border = '#198754'; }
                                    } else {
                                        $bg = 'transparent'; $color = '#6c757d'; $border = '#dee2e6';
                                    }
                                    $opacity = $active ? '1' : '0.6';
                                    $displayValue .= "<span style='flex:1; text-align:center; padding:5px 2px; font-weight:bold; background-color:{$bg}; color:{$color}; border:1px solid {$border}; border-radius:3px; opacity:{$opacity};'>{$i}</span>";
                                }
                                $displayValue .= '</div>';
                            }
                            $displayValue .= '</div>';
                        }
                    }
                    // Boolean (checkboxes and Switches)
                    elseif ($fieldDef->type === 'bool' || $fieldDef->type === 'checkbox' || in_array($fieldDef->subtype_in_form, ['select_checkbox', 'select_switch'])) {
                        global $app_strings;
                        $isTrue = ($value === '1' || $value === 'on' || $value === 'true' || $value === true);
                        $yesStr = $app_strings['LBL_YES'] ?? 'Yes';
                        $noStr  = $app_strings['LBL_NO'] ?? 'No';
                        
                        if ($isTrue) {
                            $displayValue = "<span style='color: #198754; font-weight: bold;'>✓ {$yesStr}</span>";
                        } else {
                            $displayValue = "<span style='color: #dc3545;'>✗ {$noStr}</span>";
                        }
                        $isHtmlValue = true; // Do not scape the string
                    }
                    // Dates and times
                    elseif (in_array($fieldDef->type, ['date', 'datetime', 'datetimecombo']) || in_array($fieldDef->subtype_in_form, ['date', 'date_time', 'date_datetime'])) {
                        if ($value === '' || $value === null) {
                            $displayValue = '<em style="color:#9aa0a6;">[ '.translate('LBL_EMPTY', 'stic_AWF_Responses').' ]</em>';
                            $isHtmlValue = true;
                        } else {
                            global $timedate;
                            try {
                                $dt = new \DateTime($value);
                                if ($fieldDef->type === 'date' || $fieldDef->subtype_in_form === 'date') {
                                    $displayValue = $timedate->asUserDate($dt);
                                } elseif ($fieldDef->subtype_in_form === 'date_time') {
                                    $displayValue = $timedate->asUserTime($dt);
                                } else {
                                    $displayValue = $timedate->asUser($dt);
                                }
                            } catch (\Exception $e) {
                                $displayValue = $value; 
                            }
                        }
                    }
                    // Enum and multienum with value options: we display the text instead of the value
                    elseif (!empty($fieldDef->value_options)) {
                        // Helper function to find the text of a value
                        $findLabel = function($val) use ($fieldDef) {
                            foreach ($fieldDef->value_options as $opt) {
                                if ($opt->value == $val) return $opt->text;
                            }
                            return $val; 
                        };
    
                        if (is_array($value)) {
                            $labels = array_map($findLabel, $value);
                            $displayValue = implode(', ', $labels);
                        } else {
                            $displayValue = $findLabel($value);
                        }
                    } 
                    // For other fields, we display the value directly (after converting arrays to strings)
                    else {
                        if (is_array($value)) {
                            $value = implode(', ', $value);
                        }
                        $displayValue = $value;
                    }
                    
                    // If the value does not contain HTML (like the rating field), we escape it to prevent XSS and preserve formatting
                    if (!$isHtmlValue) {
                        $displayValue = nl2br(htmlspecialchars((string)$displayValue));
                    }

                    $html .= "<tr>";
                    $html .= "<td style=\"padding: 8px 12px;border-bottom: 1px solid {$borderColor};vertical-align: top;width: 35%;font-weight: bold;color: {$textColor};background-color: rgba(0,0,0,0.02);\">" . htmlspecialchars($fieldDef->label) . "</td>";
                    $html .= "<td style=\"padding: 8px 12px;border-bottom: 1px solid {$borderColor};vertical-align: top;width: 65%;\">" . $displayValue . "</td>";
                    $html .= "</tr>";
                    $hasFields = true;
                }
            }

            if (!$hasFields) {
                // If the section had no visible fields, we close the table and continue (CSS will make it invisible or minimal)
            }
            $html .= "</table>";
            $html .= "</div>";
        }
        
        $html .= "</div>";
        $html .= "</div>";
        
        return $html;
    }

    /**
     * Generates a plain-text summary with all form data based on the Layout.
     *
     * @param ExecutionContext $context The context containing the data.
     * @param array $options Display options ['title' => string] 
     * @return string A plain-text string with the summary.
     */
    public static function generateSummaryText(ExecutionContext $context, array $options = []): string
    {
        $text = "";

        // Main title
        $title = $options['title'] ?? '';
        if ($title != '') {
            $text .= mb_strtoupper($title) . "\n" . str_repeat('=', mb_strlen($title)) . "\n\n";
        }
        
        $layout = $context->formConfig->layout;
        $formData = $context->formData; 
        
        foreach ($layout->structure as $section) {
            
            // Section title
            if ($section->showTitle && !empty($section->title)) {
                $sectionTitle = mb_strtoupper($section->title);
                $text .= $sectionTitle . "\n";
                $text .= str_repeat('-', mb_strlen($sectionTitle)) . "\n";
            } else {
                // Visual separator if there is no title
                $text .= "--------------------\n";
            }

            foreach ($section->elements as $element) {
                if ($element->type !== 'datablock') continue;

                $block = $context->getDataBlockById($element->ref_id);
                if (!$block) continue;

                foreach ($block->fields as $fieldDef) {
                    if ($fieldDef->type_field === DataBlockFieldType::FIXED) continue;
                    if (empty($fieldDef->label)) continue;

                    $formKey = "{$block->name}.{$fieldDef->name}";
                    if ($fieldDef->type_field === DataBlockFieldType::UNLINKED) {
                        $formKey = "_detached.{$formKey}";
                    }
                    $formKey = str_replace('.', '_', $formKey);

                    // Value to display
                    $value = $formData[$formKey] ?? '';

                    if (!empty($fieldDef->value_options)) {
                         $findLabel = function($val) use ($fieldDef) {
                            foreach ($fieldDef->value_options as $opt) {
                                if ($opt->value == $val) return $opt->text;
                            }
                            return $val; 
                        };
    
                        if (is_array($value)) {
                            $labels = array_map($findLabel, $value);
                            $displayValue = implode(', ', $labels);
                        } else {
                            $displayValue = $findLabel($value);
                        }
                    } else {
                        if (is_array($value)) {
                            $value = implode(', ', $value);
                        }
                        $displayValue = $value;
                    }
                    
                    // Format: "Label: Value"
                    $text .= "{$fieldDef->label} {$displayValue}\n";
                }
            }
            $text .= "\n";
        }
        
        return $text;
    }

    /**
     * Returns the raw SVG string for a given icon name. This is used for rendering icons in the rating field (e.g., stars). 
     * The method contains predefined SVGs for supported icons such as 'star_fill', 'star_empty', 'emoji_angry', and 'emoji_frown'.
     * 
     * @param string $name The name of the icon to retrieve (e.g., 'star_fill' or 'star')
     * @return string The raw SVG string corresponding to the requested icon, or an empty string
     */
    public static function getRawSvgIcon(string $name): string {
        $icons = [
            'star_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-star" viewBox="0 0 16 16"><path d="M2.866 14.85c-.078.444.36.791.746.593l4.39-2.256 4.389 2.256c.386.198.824-.149.746-.592l-.83-4.73 3.522-3.356c.33-.314.16-.888-.282-.95l-4.898-.696L8.465.792a.513.513 0 0 0-.927 0L5.354 5.12l-4.898.696c-.441.062-.612.636-.283.95l3.523 3.356-.83 4.73zm4.905-2.767-3.686 1.894.694-3.957a.56.56 0 0 0-.163-.505L1.71 6.745l4.052-.576a.53.53 0 0 0 .393-.288L8 2.223l1.847 3.658a.53.53 0 0 0 .393.288l4.052.575-2.906 2.77a.56.56 0 0 0-.163.506l.694 3.957-3.686-1.894a.5.5 0 0 0-.461 0z"/></svg>',
            'star_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-star-fill" viewBox="0 0 16 16"><path d="M3.612 15.443c-.386.198-.824-.149-.746-.592l.83-4.73L.173 6.765c-.329-.314-.158-.888.283-.95l4.898-.696L7.538.792c.197-.39.73-.39.927 0l2.184 4.327 4.898.696c.441.062.612.636.282.95l-3.522 3.356.83 4.73c.078.443-.36.79-.746.592L8 13.187l-4.389 2.256z"/></svg>',
            'emoji_angry_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-angry" viewBox="0 0 16 16"><path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"/><path d="M4.285 12.433a.5.5 0 0 0 .683-.183A3.5 3.5 0 0 1 8 10.5c1.295 0 2.426.703 3.032 1.75a.5.5 0 0 0 .866-.5A4.5 4.5 0 0 0 8 9.5a4.5 4.5 0 0 0-3.898 2.25.5.5 0 0 0 .183.683m6.991-8.38a.5.5 0 1 1 .448.894l-1.009.504c.176.27.285.64.285 1.049 0 .828-.448 1.5-1 1.5s-1-.672-1-1.5c0-.247.04-.48.11-.686a.502.502 0 0 1 .166-.761zm-6.552 0a.5.5 0 0 0-.448.894l1.009.504A1.94 1.94 0 0 0 5 6.5C5 7.328 5.448 8 6 8s1-.672 1-1.5c0-.247-.04-.48-.11-.686a.502.502 0 0 0-.166-.761z"/></svg>',
            'emoji_angry_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-angry-fill" viewBox="0 0 16 16"><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16M4.053 4.276a.5.5 0 0 1 .67-.223l2 1a.5.5 0 0 1 .166.76c.071.206.111.44.111.687C7 7.328 6.552 8 6 8s-1-.672-1-1.5c0-.408.109-.778.285-1.049l-1.009-.504a.5.5 0 0 1-.223-.67zm.232 8.157a.5.5 0 0 1-.183-.683A4.5 4.5 0 0 1 8 9.5a4.5 4.5 0 0 1 3.898 2.25.5.5 0 1 1-.866.5A3.5 3.5 0 0 0 8 10.5a3.5 3.5 0 0 0-3.032 1.75.5.5 0 0 1-.683.183M10 8c-.552 0-1-.672-1-1.5 0-.247.04-.48.11-.686a.502.502 0 0 1 .166-.761l2-1a.5.5 0 1 1 .448.894l-1.009.504c.176.27.285.64.285 1.049 0 .828-.448 1.5-1 1.5"/></svg>',
            'emoji_frown_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-frown" viewBox="0 0 16 16"><path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"/><path d="M4.285 12.433a.5.5 0 0 0 .683-.183A3.5 3.5 0 0 1 8 10.5c1.295 0 2.426.703 3.032 1.75a.5.5 0 0 0 .866-.5A4.5 4.5 0 0 0 8 9.5a4.5 4.5 0 0 0-3.898 2.25.5.5 0 0 0 .183.683M7 6.5C7 7.328 6.552 8 6 8s-1-.672-1-1.5S5.448 5 6 5s1 .672 1 1.5m4 0c0 .828-.448 1.5-1 1.5s-1-.672-1-1.5S9.448 5 10 5s1 .672 1 1.5"/></svg>',
            'emoji_frown_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-frown-fill" viewBox="0 0 16 16"><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16M7 6.5C7 7.328 6.552 8 6 8s-1-.672-1-1.5S5.448 5 6 5s1 .672 1 1.5m-2.715 5.933a.5.5 0 0 1-.183-.683A4.5 4.5 0 0 1 8 9.5a4.5 4.5 0 0 1 3.898 2.25.5.5 0 0 1-.866.5A3.5 3.5 0 0 0 8 10.5a3.5 3.5 0 0 0-3.032 1.75.5.5 0 0 1-.683.183M10 8c-.552 0-1-.672-1-1.5S9.448 5 10 5s1 .672 1 1.5S10.552 8 10 8"/></svg>',
            'emoji_neutral_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-neutral" viewBox="0 0 16 16"><path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"/><path d="M4 10.5a.5.5 0 0 0 .5.5h7a.5.5 0 0 0 0-1h-7a.5.5 0 0 0-.5.5m3-4C7 5.672 6.552 5 6 5s-1 .672-1 1.5S5.448 8 6 8s1-.672 1-1.5m4 0c0-.828-.448-1.5-1-1.5s-1 .672-1 1.5S9.448 8 10 8s1-.672 1-1.5"/></svg>',
            'emoji_neutral_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-neutral-fill" viewBox="0 0 16 16"><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16M7 6.5C7 7.328 6.552 8 6 8s-1-.672-1-1.5S5.448 5 6 5s1 .672 1 1.5m-3 4a.5.5 0 0 1 .5-.5h7a.5.5 0 0 1 0 1h-7a.5.5 0 0 1-.5-.5M10 8c-.552 0-1-.672-1-1.5S9.448 5 10 5s1 .672 1 1.5S10.552 8 10 8"/></svg>',
            'emoji_smile_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-smile" viewBox="0 0 16 16"><path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"/><path d="M4.285 9.567a.5.5 0 0 1 .683.183A3.5 3.5 0 0 0 8 11.5a3.5 3.5 0 0 0 3.032-1.75.5.5 0 1 1 .866.5A4.5 4.5 0 0 1 8 12.5a4.5 4.5 0 0 1-3.898-2.25.5.5 0 0 1 .183-.683M7 6.5C7 7.328 6.552 8 6 8s-1-.672-1-1.5S5.448 5 6 5s1 .672 1 1.5m4 0c0 .828-.448 1.5-1 1.5s-1-.672-1-1.5S9.448 5 10 5s1 .672 1 1.5"/></svg>',
            'emoji_smile_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-smile-fill" viewBox="0 0 16 16"><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16M7 6.5C7 7.328 6.552 8 6 8s-1-.672-1-1.5S5.448 5 6 5s1 .672 1 1.5M4.285 9.567a.5.5 0 0 1 .683.183A3.5 3.5 0 0 0 8 11.5a3.5 3.5 0 0 0 3.032-1.75.5.5 0 1 1 .866.5A4.5 4.5 0 0 1 8 12.5a4.5 4.5 0 0 1-3.898-2.25.5.5 0 0 1 .183-.683M10 8c-.552 0-1-.672-1-1.5S9.448 5 10 5s1 .672 1 1.5S10.552 8 10 8"/></svg>',
            'emoji_laugh_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-laughing" viewBox="0 0 16 16"><path d="M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16"/><path d="M12.331 9.5a1 1 0 0 1 0 1A5 5 0 0 1 8 13a5 5 0 0 1-4.33-2.5A1 1 0 0 1 4.535 9h6.93a1 1 0 0 1 .866.5M7 6.5c0 .828-.448 0-1 0s-1 .828-1 0S5.448 5 6 5s1 .672 1 1.5m4 0c0 .828-.448 0-1 0s-1 .828-1 0S9.448 5 10 5s1 .672 1 1.5"/></svg>',
            'emoji_laugh_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-emoji-laughing-fill" viewBox="0 0 16 16"><path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16M7 6.5c0 .501-.164.396-.415.235C6.42 6.629 6.218 6.5 6 6.5s-.42.13-.585.235C5.164 6.896 5 7 5 6.5 5 5.672 5.448 5 6 5s1 .672 1 1.5m5.331 3a1 1 0 0 1 0 1A5 5 0 0 1 8 13a5 5 0 0 1-4.33-2.5A1 1 0 0 1 4.535 9h6.93a1 1 0 0 1 .866.5m-1.746-2.765C10.42 6.629 10.218 6.5 10 6.5s-.42.13-.585.235C9.164 6.896 9 7 9 6.5c0-.828.448-1.5 1-1.5s1 .672 1 1.5c0 .501-.164.396-.415.235"/></svg>',
            'thumb_down_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-hand-thumbs-down" viewBox="0 0 16 16"><path d="M8.864 15.674c-.956.24-1.843-.484-1.908-1.42-.072-1.05-.23-2.015-.428-2.59-.125-.36-.479-1.012-1.04-1.638-.557-.624-1.282-1.179-2.131-1.41C2.685 8.432 2 7.85 2 7V3c0-.845.682-1.464 1.448-1.546 1.07-.113 1.564-.415 2.068-.723l.048-.029c.272-.166.578-.349.97-.484C6.931.08 7.395 0 8 0h3.5c.937 0 1.599.478 1.934 1.064.164.287.254.607.254.913 0 .152-.023.312-.077.464.201.262.38.577.488.9.11.33.172.762.004 1.15.069.13.12.268.159.403.077.27.113.567.113.856s-.036.586-.113.856c-.035.12-.08.244-.138.363.394.571.418 1.2.234 1.733-.206.592-.682 1.1-1.2 1.272-.847.283-1.803.276-2.516.211a10 10 0 0 1-.443-.05 9.36 9.36 0 0 1-.062 4.51c-.138.508-.55.848-1.012.964zM11.5 1H8c-.51 0-.863.068-1.14.163-.281.097-.506.229-.776.393l-.04.025c-.555.338-1.198.73-2.49.868-.333.035-.554.29-.554.55V7c0 .255.226.543.62.65 1.095.3 1.977.997 2.614 1.709.635.71 1.064 1.475 1.238 1.977.243.7.407 1.768.482 2.85.025.362.36.595.667.518l.262-.065c.16-.04.258-.144.288-.255a8.34 8.34 0 0 0-.145-4.726.5.5 0 0 1 .595-.643h.003l.014.004.058.013a9 9 0 0 0 1.036.157c.663.06 1.457.054 2.11-.163.175-.059.45-.301.57-.651.107-.308.087-.67-.266-1.021L12.793 7l.353-.354c.043-.042.105-.14.154-.315.048-.167.075-.37.075-.581s-.027-.414-.075-.581c-.05-.174-.111-.273-.154-.315l-.353-.354.353-.354c.047-.047.109-.176.005-.488a2.2 2.2 0 0 0-.505-.804l-.353-.354.353-.354c.006-.005.041-.05.041-.17a.9.9 0 0 0-.121-.415C12.4 1.272 12.063 1 11.5 1"/></svg>',
            'thumb_down_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-hand-thumbs-down-fill" viewBox="0 0 16 16"><path d="M6.956 14.534c.065.936.952 1.659 1.908 1.42l.261-.065a1.38 1.38 0 0 0 1.012-.965c.22-.816.533-2.512.062-4.51q.205.03.443.051c.713.065 1.669.071 2.516-.211.518-.173.994-.68 1.2-1.272a1.9 1.9 0 0 0-.234-1.734c.058-.118.103-.242.138-.362.077-.27.113-.568.113-.856 0-.29-.036-.586-.113-.857a2 2 0 0 0-.16-.403c.169-.387.107-.82-.003-1.149a3.2 3.2 0 0 0-.488-.9c.054-.153.076-.313.076-.465a1.86 1.86 0 0 0-.253-.912C13.1.757 12.437.28 11.5.28H8c-.605 0-1.07.08-1.466.217a4.8 4.8 0 0 0-.97.485l-.048.029c-.504.308-.999.61-2.068.723C2.682 1.815 2 2.434 2 3.279v4c0 .851.685 1.433 1.357 1.616.849.232 1.574.787 2.132 1.41.56.626.914 1.28 1.039 1.638.199.575.356 1.54.428 2.591"/></svg>',
            'thumb_up_empty' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-hand-thumbs-up" viewBox="0 0 16 16"><path d="M8.864.046C7.908-.193 7.02.53 6.956 1.466c-.072 1.051-.23 2.016-.428 2.59-.125.36-.479 1.013-1.04 1.639-.557.623-1.282 1.178-2.131 1.41C2.685 7.288 2 7.87 2 8.72v4.001c0 .845.682 1.464 1.448 1.545 1.07.114 1.564.415 2.068.723l.048.03c.272.165.578.348.97.484.397.136.861.217 1.466.217h3.5c.937 0 1.599-.477 1.934-1.064a1.86 1.86 0 0 0 .254-.912c0-.152-.023-.312-.077-.464.201-.263.38-.578.488-.901.11-.33.172-.762.004-1.149.069-.13.12-.269.159-.403.077-.27.113-.568.113-.857 0-.288-.036-.585-.113-.856a2 2 0 0 0-.138-.362 1.9 1.9 0 0 0 .234-1.734c-.206-.592-.682-1.1-1.2-1.272-.847-.282-1.803-.276-2.516-.211a10 10 0 0 0-.443.05 9.4 9.4 0 0 0-.062-4.509A1.38 1.38 0 0 0 9.125.111zM11.5 14.721H8c-.51 0-.863-.069-1.14-.164-.281-.097-.506-.228-.776-.393l-.04-.024c-.555-.339-1.198-.731-2.49-.868-.333-.036-.554-.29-.554-.55V8.72c0-.254.226-.543.62-.65 1.095-.3 1.977-.996 2.614-1.708.635-.71 1.064-1.475 1.238-1.978.243-.7.407-1.768.482-2.85.025-.362.36-.594.667-.518l.262.066c.16.04.258.143.288.255a8.34 8.34 0 0 1-.145 4.725.5.5 0 0 0 .595.644l.003-.001.014-.003.058-.014a9 9 0 0 1 1.036-.157c.663-.06 1.457-.054 2.11.164.175.058.45.3.57.65.107.308.087.67-.266 1.022l-.353.353.353.354c.043.043.105.141.154.315.048.167.075.37.075.581 0 .212-.027.414-.075.582-.05.174-.111.272-.154.315l-.353.353.353.354c.047.047.109.177.005.488a2.2 2.2 0 0 1-.505.805l-.353.353.353.354c.006.005.041.05.041.17a.9.9 0 0 1-.121.416c-.165.288-.503.56-1.066.56z"/></svg>',
            'thumb_up_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-hand-thumbs-up-fill" viewBox="0 0 16 16"><path d="M6.956 1.745C7.021.81 7.908.087 8.864.325l.261.066c.463.116.874.456 1.012.965.22.816.533 2.511.062 4.51a10 10 0 0 1 .443-.051c.713-.065 1.669-.072 2.516.21.518.173.994.681 1.2 1.273.184.532.16 1.162-.234 1.733q.086.18.138.363c.077.27.113.567.113.856s-.036.586-.113.856c-.039.135-.09.273-.16.404.169.387.107.819-.003 1.148a3.2 3.2 0 0 1-.488.901c.054.152.076.312.076.465 0 .305-.089.625-.253.912C13.1 15.522 12.437 16 11.5 16H8c-.605 0-1.07-.081-1.466-.218a4.8 4.8 0 0 1-.97-.484l-.048-.03c-.504-.307-.999-.609-2.068-.722C2.682 14.464 2 13.846 2 13V9c0-.85.685-1.432 1.357-1.615.849-.232 1.574-.787 2.132-1.41.56-.627.914-1.28 1.039-1.639.199-.575.356-1.539.428-2.59z"/></svg>',
            'circle_fill' => '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-circle-fill" viewBox="0 0 16 16"><circle cx="8" cy="8" r="8"/></svg>'
        ];
        return $icons[$name] ?? '';
    }

    /**
     * Render a basic HTML page using the form styles
     * @param ?FormConfig $config The form configuration to extract styles from (can be null for defaults)
     * @param string $title The title of the page (also used in the <title> tag)
     * @param string $titleHtml The HTML content for the title (can include styling, different from the plain text $title)
     * @param string $messageHtml The HTML content for the message/body of the page
     * 
     */
    public static function renderGenericResponseHtml(?FormConfig $config, string $title, string $titleHtml, string $messageHtml): void
    {
        // Clear the output buffer to remove previous warnings or errors
        while (ob_get_level()) {
            ob_end_clean();
        }
        
        // Avoid showing new errors or warnings
        ini_set('display_errors', 0);
        error_reporting(0);
        
        $theme = $config?->layout?->theme ?? new FormTheme();
        $fontFamily = $theme->font_family ?? 'sans-serif';
        $bgColor = $theme->page_bg_color ?? '#f8f9fa';
        $textColor = $theme->text_color ?? '#212529';
        $formBg = $theme->form_bg_color ?? '#ffffff';
        $primaryColor = $theme->primary_color ?? '#0d6efd';
        $customCss = $config?->layout?->custom_css ?? '';
        $customJs = $config?->layout?->custom_js ?? '';
        
        echo "
<!DOCTYPE html>
<html>
    <head>
        <meta charset='UTF-8'>
        <meta name='viewport' content='width=device-width, initial-scale=1.0'>
        <title>{$title}</title>
        <style>
            body { font-family: {$fontFamily}; background-color: {$bgColor}; color: {$textColor}; display: flex; justify-content: center; align-items: center; min-height: 100vh; margin: 0; }
            .message-card { background-color: {$formBg}; padding: 40px; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); max-width: 600px; width: 90%; text-align: center; }
            h1 { color: {$primaryColor}; margin-bottom: 20px; }
            .btn-primary { background-color: {$primaryColor}; border-color: {$primaryColor}; color: {$textColor}; }
            {$customCss}
        </style>
    </head>
    <body>
        <div class='message-card'>
            <h1>{$titleHtml}</h1>
            <div>{$messageHtml}</div>
        </div>";
        if (!empty($customJs)) { 
            echo "<script>" . $customJs . "</script>"; 
        }
        echo "
    </body>
</html>";
        
        sugar_cleanup(true);
    }

    /**
     * Render a basic HTML page using the form styles
     * @param ?FormConfig $config The form configuration to extract styles from (can be null for defaults)
     * @param string $title The title of the page (also used in the <title> tag and as a heading)
     * @param string $message The message/body of the page (plain text, will be escaped and newlines converted to <br>) 
     */
    public static function renderGenericResponse(?FormConfig $config, string $title, string $message): void
    {
        $title = htmlspecialchars($title);
        $message = nl2br(htmlspecialchars($message));

        self::renderGenericResponseHtml($config, $title, $title, $message);
    }

    /**
     * Render an error page with a back button using form styles
     * @param ?FormConfig $formConfig The form configuration to extract styles from (can be null for defaults)
     * @param string $title The title of the error page
     * @param array $errors An array of error messages to display
     */
    public static function renderErrorWithBackButton(?FormConfig $formConfig, string $title, array $errors): void
    {
        $title = htmlspecialchars($title);

        // Generate the HTML list of errors
        $errorListHtml = "<ul>";
        foreach ($errors as $err) {
            $errorListHtml .= "<li>" . htmlspecialchars($err, ENT_QUOTES, 'UTF-8') . "</li>";
        }
        $errorListHtml .= "</ul>";

        // Add the Javascript History Back button
        $backButtonHtml = "
        <div style='margin-top: 20px; text-align: center;'>
            <button onclick='window.history.back()' class='btn btn-primary' style='padding: 10px 20px; font-size: 16px; cursor: pointer;'>
                " . translate('LBL_BUTTON_GO_BACK_AND_FIX', 'stic_AWF_Responses') . "
            </button>
        </div>";

        // Combine the error list and back button into the final HTML
        $finalHtml = $errorListHtml . $backButtonHtml;

        self::renderGenericResponseHtml($formConfig, $title, $title, $finalHtml);
    }


    /**
     * Render a basic HTML page to respond to Spam (avoiding extra processing)
     */
    public static function renderGenericSpamResponse()
    {
        $title = htmlspecialchars(translate('LBL_THEME_RECEIPT_FORM_TITLE_VALUE', 'stic_AWF_Forms'));
        $message = nl2br(htmlspecialchars(translate('LBL_THEME_RECEIPT_FORM_TEXT_VALUE', 'stic_AWF_Forms')));

        $fontFamily = 'sans-serif';
        $bgColor = '#f8f9fa';
        $textColor = '#212529';
        $formBg = '#ffffff';
        $primaryColor = '#0d6efd';
        
        echo "
<!DOCTYPE html>
<html>
    <head>
        <meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1.0'>
        <title>{$title}</title>
        <style>
            body { font-family: {$fontFamily}; background-color: {$bgColor}; color: {$textColor}; display: flex; justify-content: center; align-items: center; min-height: 100vh; margin: 0; }
            .message-card { background-color: {$formBg}; padding: 40px; border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); max-width: 600px; width: 90%; text-align: center; }
            h1 { color: {$primaryColor}; margin-bottom: 20px; }
        </style>
    </head>
    <body>
        <div class='message-card'>
            <h1>{$title}</h1>
            <div>{$message}</div>
        </div>
    </body>
</html>";
        sugar_cleanup(true);
    }

    /** 
     * Method to send an email based on a template 
     * 
     * @param string $toAddress Recipient's email address. 
     * @param string $templateId ID of the mail template to use. 
     * @param ExecutionContext $context The execution context of the form. 
     * @param ?SugarBean $parentBeanForArchive (Optional) Parent bean to archive mail. 
     * @throws \Exception If there are errors in the submission or in the template. 
     */
    public static function sendTemplateEmail(
        string $toAddress, 
        string $templateId, 
        ExecutionContext $context, 
        ?SugarBean $parentBeanForArchive = null
    ): void 
    {
        // Load the email template
        /** @var EmailTemplate $emailTemplate */
        $emailTemplate = BeanFactory::getBean('EmailTemplates', $templateId);
        if (!$emailTemplate) {
            throw new \Exception("Email template not found: '{$templateId}'.");
        }

        // Get attachments from the template
        $attachments = $emailTemplate->getAttachments() ?: [];


        // Prepare the array of Beans for the parser
        $beansForTemplate = [];
        // The main Context Bean (the recipient)
        if ($parentBeanForArchive && isset($parentBeanForArchive->module_dir)) {
            $beansForTemplate[] = $parentBeanForArchive;
        }
        // The Response Bean (to access $stic_awf_responses_html_summary)
        if ($context->responseBean) {
            $beansForTemplate[] = $context->responseBean;
        }

        // Add all other generated Beans from the form context (DataBlocks)
        // We only add them if a bean of that module hasn't been added yet to avoid unpredictable overwrites
        $loadedModules = array_map(function($b) { return $b->module_dir; }, $beansForTemplate);
        foreach ($context->actionResults as $result) {
            foreach ($result->modifiedBeans as $modBean) {
                // Ignore Metadata entries wich represent action logs not CRM records
                if ($modBean->modificationType === BeanModificationType::METADATA) continue;

                // Ignore beans from modules that are already loaded to avoid unpredictable overwrites in the template parser
                if (in_array($modBean->moduleName, $loadedModules)) continue;

                $loadedBean = $modBean->getBean();
                if ($loadedBean) {
                    $beansForTemplate[] = $loadedBean;
                    $loadedModules[] = $modBean->moduleName;
                }
            }
        }

        // Parse the email template using all relevant beans
        require_once 'SticInclude/Utils.php';
        $parsedOutput = SticUtils::parseEmailTemplate($templateId, $beansForTemplate);
        if (empty($parsedOutput)) {
            throw new \Exception("Email template empty after parsing: '{$templateId}'.");
        }
        
        $subject = $parsedOutput['subject'] ?? '';
        $bodyHtml = $parsedOutput['body_html'] ?? '';
        $bodyText = $parsedOutput['body'] ?? '';
        // Determine final body and decode HTML entities safely
        $body = $bodyHtml;
        if (!empty($bodyHtml)) {
            $body = html_entity_decode($bodyHtml, ENT_QUOTES, 'UTF-8');
        } else {
            $body = nl2br(html_entity_decode($bodyText, ENT_QUOTES, 'UTF-8'));
        }

        // Initialize the mailer
        require_once 'include/SugarPHPMailer.php';
        $mailer = new SugarPHPMailer();
        $mailer->prepForOutbound();
        $mailer->setMailerForSystem();

        // Get the system email address
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();
        $systemFromAddress = $admin->settings['notify_fromaddress'] ?? '';
        $systemFromName = $admin->settings['notify_fromname'] ?? '';
        if (empty($systemFromAddress)) {
            throw new \Exception("System email address not configured.");
        }

        $mailer->From = $systemFromAddress;
        $mailer->FromName = $systemFromName;

        // Add recipient and content
        $mailer->addAddress($toAddress);
        $mailer->Subject = $subject;
        $mailer->Body = $body;
        $mailer->isHTML(true); 
        $mailer->CharSet = 'UTF-8';

        // Process and add attachments
        $mailer->handleAttachments($attachments);

        // Send the email
        if (!$mailer->send()) {
            throw new \Exception("Error sending email to {$toAddress}: " . $mailer->ErrorInfo);
        }

        // After sending, archive the email in the CRM
        // If the parent is not specified, we use the context
        if ($parentBeanForArchive != null) {
            try {
                self::archiveEmail($subject, $body, $systemFromAddress, $toAddress, $parentBeanForArchive, $attachments);
            } catch (\Exception $e) {
                // If the archive fails, we do not stop the process but we record it in the log
                $GLOBALS['log']->error('Line '.__LINE__.': '.__METHOD__.':  Email sent but error archiving: ' . $e->getMessage());
            }
        }
    }

    /** 
     * Archive the email sent as a record in the Emails module, linked to the parent bean. 
     * It also duplicates email attachments. 
     * 
     * @param string $subject The subject of the mail. 
     * @param string $body The body of the email in HTML. 
     * @param string $from The sender's email address. 
     * @param string $to The recipient's email address. 
     * @param SugarBean $parentBean The bean to which the archived mail will be linked. 
     * @param array $attachments List of Note objects that represent the original attachments. 
     * @throws \Exception If there is an error during the archiving process. 
     */
    private static function archiveEmail(string $subject, string $body, string $from, string $to, SugarBean $parentBean, array $attachments): void
    {
        /** @var Email $emailBean */
        $emailBean = BeanFactory::newBean('Emails');

        // Basic data
        $emailBean->name = $subject;
        $emailBean->date_sent_received = TimeDate::getInstance()->nowDb();
        $emailBean->type = 'out';
        $emailBean->status = 'sent';
        $emailBean->assigned_user_id = $GLOBALS['current_user']->id ?? '1';

        // Addresses
        $emailBean->from_addr = $from;
        $emailBean->to_addrs = $to;

        // Content
        $emailBean->description = strip_tags($body);
        $emailBean->description_html = $body;

        // Relationship with parent bean
        $emailBean->parent_type = $parentBean->module_dir;
        $emailBean->parent_id = $parentBean->id;

        // Email saving
        $emailBean->save();

        // Link email to parent bean
        if ($parentBean->load_relationship('emails')) {
            $parentBean->emails->add($emailBean->id);
        }

        // Duplication of attachments
        foreach ($attachments as $originalNote) {
            $newNote = BeanFactory::newBean('Notes');
            $newNote->id = create_guid();
            $newNote->new_with_id = true;

            // Link with archived email
            $newNote->parent_id = $emailBean->id;
            $newNote->parent_type = 'Emails';

            // Copy the original Note properties
            $newNote->name = $originalNote->name;
            $newNote->filename = $originalNote->filename;
            $newNote->file_mime_type = $originalNote->file_mime_type;
            
            // Copy the physical file
            $source = "upload://{$originalNote->id}";
            $dest = "upload://{$newNote->id}";

            if (file_exists($source) && copy($source, $dest)) {
                $newNote->save();
            } else {
                $GLOBALS['log']->warn('Line '.__LINE__.': '.__METHOD__.":  Cannot copy attachment file from {$source} to {$dest}.");
            }
        }
    }


    /** 
     * Converts a hexadecimal color to its RGB representation. 
     * @param string $hex The color in hexadecimal format (eg: "#FF5733" or "FF5733"). 
     * @return string The color in RGB format separated by commas (eg: "255, 87, 51"). 
     */
    public static function hex2rgb(string $hex): string 
    {
        $hex = str_replace("#", "", $hex);
    
        if (strlen($hex) == 3) {
            $r = hexdec(substr($hex, 0, 1) . substr($hex, 0, 1));
            $g = hexdec(substr($hex, 1, 1) . substr($hex, 1, 1));
            $b = hexdec(substr($hex, 2, 1) . substr($hex, 2, 1));
        } else {
            $r = hexdec(substr($hex, 0, 2));
            $g = hexdec(substr($hex, 2, 2));
            $b = hexdec(substr($hex, 4, 2));
        }
        
        return "{$r}, {$g}, {$b}"; // Returns "255, 87, 51"
    }

    /** 
     * Parse a link in Markdown format [text](url) and convert it to an HTML anchor tag.
     * We allow a whitelist of URL schemes (http, https, mailto, tel, sms) and also relative links or anchors starting with /, ? or #.
     * For security reasons, all links will have target="_blank" and rel="noopener noreferrer" to prevent tabnabbing. 
     * @param string $text The text to parse 
     * @return string The parsed text (in html) 
     */
    public static function parseAnchorMarkdown(string $text): string {
        if (empty($text)) return '';

        // Whitelist: we allow http://, https://, mailto:, tel:, sms:
        // We also allow relative links or anchors that start with /, ? or #.
        $pattern = '/\[([^\]]+)\]\(((?:https?:\/\/|mailto:|tel:|sms:|[#\/\?])[^\)]+)\)/i';
        $replacement = '<a href="$2" target="_blank" rel="noopener noreferrer" class="awf-link" title="$1">$1</a>';
        
        $html = preg_replace($pattern, $replacement, $text);

        return nl2br($html);
    }

    /**
     * Evaluates an array of conditions against the provided form data.
     * Assumes an implicit AND between all conditions.
     * @param array|null $conditions Array of condition DTOs (field_name, operator, value)
     * @param array $formData The submitted form data
     * @return bool True if all conditions are met or if no conditions exist, false otherwise
     */
    public static function evaluateConditions(?array $conditions, array $formData): bool {
        if (empty($conditions)) {
            return true;
        }

        foreach ($conditions as $cond) {
            if (empty($cond->field_name)) {
                continue; // Prevent errors if an empty condition slipped through
            }

            $fieldKey = $cond->field_name; // Ex: "Contact0.newsletter"
            $phpKey = str_replace('.', '_', $fieldKey); // Convert field name to PHP key format
            $expectedValue = $cond->value;
            $submittedValue = $formData[$phpKey] ?? null;

            $isMatch = is_array($submittedValue) 
                ? in_array($expectedValue, $submittedValue)
                : ($submittedValue == $expectedValue);

            // Evaluate based on the operator
            switch ($cond->operator) {
                case 'Equal_To':
                    if (!$isMatch) return false;
                    break;
                case 'Not_Equal_To':
                    if ($isMatch) return false;
                    break;
                // Future operators (>, <, IN, etc.) go here
                default:
                    return false; // Unknown operator fails safely
            }
        }

        return true; // All conditions passed
    }
}
