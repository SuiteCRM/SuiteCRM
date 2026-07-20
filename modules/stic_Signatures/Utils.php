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

/**
 * Utility class for managing signature-related functionalities in SinergiaCRM.
 * This includes retrieving module relationships, handling emailable modules,
 * populating signer path lists, identifying signature signers, and parsing PDF templates.
 */
class stic_SignaturesUtils
{
    /**
     * Retrieves the relationships and fields for a given SuiteCRM module.
     * This function fetches a module's own reportable fields and also
     * identifies and lists reportable fields from related modules.
     * The field names are formatted as '$[table_name]:[field_name]' or
     * '$[relationship_name]:[field_name]' for clarity in reporting or display.
     *
     * @param string $moduleName The name of the module (e.g., 'Accounts', 'Contacts').
     * @param string $format The format of the output, either 'raw' (associative array) or 'json'.
     * @return array|string An associative array or JSON string containing module relationships and field options.
     */
    public static function getModuleRelationships($moduleName, $format = 'raw')
    {
        global $beanList;

        $options_array = ['' => ''];
        $mod_options_array = [];

        // Validate module existence
        if (!isset($beanList[$moduleName])) {
            return [];
        }

        // Instantiate the module
        $module = new $beanList[$moduleName]();

        // Process main module's fields
        foreach ($module->field_defs as $name => $arr) {
            // Exclude 'id' and 'link' type fields from the reportable list
            if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || (isset($arr['type']) && $arr['type'] == 'id') || (isset($arr['type']) && $arr['type'] == 'link'))) {
                // Check if the field is explicitly marked as non-reportable
                if (!isset($arr['reportable']) || $arr['reportable']) {
                    $options_array['$' . $module->table_name . ':' . $name] = translate($arr['vname'] ?? '', $module->module_dir);
                }
            }
        }

        $options = $options_array;

        // Add the main module to the options array
        $mod_options_array[$module->module_dir] = translate('LBL_MODULE_NAME', $module->module_dir);


        $fmod_options_array = [];

        // Process related module fields (non-db relates)
        foreach ($module->field_defs as $module_name => $module_arr) {
            if (isset($module_arr['type']) && $module_arr['type'] == 'relate' && isset($module_arr['source']) && $module_arr['source'] == 'non-db') {
                $options_array = ['' => ''];

                // Skip 'EmailAddress' module as it's not a primary related module for reporting
                if (isset($module_arr['module']) && $module_arr['module'] !== '' && $module_arr['module'] !== 'EmailAddress') {
                    $relate_module_class_name = $beanList[$module_arr['module']];

                    $relate_module = null;
                    if (!is_null($relate_module_class_name)) {
                        $relate_module = new $relate_module_class_name();
                    }

                    $options = $options_array;

                    // Build module path and label if not 'LBL_DELETED'
                    if ($module_arr['vname'] !== 'LBL_DELETED') {
                        $options_array['$' . $module->table_name . ':' . $name] = translate($module_arr['vname'], $module->module_dir);
                        $module_path = $module_arr['module'] . ':' . ($module_arr['id_name'] ?? $module_arr['link']);
                        $fmod_options_array[$module_path] = translate($relate_module->module_dir) . ' : ' . translate($module_arr['vname'], $module->module_dir);
                    }
                }
            }
        }

        // Finalize and sort module options
        array_multisort($fmod_options_array, SORT_ASC, $fmod_options_array);
        $mod_options_array = array_merge($mod_options_array, $fmod_options_array);

        $moduleOptions = $mod_options_array;

        if ($format === 'json') {
            // Convert to JSON format if requested
            $moduleOptions = json_encode($moduleOptions);
            echo $moduleOptions;
            die();
        } else {
            // Otherwise, return as an associative array
            return $moduleOptions;
        }
    }

    /**
     * Retrieves related modules that are emailable (i.e., modules that can have email addresses).
     * It checks if related modules are instances of the 'Person' class.
     *
     * @param string $mainModule The main module to check for related emailable modules.
     * @return array An associative array of related emailable modules with their labels (beanPath => beanLabel).
     */
    public static function getRelatedEmailableModules($mainModule)
    {
        global $beanFiles, $beanList;
        $emailableModules = [];

        $relatedModules = self::getModuleRelationships($mainModule);
        foreach ($relatedModules as $beanPath => $beanLabel) {
            // Extract the bean name from the path (e.g., 'Contacts:contact_id' -> 'Contacts')
            $beanName = explode(':', $beanPath)[0];

            if (isset($beanList[$beanName]) && isset($beanFiles[$beanList[$beanName]])) {
                require_once $beanFiles[$beanList[$beanName]];
                $obj = new $beanList[$beanName]();
                
                // Check if the related module is a 'Person' type, which implies it can be 'emailable'
                if ($obj instanceof Person) {
                    $emailableModules[$beanPath] = $beanLabel;
                }
            }
        }
        
        asort($emailableModules);
        return $emailableModules;
    }

    /**
     * Populates the global `app_list_strings` with emailable related modules
     * for the 'signer_path' dropdown. Leads and non-assigned 'Users' are excluded.
     *
     * @param string $moduleName The name of the module for which to get emailable related modules.
     * @return void
     */
    public static function populateSignerPathListString($moduleName)
    {
        global $app_list_strings;
        
        // Get emailable related modules and populate the dropdown
        require_once 'modules/stic_Signatures/Utils.php';
        $relatedModules = stic_SignaturesUtils::getRelatedEmailableModules($moduleName);
        
        $app_list_strings['stic_signatures_signer_path_list'][''] = '';
        
        // Populate the list, applying exclusions
        foreach ($relatedModules as $key => $value) {
            $module = explode(':', $key)[0];
            $moduleField = explode(':', $key)[1] ?? '';
            
            // Exclude Leads module as signers
            if ($module == 'Leads') {
                continue;
            }
           
            $app_list_strings['stic_signatures_signer_path_list'][$key] = $value;
        }
    }

    /**
     * Retrieves the signers for a given signature ID and main module IDs.
     * It handles direct relationships and relate fields, and optionally retrieves authorized signers.
     *
     * @param string $signatureId The ID of the signature.
     * @param array|string $mainModuleIds An array or single value of main module IDs.
     * @return array An array of unique signer data (id => ['module', 'sourceModule', 'sourceId', 'signerPath', 'onBehalfOfId', 'id', 'name', 'email', 'phone']).
     */
    public static function getSignatureSigners($signatureId, $mainModuleIds)
    {
        global $app_list_strings;
        
        if (empty($signatureId) || empty($mainModuleIds)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Signature [{$signatureId}] or Main Module ID [{$mainModuleIds}] is empty.");
            return [];
        }
        
        // Initialize an empty array to hold unique signer IDs
        $signersIdList = [];

        // Ensure $mainModuleIds is an array
        $mainModuleIds = is_array($mainModuleIds) ? $mainModuleIds : [$mainModuleIds];

        // Load the signature bean and check if it exists
        $signatureBean = BeanFactory::getBean('stic_Signatures', $signatureId);
        if (empty($signatureBean)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Signature Bean for ID [{$signatureId}] is empty.");
            return [];
        }

        $mainModule = $signatureBean->main_module;
        $signerPath = $signatureBean->signer_path;
        $signerModule = explode(':', $signerPath)[0];
        // Handle case where $signerPath is just the module name (direct bean)
        $signerPath = explode(':', $signerPath)[1] ?? $signerPath; 

        // Process each main module ID to find related signers
        foreach ($mainModuleIds as $mainModuleId) {
            $mainModuleBean = BeanFactory::getBean($mainModule, $mainModuleId);
            if (empty($mainModuleBean)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Main Module Bean for ID [{$mainModuleId}] is empty.");
                continue;
            }

            $signers = [];
            // Try to retrieve linked beans based on relationships
            if ($signerPath !== $signerModule) {
                $signers = $mainModuleBean->get_linked_beans($signerPath, $signerModule);
            } else {
                // If the signerPath is the same as the signerModule, the signer *is* the main module bean itself.
                $signers = [$mainModuleBean];
            }

            // Fallback for relate fields where get_linked_beans might not apply directly
            if (empty($signers) && !empty($mainModuleBean->$signerPath)) {
                $relatedId = $mainModuleBean->$signerPath;

                $relatedBean = is_string($relatedId) ? BeanFactory::getBean($signerModule, $relatedId) : '';
                if (!empty($relatedBean)) {
                    $signers[] = $relatedBean;
                } else {
                    // Log error if related bean is not found
                    SugarApplication::appendErrorMessage("<p class='msg-error'> " . translate('LBL_SIGNERS_NOT_ADDED_NOT_EXISTS', 'stic_Signatures') . " : [" . $app_list_strings['moduleListSingular'][$mainModuleBean->object_name] . "-{$mainModuleBean->name}]</p>");

                }
            }

            // Check if authorized signer option is enabled (on behalf of a contact).
            $useAuthorizedSigner = $signatureBean->on_behalf_of ?? false;
            $onBehalfOfId = $signers[0]->id ?? '';
            
            // If using authorized signers, fetch them and replace/merge the current signers list
            if ($useAuthorizedSigner == true && $signerModule === 'Contacts' && !empty($onBehalfOfId)) {
                $signers = self::getAuthorizedSigner($onBehalfOfId);
            }

            // If no signers found, log an error and continue to the next main module ID
            if (empty($signers)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": No signers found for Main Module ID [{$mainModuleId}] and Signer Path [{$signerPath}].");
                continue;
            }

            // Process each signer and add to the unique list
            foreach ($signers as $signer) {
                // Ensure signer ID is unique in the list
                if (!isset($signersIdList[$signer->id])) {
                    $signersIdList[$signer->id] = [
                        'module' => $signerModule,
                        'sourceModule' => $mainModule,
                        'sourceId' => $mainModuleId,
                        'sourceName' => $mainModuleBean->name,
                        'signerPath' => $signerPath,
                        'onBehalfOfId' => $onBehalfOfId,
                        'id' => $signer->id,
                        'name' => $signer->name,
                        'email' => $signer->email1 ?? '',
                        'phone' => $signer->phone_mobile ?? '', // Prioritize mobile phone
                    ];
                }
            }
        }
        
        return $signersIdList;
    }

    /**
     * Retrieves the parsed HTML template for a given signer ID.
     * This function fetches the PDF template associated with the signature,
     * populates it with data from the source module related to the signer,
     * and returns the resulting HTML header, content, and footer.
     *
     * @param string $signerId The ID of the signer.
     * @return array An associative array containing the parsed 'header', 'converted' (content), and 'footer'.
     */
    public static function getParsedTemplate($signerId)
    {
        require_once 'SticInclude/Utils.php';
        // Use common functions for PDF generation
        require_once 'custom/modules/AOS_PDF_Templates/SticGeneratePdfFunctions.php';

        $signerBean = BeanFactory::getBean('stic_Signers', $signerId);
        // Ensure signerBean exists
        if (!$signerBean || empty($signerBean->id)) {
            sugar_die("Invalid Signer ID provided: {$signerId}");
        }

        // Retrieve the related Signature Bean
        $signatureBean = SticUtils::getRelatedBeanObject($signerBean, 'stic_signatures_stic_signers');
        // Ensure signatureBean exists
        if (!$signatureBean || empty($signatureBean->id)) {
            sugar_die("Invalid Signature Bean related to Signer ID: {$signerId}");
        }

        // Retrieve the related PDF Template Bean
        $pdfTemplateBean = BeanFactory::getBean('AOS_PDF_Templates', $signatureBean->pdftemplate_id_c ?? '');
        // Ensure pdfTemplateBean exists
        if (!$pdfTemplateBean || empty($pdfTemplateBean->id)) {
            sugar_die("Invalid PDF Template Bean related to Signature ID: {$signatureBean->id}");
        }

        // Retrieve the Source Module Bean (e.g., Accounts, Contacts)
        $sourceModuleBean = BeanFactory::getBean($signatureBean->main_module ?? '', $signerBean->record_id ?? '');
        // Ensure sourceModuleBean exists
        if (!$sourceModuleBean || empty($sourceModuleBean->id)) {
            sugar_die("Invalid Source Module Bean related to Signer ID: {$signerId} (Module: {$signatureBean->main_module}, Record ID: {$signerBean->record_id})");
        }
        
        $authorizedSourceModuleArray = [];
        // Handle case for authorized signers (on behalf of a contact)
        if ($signerBean->parent_type === 'Contacts' && $signatureBean->on_behalf_of == 1) {
            foreach (self::getAuthorizedSigner($signerBean->contact_id_c ?? '') as $auth) {
                if ($signerBean->parent_id == $auth->id) {
                    $authorizedSourceModuleArray['Contacts'] = $auth->id;
                    break;
                }
            }
        }

        require_once 'modules/AOS_PDF_Templates/templateParser.php';

        $bean = $sourceModuleBean;
        $templateBean = $pdfTemplateBean;

        // Define the patterns used to clean the template HTML code
        $search = [
            '/<script[^>]*?>.*?<\/script>/si', // Strip out javascript
            '/<[\/\!]*?[^<>]*?>/si', // Strip out HTML tags
            '/([\r\n])[\s]+/', // Strip out white space
            '/&(quot|#34);/i', // Replace HTML entities
            '/&(amp|#38);/i',
            '/&(lt|#60);/i',
            '/&(gt|#62);/i',
            '/&(nbsp|#160);/i',
            '/&(iexcl|#161);/i',
            '/<address[^>]*?>/si',
            '/&(apos|#0*39);/',
            '/&#(\d+);/',
        ];

        $replace = [
            '',
            '',
            '\1',
            '"',
            '&',
            '<',
            '>',
            ' ',
            chr(161),
            '<br>',
            "'",
            'chr(%1)',
        ];

        // Clean the template content (header, footer, description)
        $header = preg_replace($search, $replace, $templateBean->pdfheader);
        $footer = preg_replace($search, $replace, $templateBean->pdffooter);
        $text = preg_replace($search, $replace, $templateBean->description);
        
        // Replace custom pagebreak tag
        $text = str_replace("<p><pagebreak /></p>", "<pagebreak />", $text);
        
        // Replace {DATE} placeholder with current date formatted as specified in the template
        $text = preg_replace_callback(
            '/\{DATE\s+(.*?)\}/',
            function ($matches) {
                return date($matches[1]);
            },
            $text
        );

        // Handle AOS (Advanced OpenSales) specific modules for line items
        if (str_starts_with($_REQUEST['module'], "AOS_")) {
            $variableName = strtolower($bean->module_dir);
            $lineItemsGroups = [];
            $lineItems = [];

            // Query to fetch line items and groups
            $sql = "SELECT pg.id, pg.product_id, pg.group_id FROM aos_products_quotes pg LEFT JOIN aos_line_item_groups lig ON pg.group_id = lig.id WHERE pg.parent_type = '" . $bean->object_name . "' AND pg.parent_id = '" . $bean->id . "' AND pg.deleted = 0 ORDER BY lig.number ASC, pg.number ASC";
            $res = $bean->db->query($sql);
            while ($row = $bean->db->fetchByAssoc($res)) {
                $lineItemsGroups[$row['group_id']][$row['id']] = $row['product_id'];
                $lineItems[$row['id']] = $row['product_id'];
            }

            // Backward compatibility for related beans in AOS modules
            $object_arr = [];
            if (isset($bean->billing_account_id)) {
                $object_arr['Accounts'] = $bean->billing_account_id;
            }
            if (isset($bean->billing_contact_id)) {
                $object_arr['Contacts'] = $bean->billing_contact_id;
            }
            if (isset($bean->assigned_user_id)) {
                $object_arr['Users'] = $bean->assigned_user_id;
            }
            if (isset($bean->currency_id)) {
                $object_arr['Currencies'] = $bean->currency_id;
            }

            // Replace specific AOS variables with dynamic ones based on the module name
            $text = str_replace("\$aos_quotes", "\$" . $variableName, $text);
            $text = str_replace("\$aos_invoices", "\$" . $variableName, $text);
            $text = str_replace("\$total_amt", "\$" . $variableName . "_total_amt", $text);
            $text = str_replace("\$discount_amount", "\$" . $variableName . "_discount_amount", $text);
            $text = str_replace("\$subtotal_amount", "\$" . $variableName . "_subtotal_amount", $text);
            $text = str_replace("\$tax_amount", "\$" . $variableName . "_tax_amount", $text);
            $text = str_replace("\$shipping_amount", "\$" . $variableName . "_shipping_amount", $text);
            $text = str_replace("\$total_amount", "\$" . $variableName . "_total_amount", $text);

            // Populate group lines (products/services) in the template
            $text = populate_group_lines($text, $lineItemsGroups, $lineItems);
        }

        // The parse_template function requires an array of beans
        $beanArray = [$bean->module_dir => $bean->id];

        // Enable custom parsing in templates
        if (file_exists('custom/modules/stic_Signatures/customParseTemplate.php')){
            require_once 'custom/modules/stic_Signatures/customParseTemplate.php';
        }

        // First parse: Parse the template using the record's data
        $converted = templateParser::parse_template($text, $beanArray);
        $header = templateParser::parse_template($header, $beanArray);
        $footer = templateParser::parse_template($footer, $beanArray);

        // Replace $authorized_ marks with $contacts_, to re-parse later with authorized contact data
        $converted = str_replace('$authorized_signer_', '$contacts_', $converted);
        $header = str_replace('$authorized_signer_', '$contacts_', $header);
        $footer = str_replace('$authorized_signer_', '$contacts_', $footer);

        // Second parse: using authorized contact data (if available)
        $converted = templateParser::parse_template($converted, $authorizedSourceModuleArray);
        $header = templateParser::parse_template($header, $authorizedSourceModuleArray);
        $footer = templateParser::parse_template($footer, $authorizedSourceModuleArray);

        // Replace last break lines by HTML tags (to handle remaining newlines as <br /> for display)
        $printable = str_replace("\n", "<br />", $converted);

        return ['header' => $header, 'converted' => $converted, 'footer' => $footer];
    }

    /**
     * Generates a verification code for a signed PDF.
     * The verification code is created by computing the SHA-256 hash of the signed PDF file.
     *
     * @param string $pdfPath The file system path to the signed PDF file.
     * @return string The SHA-256 hash of the signed PDF file, or an empty string if the file does not exist.
     */
    public static function getVerificationCodeForSignedPdf($pdfPath)
    {
        if (!file_exists($pdfPath)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Signed PDF file does not exist for path: {$pdfPath}");
            return '';
        }
        
        $verificationCode = hash_file('sha256', $pdfPath);
        return $verificationCode;
    }

    /**
     * Retrieves authorized signers associated with a given contact ID.
     * Authorized signers are linked to the contact via 'stic_Personal_Environment' beans 
     * where the 'authorized_signer' field is set to true.
     *
     * @param string $contactId The ID of the contact.
     * @return array An array of authorized signer contact beans.
     */
    public static function getAuthorizedSigner($contactId)
    {
        // Initialize an empty array to hold authorized signers
        $authorizedSigners = [];

        // Load the contact bean, check if ID is valid
        $contactBean = null;
        if (is_string($contactId) && !empty($contactId)) {
            $contactBean = BeanFactory::getBean('Contacts', $contactId);
        }
        
        // Ensure contactBean exists
        if ($contactBean && !empty($contactBean->id)) {
            // Find 'stic_Personal_Environment' beans linked to the contact and marked as 'authorized_signer = 1'
            $environment = $contactBean->get_linked_beans(
                'stic_personal_environment_contacts',
                'stic_Personal_Environment',
                '',
                0,
                0,
                0,
                'authorized_signer = 1'
            );

            // Loop through each environment and collect authorized signers (Contacts)
            foreach ($environment as $env) {
                foreach ($env->get_linked_beans('stic_personal_environment_contacts_1', 'Contacts') as $authSigner) {
                    $authorizedSigners[] = $authSigner;
                }
            }
        } else {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Contact Bean for ID [{$contactId}] is empty.");
        }
        
        return $authorizedSigners;
    }

    /**
     * Generates an acceptance image with provided text lines.
     * This image is used to represent acceptance in button mode signatures.
     * The image is generated based on a background template and includes text overlaid.
     *
     * @param array $lines An array of text lines to include in the acceptance image. Default is ['No text received'].
     * @return string A Base64-encoded PNG image string ('data:image/png;base64,...').
     */
    public static function generateAcceptImage($lines = ['No text received'])
    {
        // Path to the background image used for the acceptance image
        $background_image_path = 'themes/SuiteP/images/SignaturePlaceholder.png';

        // Path to the font file used for rendering text
        $font_path = 'themes/SuiteP/fonts/Lato-Italic.ttf';

        // Start output buffering to capture image data
        ob_start();

        // Check if the background image exists
        if (!file_exists($background_image_path)) {
            // If the background image does not exist, create a simple placeholder image with an error message
            $width = 200;
            $height = 50;
            $image = imagecreatetruecolor($width, $height);
            $bg_color = imagecolorallocate($image, 255, 200, 200); // Light red background for error
            imagefill($image, 0, 0, $bg_color);
            $text_color = imagecolorallocate($image, 255, 0, 0); // Red text
            imagestring($image, 3, 10, 10, "Error: Background not found!", $text_color);
            imagestring($image, 3, 10, 30, join(' ', $lines), $text_color);
        } else {
            // Create the image from the background PNG file
            $image = imagecreatefrompng($background_image_path);

            // Get image dimensions
            $width = imagesx($image);
            $height = imagesy($image);

            // Create a semi-transparent white overlay
            $attenuated_color = imagecolorallocatealpha($image, 255, 255, 255, 20);

            // Fill the image with the semi-transparent overlay
            imagefilledrectangle($image, 0, 0, $width, $height, $attenuated_color);

            // Set the text color (dark blue)
            $text_color = imagecolorallocate($image, 0, 51, 102);

            // Check if the font file exists
            if (!file_exists($font_path)) {
                // If the font file does not exist, use a built-in font
                imagestring($image, 5, 5, 15, join(' ', $lines), $text_color);
            } else {
                // If the font file exists, use TrueType fonts for better quality
                $font_size = 9;
                $angle = 0;
                $line_height = $font_size * 1.2;
                $total_height = count($lines) * $line_height;

                // Calculate the starting Y position to vertically center the text block
                $y_pos = ($height / 2) - ($total_height / 2) + $font_size;

                // Draw each line of text
                foreach ($lines as $line) {
                    // Set the X position to start drawing text (left-aligned with some padding)
                    $x_pos = 5;

                    // Render the text on the image
                    imagettftext($image, $font_size, $angle, (int)$x_pos, (int)$y_pos, $text_color, $font_path, $line);

                    // Move the Y position for the next line
                    $y_pos += $line_height;
                }
            }
        }

        // Output the image as PNG to the output buffer
        ob_clean(); // Avoid any previous output
        imagepng($image);
        // Get the image data from the output buffer
        $imgData = ob_get_clean();
        // Free up memory
        imagedestroy($image);

        // Return the Base64-encoded image data
        return 'data:image/png;base64,' . base64_encode($imgData);
    }
}
