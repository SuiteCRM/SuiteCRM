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

require_once 'include/utils.php';

class WebFormMailer
{
    public const TEMP_FILE_NAME_PREFIX = 'stic_dmdata_';
    public $subject = '';
    public $from = '';
    public $fromName = '';
    public $body = '';
    private $userIdDest = array(); // Contains user IDs to retrieve the email address and send the email
    private $mailsDest = array(); // Contains email addresses to send
    protected $current_language = null;
    protected $deferredData = null; // It contains the necessary data to generate deferred mails
    protected $defaultModule = 'stic_Web_Forms';
    protected $saved_attachment = array();

    public function __construct()
    {
        global $sugar_config;
        if (!empty($_SESSION['authenticated_user_language'])) {
            $this->current_language = $_SESSION['authenticated_user_language'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  language selected as [{$this->current_language}] from the session.");
        } else {
            $this->current_language = $sugar_config['default_language'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  language selected as [{$this->current_language}] from the configuration.");
        }
    }

    /**
     * Generate a link to the detail view of a record
     */
    public static function createLinkToDetailView($module, $id)
    {
        global $sugar_config;
        return rtrim($sugar_config['site_url'], '/') . "/index.php?module={$module}&action=DetailView&record={$id}";
    }

    /**
     * Add an element to an array (used to add ids or mails)
     **/
    protected function addToCollection(&$col, $val)
    {
        if (!empty($val)) {
            if (is_array($val)) {
                $col = array_merge($col, $val);
            } else {
                $col[] = $val;
            }
            $col = array_unique($col);
        }
        return $col;
    }

    /**
     * Add an identifier to the recipient array. It can also receive an array as a parameter
     */
    public function addUserIdDest($ids)
    {
        $this->addToCollection($this->userIdDest, $ids);
    }

    /**
     * Add an identifier to the recipient array. It can also receive an array as a parameter
     */
    public function addMailsDest($mails)
    {
        $this->addToCollection($this->mailsDest, $mails);
    }

    /**
     * Empty the recipient list
     */
    public function resetDest()
    {
        unset($this->userIdDest);
        unset($this->mailsDest);
        $this->userIdDest = array();
        $this->mailsDest = array();
    }

    /**
     * Send notice of changes to those involved in the case.
     */
    public function send()
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Processing mailing...");

        // Prepare mail
        require_once 'include/SugarPHPMailer.php';
        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();
        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();

        // Set From and FromName
        if (!$this->from) {
            $this->from = $defaults['email'];
        }
        $mail->From = $this->from;

        if (!$this->fromName) {
            $this->fromName = $defaults['name'];
        }
        $mail->FromName = $this->fromName;

        // Add recipients
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding email addresses...");
        if (!empty($this->mailsDest)) {
            if (is_array($this->mailsDest)) {
                foreach ($this->mailsDest as $address) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding address [{$address}]");
                    $mail->AddAddress($address);
                }
            } else {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding address [{$this->mailsDest}]");
                $mail->AddAddress($this->mailsDest);
            }
        }

        // Add users
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding users to the email...");
        if (!empty($this->userIdDest)) {
            $usersBean = BeanFactory::getBean('Users');
            $whereClause = ' users.id in (\'' . implode('\',\'', $this->userIdDest) . '\')';
            $users = $usersBean->get_full_list('', $whereClause);
            if ($users == null) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The data of those involved could not be retrieved [" . implode(",", $this->userIdDest) . "]. Clausula where: [" . $whereClause . "]");
            } else {
                foreach ($users as $user) {
                    if (!$user->emailAddress || !($address = $user->emailAddress->getPrimaryAddress($user))) {
                        $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The user with ID [" . $user->id . "] has no email address.");
                    } else {
                        $mail->AddAddress(trim($address));
                    }
                }
            }
        }

        // Set the subject
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Indicating subject...");
        $mail->Subject = $this->subject;
        $formatedBody = $this->applyInlineStyles($this->body);
        $completeHTML = "<html lang=\"{$this->current_language}\">
                            <head>
                                <title>{$this->subject}</title>
                            </head>
                            <body style=\"font-family: Arial\">
                            {$formatedBody}
                            </body>
                        </html>";
        $mail->Body = from_html($completeHTML);
        $mail->isHtml(true);
        $mail->prepForOutbound();

        ///////////////////////////////////////////////////////////////////////
        ////    ATTACHMENTS
        ////    20210525 - STIC - Code copied from modules/Emails/Email.php - Line 3039 + Delete the attached file of the array of files to attach once it has been managed.
        $indice = 0;
        if (!empty($saved_attachment) && !empty($this->$saved_attachment)) {
            foreach ($this->$saved_attachment as $note) {
                $mime_type = 'text/plain';
                if ($note->object_name == 'Note') {
                    if (!empty($note->file->temp_file_location) && is_file($note->file->temp_file_location)) {
                        // brandy-new file upload/attachment
                        $file_location = "file/" . $note->file->temp_file_location;
                        $filename = $note->file->original_file_name;
                        $mime_type = $note->file->mime_type;
                    } else {
                        // attachment coming from template/forward
                        $file_location = "upload/{$note->id}";
                        // cn: bug 9723 - documents from EmailTemplates sent with Doc Name, not file name.
                        $filename = !empty($note->filename) ? $note->filename : $note->name;
                        $mime_type = $note->file_mime_type;
                    }
                } elseif ($note->object_name == 'DocumentRevision') {
                    // from Documents
                    $filePathName = $note->id;
                    // cn: bug 9723 - Emails with documents send GUID instead of Doc name
                    $filename = $note->getDocumentRevisionNameForDisplay();
                    $file_location = "upload/$note->id";
                    $mime_type = $note->file_mime_type;
                }

                // strip out the "Email attachment label if exists
                $filename = str_replace($mod_strings['LBL_EMAIL_ATTACHMENT'] . ': ', '', $filename);
                $file_ext = pathinfo($filename, PATHINFO_EXTENSION);
                //is attachment in our list of bad files extensions?  If so, append .txt to file location
                //check to see if this is a file with extension located in "badext"
                foreach ($sugar_config['upload_badext'] as $badExt) {
                    if (strtolower($file_ext) == strtolower($badExt)) {
                        //if found, then append with .txt to filename and break out of lookup
                        //this will make sure that the file goes out with right extension, but is stored
                        //as a text in db.
                        $file_location = $file_location . ".txt";
                        break; // no need to look for more
                    }
                }
                global $locale;
                $OBCharset = $locale->getPrecedentPreference('default_email_charset');
                $mail->AddAttachment(
                    $file_location,
                    $locale->translateCharsetMIME(trim($filename), 'UTF-8', $OBCharset),
                    'base64',
                    $mime_type
                );

                // embedded Images
                if ($note->embed_flag == true) {
                    $cid = $filename;
                    $mail->AddEmbeddedImage($file_location, $cid, $filename, 'base64', $mime_type);
                }

                // Delete the attached file of the array of files to attach
                unset($this->$saved_attachment[$indice]);
                $indice++;
            }
        } else {
            LoggerManager::getLogger()->warn('Attachements not found');
        }
        ////    END ATTACHMENTS
        ///////////////////////////////////////////////////////////////////////

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Sending mail...");
        if (!$mail->Send()) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There was an error sending the mail.");
            return false;
        }
        return true;
    }

    /**
     * Apply "inline" styles so that they are visible by most mail clients.
     */
    protected function applyInlineStyles($html)
    {
        $styles = array(
            "th" => "text-align: left; font-weight: normal; width: 30%; padding: 3px; border: 1px solid",
            "td" => "padding: 3px; font-weight: bold; width: 70%; border: 1px solid",
            "table" => "width: 90%; margin: 5px 0 5px 0; font-family: Arial; border: 1px solid",
        );

        foreach ($styles as $element => $style) {
            $html = str_replace("<{$element}", "<{$element} style=\"{$style}\"", $html);
        }

        return $html;
    }

    /**
     *  Translate a string. If module is not indicated use the default module
     */
    public function translate($label, $mod = '', $selectedValue = '')
    {
        global $current_language;
        $current_language = $this->current_language;
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Language used [{$current_language}], module [" . (empty($mod) ? $this->defaultModule : $mod) . "]");
        return translate($label, empty($mod) ? $this->defaultModule : $mod, $selectedValue);
    }

    /**
     * Returns an array with the campaign data to be able to include them in the mail.
     * @param $link Indicates whether the value must be a link or not (default yes)
     * @return array('label' => String, 'value' => String)
     */
    public function getCampaingData($id, $link = true)
    {
        $ret = array('label' => '', 'value' => '');
        $campaign = BeanFactory::getBean('Campaigns');
        $campaign = $campaign->retrieve($id);

        if ($campaign != null) {
            $ret['label'] = $this->translate('LBL_CAMPAIGN', $campaign->module_name);
            if ($link) {
                $ret['value'] = '<a href="' . self::createLinkToDetailView($campaign->module_name, $campaign->id) . '">' . $campaign->name . '</a>';
            } else {
                $ret['value'] = $campaign->name;
            }
        }
        return $ret;
    }

    /**
     * Parse an email template retrieved by ID
     * Save the result in $this->body, $this->body_text, $this->subject
     * @param String $templateId Template Identifier
     * @param Array $replacementObjects Array of objects to be parsed
     * @param Optional String $lang Language with which to parse the template
     * @return String Mail body in html
     */
    public function parseEmailTemplateById($templateId, $replacementObjects, $lang = null)
    {
        if (empty($templateId)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No ID received.");
            return false;
        }

        require_once 'modules/EmailTemplates/EmailTemplate.php';
        $template = new EmailTemplate();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving email template with ID [{$templateId}]...");

        if (!$template->retrieve($templateId)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Template with ID  [{$templateId}]  not found.");
            return false;
        }
        return $this->parseEmailTemplate($template, $replacementObjects, $lang);
    }

    /**
     * Parse an email template retrieved by name
     * Save the result in $this->body, $this->body_text, $this->subject
     * @param String $templateId Template name
     * @param Array $replacementObjects Array of objects to be parsed
     * @param Optional String $lang Language with which to parse the template
     * @return String Mail body in html
     */
    public function parseEmailTemplateByName($templateName, $replacementObjects, $lang = null, $type = 'email')
    {
        if (empty($templateName)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No name has been received.");
            return false;
        }

        require_once 'modules/EmailTemplates/EmailTemplate.php';
        $template = new EmailTemplate();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving email template [{$templateId}] type [{$type}]...");

        if (!$template->retrieve_by_string_fields(array('name' => $templateName, 'type' => $type))) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Template not found with name [{$templateName}]");
            return false;
        }
        return $this->parseEmailTemplate($template, $replacementObjects, $lang);
    }

    /**
     * Parse an email template with past objects
     * Save the result in $this->body, $this->body_text, $this->subject
     * @param $template EmailTemplate object
     * @param $replacementObjects Array of objects to be parsed
     * @return String Mail body in html
     */
    protected function parseEmailTemplate($template, $replacementObjects, $lang)
    {
        global $current_language, $app_list_strings, $app_strings;

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Parsing template ...");

        if (empty($lang)) {
            $lang = $_REQUEST['language'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Indicating language [{$lang}] from form.");
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Indicating language [{$lang}] from parameter.");
        }

        if (empty($lang)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Language not found [{$lang}], default value is used [{$current_language}].");
            $lang = $current_language;
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading language files...");
        $prev_lang = $current_language;
        $current_language = $lang;
        $app_strings = return_application_language($current_language);
        $app_list_strings = return_app_list_strings_language($current_language);

        $parseArr = array("subject0" => $template->subject, "text0" => $template->body, "html0" => $template->body_html);
        $replacementObjectsLength = (empty($replacementObjects) || !is_array($replacementObjects) ? 0 : count($replacementObjects));

        $j = 0;
        for ($i = 0; $i < $replacementObjectsLength; $i++) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Parsing object [{$i}] [{$replacementObjects[$i]->module_dir}] ... ");
            $macro_nv = array();
            $obj = $this->prepareBean2EmailTemplate($replacementObjects[$i]);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Date [" . ($obj->registration_date ?? null) . "] ... ");
            $parseArr = $template->parse_email_template($parseArr, $obj->module_dir, $obj, $macro_nv);
            $j = $i + 1;
            $parseArr["text{$j}"] = $parseArr["text{$i}"];
            $parseArr["html{$j}"] = $parseArr["html{$i}"];
            $parseArr["subject{$j}"] = $parseArr["subject{$i}"];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Result [{$i}] -> " . $parseArr["html{$j}"]);
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Recovering original language files ...");
        $current_language = $prev_lang;
        $app_strings = return_application_language($current_language);
        $app_list_strings = return_app_list_strings_language($current_language);

        $this->body = $parseArr["html{$j}"];
        $this->body_text = $parseArr["text{$j}"];
        $this->subject = $parseArr["subject{$j}"];

        // Get the attached files through Notes
        $notesBean = BeanFactory::getBean('Notes');
        $this->saved_attachment = $notesBean->get_full_list(
            'name',
            "notes.parent_id = '$template->id'"
        );

        return $this->body;
    }

    /**
     * Format the last bean so that it is displayed correctly in the mail template
     * @param Bean $bean
     * @return Bean $formattedBean
     */
    protected function prepareBean2EmailTemplate($bean)
    {
        global $timedate, $sugar_config;
        $currencyFormatParams = array('currency_symbol' => true);
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Cloning object [{$bean->module_dir}] ...");
        $ret = clone $bean;
        $dateFormat = (empty($sugar_config['default_date_format']) ? 'd/m/Y' : $sugar_config['default_date_format']);
        $dateTime = (empty($sugar_config['default_time_format']) ? 'H:i:s' : $sugar_config['default_time_format']);
        foreach ($ret->field_defs as $field => $fieldDef) {
            $dateTimeFormat = "";
            switch ($fieldDef['type']) {
                case 'currency':
                    $ret->$field = format_number($ret->$field, null, null, $currencyFormatParams);
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Field [{$field}] formatted [{$bean->$field}] => [{$ret->$field}].");
                    break;
                case 'datetimecombo':
                case 'datetime':
                    $dateTimeFormat = " {$dateTime}";
                    break;
                case 'date':
                    $dateTimeFormat = "{$dateFormat}{$dateTimeFormat}";
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Format [{$fieldDef['type']}] = [{$dateTimeFormat}].");
                    $td = !empty($ret->$field) ? $timedate->fromDbType($ret->$field, $fieldDef['type']) : null;
                    if (!empty($td)) {
                        $ret->$field = $td->format($dateTimeFormat);
                    }
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Field [{$field}] formatted [" . ($bean->$field ?? null) ." ] => [" . ($ret->$field ?? null) . "]");
                    break;
            }
        }
        return $ret;
    }

    /**
     * Save the data necessary to build an email to send after going through another web request.
     * e.g. in a payment with POS (TPV) where you must wait for the response from the external website
     * @param string $id Request Identifier
     * @param string $webFormClass Name of the form class (indicates the folder in which the specific class file will be found)
     * @param string $class Name of the class that will manage the data delivery
     * @param array  $data Array with the data to be saved (it will be converted to JSON)
     * @return boolean True if it could be saved, false otherwise
     */
    public function saveDataToDeferredMail($id, $webFormClass, $class, $data)
    {
        global $php_errormsg;

        if (empty($id)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  ID not received, information cannot be saved.");
            return false;
        }

        if (empty($class)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Class not received to retrieve data, information cannot be saved.");
            return false;
        }

        $tmpdir = ini_get('upload_tmp_dir') ? ini_get('upload_tmp_dir') : sys_get_temp_dir();
        $fileName = "{$tmpdir}/" . WebFormMailer::TEMP_FILE_NAME_PREFIX . "{$id}";
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving information in {$fileName} ...");

        // Generate the JSON data array
        $jsonData = json_encode(
            array(
                "id" => $id,
                "webFormClass" => $webFormClass,
                "class" => $class,
                "data" => self::processDataToSave($data),
            )
        );
        ini_set('track_errors', 1);

        $res = file_put_contents($fileName, $jsonData);
        if ($res === false) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Error writing file. -> {$php_errormsg}");
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Data saved successfully.");
        }

        ini_set('track_errors', 0);
        return $res;
    }

    /**
     * Prepare the data to save it in the text file
     * @param array $data
     * @return array
     */
    protected static function processDataToSave($data)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Processing data to save in the file ...");
        if (!empty($data) && is_array($data)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There is data to process.");
            foreach ($data as $key => $value) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Processing field [{$key}] ...");

                // If the object is from a daughter class of sugarbean, call the array method and mark it as an instance of sugarbean
                if (is_object($value) && is_subclass_of($value, "SugarBean")) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  It is an object of a class [{$value->module_name}] daughter of SugarBean, keeping item...");
                    $temp = $value->toArray(false, true);
                    $temp['module_name'] = $value->module_name;
                    $temp['is_sugarbean_instance'] = '1';
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Object saved.");
                    // Overwrite the data in the array
                    $data[$key] = $temp;
                }
            }
        }
        return $data;
    }

    /**
     * Treat the recovered data to turn it into objects
     * @param array $data
     */
    protected static function processDataFromFile($data)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Processing read data from the file ...");
        if (!empty($data) && is_array($data)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There is data to process.");
            foreach ($data as $key => $value) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Processing field [{$key}] ...");
                // If the object is from a daughter class of sugarbean, create a new class object and populate the fields using the fromArray method
                if (is_array($value) && !empty($value['is_sugarbean_instance'])) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  It is an object of a class [{$value['module_name']}] daughter of SugarBean, loading item ...");
                    $temp = BeanFactory::getBean($value['module_name']);
                    $temp->fromArray($value);
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Object loaded.");
                    // Overwrite the data in the array
                    $data[$key] = $temp;
                }
            }
        }
        return $data;
    }

    /**
     * Recover saved data for deferred mail delivery
     * @param string $id Request Identifier
     * @return object Returns an instance of the class responsible for managing the request with the data loaded in the deferredData property
     */
    public static function readDataToDeferredMail($id, $deleteTmpFile = true)
    {
        if (empty($id)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  ID not received, information cannot be saved.");
            return false;
        }

        $tmpdir = ini_get('upload_tmp_dir') ? ini_get('upload_tmp_dir') : sys_get_temp_dir();
        $fileName = "{$tmpdir}/" . WebFormMailer::TEMP_FILE_NAME_PREFIX . "{$id}";
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving information from {$fileName}...");

        ini_set('track_errors', 1);
        $ret = false;
        $data = file_exists($fileName) ? file_get_contents($fileName) : false;
        if ($data === false) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Error reading file.");
        } else {
            // We decode the data and save it in an associative array
            $decodedData = json_decode($data, true);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  File read correctly.");
            if ($decodedData['id'] != $id) {
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The identifiers do not match, the data is not valid.");
            } else {
                // Build the name of the driver file
                $classFile = __DIR__ . "/../../{$decodedData['webFormClass']}/{$decodedData['class']}.php";
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading specific file from the mailer {$classFile} ...");
                require_once $classFile;
                $className = $decodedData['class'];
                $ret = new $className();
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  File uploaded.");
                $ret->setDeferredData(self::processDataFromFile($decodedData['data']));
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Data loaded correctly.");
            }
        }

        if ($deleteTmpFile && file_exists($fileName)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Deleting File [{$fileName}]...");
            unlink($fileName);
        }
        ini_set('track_errors', 0);
        return $ret;
    }

    /**
     * Method that all daughter classes must define.
     * It is responsible for sending deferred notice emails.
     * @param string $response It contains an answer code
     * @param string $mailType It contains a type of mail in case more than one can be sent
     * @return boolean
     */
    public function sendDeferredMails($response = null, $mailType = null)
    {
        $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Function call not defined.");
    }

    /**
     * Report the property DeferredData
     * @param array $deferredData
     */
    protected function setDeferredData($deferredData)
    {
        $this->deferredData = $deferredData;
    }

    /**
     * Send email to form assigned user because of missing required fields
     *
     * @param Array $formData received form data
     * @param String $assignedUserId. If user not exists or can't find email address, the email is sent to all admin users
     * @param String $msg Error message info
     * @return void
     */
    public static function sendErrorNotification($formData, $formParams, $assignedUserId, $msg)
    {
        $usersBean = BeanFactory::getBean('Users', $assignedUserId);

        $emailToNotify = array();
        if (isset($usersBean->email1)) {
            $emailToNotify[] = $usersBean->email1;
        }

        // If there is no assigned user email, select all admin emails
        if (empty($emailToNotify)) {
            $users = BeanFactory::getBean('Users');
            $adminList = $users->get_full_list('', "users.is_admin = 1 AND users.status = 'Active'");
            $emailToNotify = array();
            foreach ($adminList as $user) {
                $emailToNotify[] = $user->emailAddress->getPrimaryAddress($user);
            }
        }

        require_once 'include/SugarPHPMailer.php';
        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();

        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();
        $mail->From = $defaults['email'];
        $mail->FromName = $defaults['name'];

        foreach ($emailToNotify as $key => $value) {
            if (isValidEmailAddress($value)) {
                $mail->AddCC($value);
            }
        }

        $mail->Subject = "[SinergiaCRM Web Form Error] {$msg['subject']}";

        $bodyContent = '';
        $bodyContent .= "<h2>{$msg['subject']}</h2>";
        $bodyContent .= "<p>{$msg['information']}</p>";

        $bodyContent .= "<h3>{$msg['received_data_title']}</h3>";
        foreach ($formData as $key => $value) {
            $bodyContent .= "<li>{$key}: {$value}</li>";
        }

        $bodyContent .= "<h3>{$msg['form_params_title']}</h3>";
        foreach ($formParams as $key => $value) {
            $bodyContent .= "<li>{$key}: {$value}</li>";
        }

        $completeHTML = "
        <html lang=\"{$GLOBALS['current_language']}\">
            <head>
                <title>{$mail->Subject}</title>
            </head>
            <body style=\"font-family: Arial\">
            {$bodyContent}
            </body>
        </html>";

        $mail->Body = $bodyContent;

        $mail->IsHTML(true);
        $mail->prepForOutbound();

        // Send the message
        if (!$mail->Send()) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error sending notification email.");
        }
    }
}
