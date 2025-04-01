<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

/**
 * vCard implementation
 * @api
 */
#[\AllowDynamicProperties]
class vCard
{
    protected $properties = array();

    protected $name = 'no_name';

    public function clear()
    {
        $this->properties = array();
    }

    public function loadContact($contactid, $module='Contacts')
    {
        global $app_list_strings;

        require_once($GLOBALS['beanFiles'][$GLOBALS['beanList'][$module]]);
        $contact = new $GLOBALS['beanList'][$module]();
        $contact->retrieve($contactid);
        // cn: bug 8504 - CF/LB break Outlook's vCard import
        $bad = array("\n", "\r");
        $good = array("=0A", "=0D");
        $encoding = '';
        if (strpos((string) $contact->primary_address_street, "\n") || strpos((string) $contact->primary_address_street, "\r")) {
            $contact->primary_address_street = str_replace($bad, $good, (string) $contact->primary_address_street);
            $encoding = 'QUOTED-PRINTABLE';
        }

        $this->setName(from_html($contact->first_name), from_html($contact->last_name), $app_list_strings['salutation_dom'][from_html($contact->salutation)]);
        if (isset($contact->birthdate)) {
            $this->setBirthDate(from_html($contact->birthdate));
        }
        $this->setPhoneNumber(from_html($contact->phone_fax), 'FAX');
        $this->setPhoneNumber(from_html($contact->phone_home), 'HOME');
        $this->setPhoneNumber(from_html($contact->phone_mobile), 'CELL');
        $this->setPhoneNumber(from_html($contact->phone_work), 'WORK');
        $this->setEmail(from_html($contact->email1));
        $this->setAddress(from_html($contact->primary_address_street), from_html($contact->primary_address_city), from_html($contact->primary_address_state), from_html($contact->primary_address_postalcode), from_html($contact->primary_address_country), 'WORK', $encoding);
        if (isset($contact->account_name)) {
            $this->setORG(from_html($contact->account_name), from_html($contact->department));
        } else {
            $this->setORG('', from_html($contact->department));
        }
        $this->setTitle($contact->title);
    }

    public function setTitle($title)
    {
        $this->setProperty("TITLE", $title);
    }
    public function setORG($org, $dep)
    {
        $this->setProperty("ORG", "$org;$dep");
    }
    public function setAddress($address, $city, $state, $postal, $country, $type, $encoding='')
    {
        if (!empty($encoding)) {
            $encoding = ";ENCODING={$encoding}";
        }
        $this->setProperty("ADR;$type$encoding", ";;$address;$city;$state;$postal;$country");
    }

    public function setName($first_name, $last_name, $prefix)
    {
        $this->name = strtr($first_name.'_'.$last_name, ' ', '_');
        $this->setProperty('N', $last_name.';'.$first_name.';;'.$prefix);
        $this->setProperty('FN', "$prefix $first_name $last_name");
    }

    public function setEmail($address)
    {
        $this->setProperty('EMAIL;INTERNET', $address);
    }

    public function setPhoneNumber($number, $type)
    {
        if ($type != 'FAX') {
            $this->setProperty("TEL;$type", $number);
        } else {
            $this->setProperty("TEL;WORK;$type", $number);
        }
    }
    public function setBirthDate($date)
    {
        $this->setProperty('BDAY', $date);
    }
    public function getProperty($name)
    {
        if (isset($this->properties[$name])) {
            return $this->properties[$name];
        }
        return null;
    }

    public function setProperty($name, $value)
    {
        $this->properties[$name] = $value;
    }

    public function toString()
    {
        global $locale;
        $temp = "BEGIN:VCARD\n";
        foreach ($this->properties as $key=>$value) {
            if (!empty($value)) {
                $temp .= $key. ';CHARSET='.strtolower($locale->getExportCharset()).':'.$value."\n";
            } else {
                $temp .= $key. ':'.$value."\n";
            }
        }
        $temp.= "END:VCARD\n";


        return $temp;
    }

    public function saveVCard()
    {
        global $locale;
        $content = $this->toString();
        if (!defined('SUITE_PHPUNIT_RUNNER')) {
            header("Content-Disposition: attachment; filename={$this->name}.vcf");
            header("Content-Type: text/x-vcard; charset=".$locale->getExportCharset());
            header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
            header("Last-Modified: " . TimeDate::httpTime());
            header("Cache-Control: max-age=0");
            header("Pragma: public");
            //bug45856 IIS Doesn't like this to be set and it causes the vCard to not get saved
            $serverSoftvare = isset($_SERVER['SERVER_SOFTWARE']) ? $_SERVER['SERVER_SOFTWARE'] : null;
            if (preg_match('/iis/i', (string) $serverSoftvare) === 0) {
                header("Content-Length: ".strlen((string) $content));
            }
        }

        print $locale->translateCharset($content, 'UTF-8', $locale->getExportCharset());
    }

    public function importVCard($filename, $module = 'Contacts')
    {
        global $current_user;
        $lines = file($filename);
        $start = false;

        $bean = BeanFactory::getBean($module);
        $bean->assigned_user_id = $current_user->id;
        $email_suffix = 1;
        $linesCount = count($lines);

        for ($index = 0; $index < $linesCount; $index++) {
            $line = $lines[$index];

            // check the encoding and change it if needed
            $locale = new Localization();
            $encoding = false;
            //detect charset
            if (preg_match("/CHARSET=([A-Z]+([A-Z0-9]-?)*):/", $line, $matches)) {
                //found charset hint in vcard
                $encoding = $matches[1];
            } else {
                //use locale to detect charset automatically
                $encoding = $locale->detectCharset($line);
            }
            if ($encoding != $GLOBALS['sugar_config']['default_charset']) {
                $line = $locale->translateCharset($line, $encoding);
            }

            $line = trim($line);
            if ($start) {
                //VCARD is done
                if (substr_count(strtoupper($line), 'END:VCARD')) {
                    if (!isset($bean->last_name) && !empty($fullname)) {
                        $bean->last_name = $fullname;
                    }
                    break;
                }

                $keys = [];
                $key = '';
                $values = [];
                $keyvalue = explode(':', $line);
                if (count($keyvalue) == 2) {
                    $value = $keyvalue[1];
                    $linesCount = count($lines);
                    for ($newindex = $index + 1; $newindex < $linesCount, substr_count($lines[$newindex], ':') == 0; $newindex++) {
                        $value .= $lines[$newindex];
                        $index = $newindex;
                    }
                    $values = explode(';', $value);
                    $key = strtoupper($keyvalue[0] ?? '');
                    $key = strtr($key, '=', '');
                    $key = strtr($key, ',', ';');
                    $keys = explode(';', $key);

                    if (($keys[0] ?? '') == 'TEL') {
                        if (substr_count($key, 'WORK') > 0) {
                            if (substr_count($key, 'FAX') > 0) {
                                if (!isset($bean->phone_fax)) {
                                    $bean->phone_fax = $value;
                                }
                            } else {
                                if (!isset($bean->phone_work)) {
                                    $bean->phone_work = $value;
                                }
                            }
                        }

                        if (substr_count($key, 'HOME') > 0) {
                            if (substr_count($key, 'FAX') > 0) {
                                if (!isset($bean->phone_fax)) {
                                    $bean->phone_fax = $value;
                                }
                            } else {
                                if (!isset($bean->phone_home)) {
                                    $bean->phone_home = $value;
                                }
                            }
                        }
                        if (substr_count($key, 'CELL') > 0) {
                            if (!isset($bean->phone_mobile)) {
                                $bean->phone_mobile = $value;
                            }
                        }
                        if (substr_count($key, 'FAX') > 0) {
                            if (!isset($bean->phone_fax)) {
                                $bean->phone_fax = $value;
                            }
                        }
                    }

                    if (($keys[0] ?? '') == 'N') {
                        if (count($values) > 0) {
                            $bean->last_name = $values[0];
                        }
                        if (count($values) > 1) {
                            $bean->first_name = $values[1];
                        }
                        if (count($values) > 2) {
                            $bean->salutation = $values[2];
                        }
                    }

                    if (($keys[0] ?? '') == 'FN') {
                        $fullname = $value;
                    }
                }

                if (($keys[0] ?? '') == 'ADR') {
                    if (substr_count((string) $key, 'WORK') > 0 && (substr_count((string) $key, 'POSTAL') > 0|| substr_count((string) $key, 'PARCEL') == 0)) {
                        if (!isset($bean->primary_address_street) && (is_countable($values) ? count($values) : 0) > 2) {
                            $textBreaks = array("\n", "\r");
                            $vcardBreaks = array("=0A", "=0D");
                            $bean->primary_address_street = str_replace($vcardBreaks, $textBreaks, $values[2]);
                        }
                        if (!isset($bean->primary_address_city) && (is_countable($values) ? count($values) : 0) > 3) {
                            $bean->primary_address_city = $values[3];
                        }
                        if (!isset($bean->primary_address_state) && (is_countable($values) ? count($values) : 0) > 4) {
                            $bean->primary_address_state = $values[4];
                        }
                        if (!isset($bean->primary_address_postalcode) && (is_countable($values) ? count($values) : 0) > 5) {
                            $bean->primary_address_postalcode = $values[5];
                        }
                        if (!isset($bean->primary_address_country) && (is_countable($values) ? count($values) : 0) > 6) {
                            $bean->primary_address_country = $values[6];
                        }
                    }
                }

                if (($keys[0] ?? '') == 'TITLE') {
                    $bean->title = $value;
                }
                if (($keys[0] ?? '') == 'EMAIL') {
                    $field = 'email' . $email_suffix;
                    if (!isset($bean->$field)) {
                        $bean->$field = $value;
                    }
                    if ($email_suffix == 1) {
                        $_REQUEST['email1'] = $value;
                    }
                    $email_suffix++;
                }

                if (($keys[0] ?? '') == 'ORG') {
                    $GLOBALS['log']->debug('I found a company name');
                    if (!empty($value)) {
                        $GLOBALS['log']->debug('I found a company name (fer real)');
                        if (is_a($bean, "Contact") || is_a($bean, "Lead")) {
                            $GLOBALS['log']->debug('And Im dealing with a person!');
                            $accountBean = BeanFactory::getBean('Accounts');
                            // It's a contact, we better try and match up an account
                            $full_company_name = trim($values[0]);
                            // Do we have a full company name match?
                            $result = $accountBean->retrieve_by_string_fields(array('name' => $full_company_name, 'deleted' => 0));
                            if (! isset($result->id)) {
                                // Try to trim the full company name down, see if we get some other matches
                                $vCardTrimStrings = array('/ltd\.*/i'=>'',
                                                            '/llc\.*/i'=>'',
                                                            '/gmbh\.*/i'=>'',
                                                            '/inc\.*/i'=>'',
                                                            '/\.com/i'=>'',
                                                    );
                                // Allow users to override the trimming strings
                                if (file_exists('custom/include/vCardTrimStrings.php')) {
                                    require_once('custom/include/vCardTrimStrings.php');
                                }
                                $short_company_name = trim(preg_replace(array_keys($vCardTrimStrings), $vCardTrimStrings, $full_company_name), " ,.");

                                $GLOBALS['log']->debug('Trying an extended search for: ' . $short_company_name);
                                $result = $accountBean->retrieve_by_string_fields(array('name' => $short_company_name, 'deleted' => 0));
                            }

                            if (is_a($bean, "Lead") || ! isset($result->id)) {
                                // We could not find a parent account, or this is a lead so only copy the name, no linking
                                $GLOBALS['log']->debug("Did not find a matching company ($full_company_name)");
                                $bean->account_id = '';
                                $bean->account_name = $full_company_name;
                            } else {
                                $GLOBALS['log']->debug("Found a matching company: " . $result->name);
                                $bean->account_id = $result->id;
                                $bean->account_name = $result->name;
                            }
                            $bean->department = $values[1];
                        } else {
                            $bean->department = $value;
                        }
                    }
                }
            }

            //FOUND THE BEGINING OF THE VCARD
            if (!$start && substr_count(strtoupper($line), 'BEGIN:VCARD')) {
                $start = true;
            }
        }

        foreach ($bean->get_import_required_fields() as $key => $value) {
            if (empty($bean->$key)) {
                $GLOBALS['log']->error("Cannot import vCard, required field is not set: $key");
                return;
            }
        }

        if (is_a($bean, "Contact") && empty($bean->account_id) && !empty($bean->account_name)) {
            $GLOBALS['log']->debug("Look ma! I'm creating a new account: " . $bean->account_name);
            // We need to create a new account
            $accountBean = BeanFactory::getBean('Accounts');
            // Populate the newly created account with all of the contact information
            foreach ($bean->field_defs as $field_name => $field_def) {
                if (!empty($bean->$field_name)) {
                    $accountBean->$field_name = $bean->$field_name;
                }
            }
            $accountBean->name = $bean->account_name;
            $accountBean->save();
            $bean->account_id = $accountBean->id;
        }

        $beanId = $bean->save();
        return $beanId;
    }
}
