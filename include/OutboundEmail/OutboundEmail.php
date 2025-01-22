<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Outbuound email management
 * @api
 */
#[\AllowDynamicProperties]
class OutboundEmail
{
    /**
     * Necessary
     */
    public $db;
    public $field_defs = array(
        'id',
        'name',
        'type',
        'user_id',
        'mail_sendtype',
        'mail_smtptype',
        'mail_smtpserver',
        'mail_smtpport',
        'mail_smtpuser',
        'mail_smtppass',
        'mail_smtpauth_req',
        'mail_smtpssl',
        'smtp_from_name',
        'smtp_from_addr',
    );

    /**
     * Columns
     */
    public $id;
    public $name;
    public $type; // user or system
    public $user_id; // owner
    public $mail_sendtype; // smtp
    public $mail_smtptype;
    public $mail_smtpserver;
    public $mail_smtpport = 25;
    public $mail_smtpuser;
    public $mail_smtppass;
    public $smtp_from_name;
    public $smtp_from_addr;
    public $mail_smtpauth_req; // bool
    public $mail_smtpssl; // bool
    public $mail_smtpdisplay; // calculated value, not in DB
    public $new_with_id = false;

    /**
     * Sole constructor
     */
    public function __construct()
    {
        $this->db = DBManagerFactory::getInstance();
    }




    /**
     * Retrieves the mailer for a user if they have overriden the username
     * and password for the default system account.
     *
     * @param String $user_id
     */
    public function getUsersMailerForSystemOverride($user_id)
    {
        $query = "SELECT id FROM outbound_email WHERE user_id = '{$user_id}' AND type = 'system-override' ORDER BY name";
        $rs = $this->db->query($query);
        $row = $this->db->fetchByAssoc($rs);
        if (!empty($row['id'])) {
            $oe = new OutboundEmail();
            $oe->retrieve($row['id']);

            return $oe;
        } else {
            return null;
        }
    }

    /**
     * Duplicate the system account for a user, setting new parameters specific to the user.
     *
     * @param string $user_id
     * @param string $user_name
     * @param string $user_pass
     */
    public function createUserSystemOverrideAccount($user_id, $user_name = "", $user_pass = "")
    {
        $ob = $this->getSystemMailerSettings();
        $ob->id = create_guid();
        $ob->new_with_id = true;
        $ob->user_id = $user_id;
        $ob->type = 'system-override';
        $ob->mail_smtpuser = $user_name;
        $ob->mail_smtppass = $user_pass;
        $ob->save();

        return $ob;
    }

    /**
     * Determines if a user needs to set their user name/password for their system
     * override account.
     *
     * @param unknown_type $user_id
     * @return unknown
     */
    public function doesUserOverrideAccountRequireCredentials($user_id)
    {
        $userCredentialsReq = false;
        $sys = new OutboundEmail();
        $ob = $sys->getSystemMailerSettings(); //Dirties '$this'

        //If auth for system account is disabled or user can use system outbound account return false.
        if ($ob->mail_smtpauth_req == 0 || $this->isAllowUserAccessToSystemDefaultOutbound() || $this->mail_sendtype == 'sendmail') {
            return $userCredentialsReq;
        }

        $userOverideAccount = $this->getUsersMailerForSystemOverride($user_id);
        if ($userOverideAccount == null || empty($userOverideAccount->mail_smtpuser) || empty($userOverideAccount->mail_smtpuser)) {
            $userCredentialsReq = true;
        }

        return $userCredentialsReq;
    }

    /**
     * Retrieves name value pairs for opts lists
     */
    public function getUserMailers($user)
    {
        global $app_strings;

        $q = "SELECT * FROM outbound_email WHERE user_id = '{$user->id}' AND type = 'user' ORDER BY name";
        $r = $this->db->query($q);

        $ret = array();

        $system = $this->getSystemMailerSettings();

        //Now add the system default or user override default to the response.
        if (!empty($system->id)) {
            if (isSmtp($system->mail_sendtype ?? '')) {
                $systemErrors = "";
                $userSystemOverride = $this->getUsersMailerForSystemOverride($user->id);

                //If the user is required to to provide a username and password but they have not done so yet,
                //create the account for them.
                $autoCreateUserSystemOverride = false;
                if ($this->doesUserOverrideAccountRequireCredentials($user->id)) {
                    $systemErrors = $app_strings['LBL_EMAIL_WARNING_MISSING_USER_CREDS'];
                    $autoCreateUserSystemOverride = true;
                }

                // Substitute in the users system override if its available.
                if ($userSystemOverride != null) {
                    $system = $userSystemOverride;
                } else {
                    if ($autoCreateUserSystemOverride) {
                        $system = $this->createUserSystemOverrideAccount($user->id, "", "");
                    }
                }
                // User overrides can be edited.
                $isEditable = !($system->type == 'system' || $system->type == 'system-override');

                if (!empty($system->mail_smtpserver)) {
                    $ret[] = array(
                        'id' => $system->id,
                        'name' => (string)$system->name,
                        'mail_smtpserver' => $system->mail_smtpdisplay,
                        'is_editable' => $isEditable,
                        'type' => $system->type,
                        'errors' => $systemErrors
                    );
                }
            } else {
                // use sendmail
                $ret[] = array(
                    'id' => $system->id,
                    'name' => "{$system->name} - sendmail",
                    'mail_smtpserver' => 'sendmail',
                    'is_editable' => false,
                    'type' => $system->type,
                    'errors' => ''
                );
            }
        }

        while ($a = $this->db->fetchByAssoc($r)) {
            $oe = array();
            if (isSmtp($a['mail_sendtype'] ?? '')) {
                continue;
            }
            $oe['id'] = $a['id'];
            $oe['name'] = $a['name'];
            $oe['type'] = $a['type'];
            $oe['is_editable'] = true;
            $oe['errors'] = '';
            if (!empty($a['mail_smtptype'])) {
                $oe['mail_smtpserver'] = $this->_getOutboundServerDisplay($a['mail_smtptype'], $a['mail_smtpserver']);
            } else {
                $oe['mail_smtpserver'] = $a['mail_smtpserver'];
            }

            $ret[] = $oe;
        }

        return $ret;
    }

    /**
     * Retrieves a cascading mailer set
     * @param object user
     * @param string mailer_id
     * @return object
     */
    public function getUserMailerSettings(&$user, $mailer_id = '', $ieId = '')
    {
        $mailer = '';

        if (!empty($mailer_id)) {
            $mailer = "AND id = '{$mailer_id}'";
        } elseif (!empty($ieId)) {
            $q = "SELECT stored_options FROM inbound_email WHERE id = '{$ieId}'";
            $r = $this->db->query($q);
            $a = $this->db->fetchByAssoc($r);

            if (!empty($a)) {
                $opts = sugar_unserialize(base64_decode($a['stored_options']));

                if (isset($opts['outbound_email'])) {
                    $mailer = "AND id = '{$opts['outbound_email']}'";
                }
            }
        }

        $q = "SELECT id FROM outbound_email WHERE user_id = '{$user->id}' {$mailer}";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (empty($a)) {
            $ret = $this->getSystemMailerSettings();
        } else {
            $ret = $this->retrieve($a['id']);
        }

        return $ret;
    }

    /**
     * Retrieve an array containing inbound emails ids for all inbound email accounts which have
     * their outbound account set to this object.
     *
     * @param SugarBean $user
     * @param string $outbound_id
     * @return array
     */
    public function getAssociatedInboundAccounts($user)
    {
        $query = "SELECT id,stored_options FROM inbound_email WHERE is_personal='1' AND deleted='0' AND created_by = '{$user->id}'";
        $rs = $this->db->query($query);

        $results = array();
        while ($row = $this->db->fetchByAssoc($rs)) {
            $opts = sugar_unserialize(base64_decode($row['stored_options']));
            if (isset($opts['outbound_email']) && $opts['outbound_email'] == $this->id) {
                $results[] = $row['id'];
            }
        }

        return $results;
    }

    /**
     * Retrieves a cascading mailer set
     * @param object user
     * @param string mailer_id
     * @return object
     */
    public function getInboundMailerSettings($user, $mailer_id = '', $ieId = '')
    {
        $mailer = '';

        if (!empty($mailer_id)) {
            $mailer = "id = '{$mailer_id}'";
        } elseif (!empty($ieId)) {
            $q = "SELECT stored_options FROM inbound_email WHERE id = '{$ieId}'";
            $r = $this->db->query($q);
            $a = $this->db->fetchByAssoc($r);

            if (!empty($a)) {
                $opts = sugar_unserialize(base64_decode($a['stored_options']));

                if (isset($opts['outbound_email'])) {
                    $mailer = "id = '{$opts['outbound_email']}'";
                } else {
                    $mailer = "id = '{$ieId}'";
                }
            } else {
                // its possible that its an system account
                $mailer = "id = '{$ieId}'";
            }
        }

        if (empty($mailer)) {
            $mailer = "type = 'system'";
        } // if

        $q = "SELECT id FROM outbound_email WHERE {$mailer}";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (empty($a)) {
            $ret = $this->getSystemMailerSettings();
        } else {
            $ret = $this->retrieve($a['id']);
        }

        return $ret;
    }

    /**
     *  Determine if the user is allowed to use the current system outbound connection.
     */
    public function isAllowUserAccessToSystemDefaultOutbound()
    {
        $allowAccess = false;

        // first check that a system default exists
        $q = "SELECT id FROM outbound_email WHERE type = 'system'";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);
        if (!empty($a)) {
            // next see if the admin preference for using the system outbound is set
            $admin = BeanFactory::newBean('Administration');
            $admin->retrieveSettings('', true);
            if (isset($admin->settings['notify_allow_default_outbound'])
                && $admin->settings['notify_allow_default_outbound'] == 2
            ) {
                $allowAccess = true;
            }
        }

        return $allowAccess;
    }

    /**
     * Retrieves the system's Outbound options
     */
    public function getSystemMailerSettings()
    {
        $q = "SELECT id FROM outbound_email WHERE type = 'system' AND deleted = 0";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (empty($a)) {
            $this->id = "";
            $this->name = 'system';
            $this->type = 'system';
            $this->user_id = '1';
            $this->mail_sendtype = 'SMTP';
            $this->mail_smtptype = 'other';
            $this->mail_smtpserver = '';
            $this->mail_smtpport = 25;
            $this->mail_smtpuser = '';
            $this->mail_smtppass = '';
            $this->mail_smtpauth_req = 1;
            $this->mail_smtpssl = 0;
            $this->mail_smtpdisplay = $this->_getOutboundServerDisplay($this->mail_smtptype, $this->mail_smtpserver);
            $this->save();
            $ret = $this;
        } else {
            $ret = $this->retrieve($a['id']);
        }

        return $ret;
    }

    /**
     * Populates this instance
     * @param string $id
     * @return object $this
     */
    public function retrieve($id)
    {
        require_once('include/utils/encryption_utils.php');
        $q = "SELECT * FROM outbound_email WHERE id = '{$id}'";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (!empty($a)) {
            foreach ($a as $k => $v) {
                if ($k == 'mail_smtppass' && !empty($v)) {
                    $this->$k = blowfishDecode(blowfishGetKey('OutBoundEmail'), $v);
                } else {
                    $this->$k = $v;
                } // else
            }
            if (!empty($a['mail_smtptype'])) {
                $this->mail_smtpdisplay = $this->_getOutboundServerDisplay($a['mail_smtptype'], $a['mail_smtpserver']);
            } else {
                $this->mail_smtpdisplay = $a['mail_smtpserver'];
            }
        }

        return $this;
    }

    public function populateFromPost()
    {
        foreach ($this->field_defs as $def) {
            if (isset($_POST[$def])) {
                $this->$def = $_POST[$def];
            } else {
                if ($def != 'mail_smtppass') {
                    $this->$def = "";
                }
            }
        }
    }

    /**
     * Generate values for saving into outbound_emails table
     * @param array $keys
     * @return array
     */
    protected function getValues(&$keys)
    {
        $values = array();
        $validKeys = array();

        foreach ($keys as $def) {
            if ($def == 'mail_smtppass' && !empty($this->$def)) {
                $this->$def = blowfishEncode(blowfishGetKey('OutBoundEmail'), $this->$def);
            } // if
            if ($def == 'mail_smtpauth_req' || $def == 'mail_smtpssl' || $def == 'mail_smtpport') {
                if (empty($this->$def)) {
                    $this->$def = 0;
                }
                $values[] = (int)$this->$def;
                $validKeys[] = $def;
            } else {
                if (isset($this->$def)) {
                    $values[] = $this->db->quoted($this->$def);
                    $validKeys[] = $def;
                }
            }
        }
        $keys = $validKeys;

        return $values;
    }

    /**
     * saves an instance
     */
    public function save()
    {
        $this->checkSavePermissions();

        require_once('include/utils/encryption_utils.php');
        if (empty($this->id)) {
            $this->id = create_guid();
            $this->new_with_id = true;
        }

        $cols = $this->field_defs;
        $values = $this->getValues($cols);

        if ($this->new_with_id) {
            $q = sprintf("INSERT INTO outbound_email (%s) VALUES (%s)", implode(",", $cols), implode(",", $values));
        } else {
            $updvalues = array();
            foreach ($values as $k => $val) {
                $updvalues[] = "{$cols[$k]} = $val";
            }
            $q = "UPDATE outbound_email SET " . implode(
                ', ',
                $updvalues
            ) . " WHERE id = " . $this->db->quoted($this->id);
        }

        $this->db->query($q, true);

        return $this;
    }

    /**
     * Saves system mailer.  Presumes all values are filled.
     */
    public function saveSystem()
    {
        $q = "SELECT id FROM outbound_email WHERE type = 'system' AND deleted = 0";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (empty($a)) {
            $a['id'] = ''; // trigger insert
        }

        $this->id = $a['id'];
        $this->name = 'system';
        $this->type = 'system';
        $this->user_id = '1';

        if (isset($_REQUEST['notify_fromname']) && $_REQUEST['notify_fromaddress']) {
            $this->smtp_from_name = $_REQUEST['notify_fromname'];
            $this->smtp_from_addr = $_REQUEST['notify_fromaddress'];
        }

        $this->save();

        $this->updateUserSystemOverrideAccounts();
    }

    /**
     * Update the user system override accounts with the system information if anything has changed.
     *
     */
    public function updateUserSystemOverrideAccounts()
    {
        require_once('include/utils/encryption_utils.php');
        $updateFields = array(
            'mail_smtptype',
            'mail_sendtype',
            'mail_smtpserver',
            'mail_smtpport',
            'mail_smtpauth_req',
            'mail_smtpssl'
        );

        //Update the username ans password for the override accounts if alloweed access.
        if ($this->isAllowUserAccessToSystemDefaultOutbound()) {
            $updateFields[] = 'mail_smtpuser';
            $updateFields[] = 'mail_smtppass';
        }
        $values = $this->getValues($updateFields);
        $updvalues = array();
        foreach ($values as $k => $val) {
            $updvalues[] = "{$updateFields[$k]} = $val";
        }
        $query = "UPDATE outbound_email set " . implode(', ', $updvalues) . " WHERE type='system-override' ";

        $this->db->query($query);
    }

    /**
     * Remove all of the user override accounts.
     *
     */
    public function removeUserOverrideAccounts()
    {
        $query = "DELETE FROM outbound_email WHERE type = 'system-override'";

        return $this->db->query($query);
    }

    /**
     * Deletes an instance
     */
    public function delete()
    {
        if (empty($this->id)) {
            return false;
        }

        $q = "DELETE FROM outbound_email WHERE id = " . $this->db->quoted($this->id);

        return $this->db->query($q);
    }

    private function _getOutboundServerDisplay(
        $smtptype,
        $smtpserver
    ) {
        global $app_strings;

        switch ($smtptype) {
            case "yahoomail":
                return $app_strings['LBL_SMTPTYPE_YAHOO'];
                break;
            case "gmail":
                return $app_strings['LBL_SMTPTYPE_GMAIL'];
                break;
            case "exchange":
                return $smtpserver . ' - ' . $app_strings['LBL_SMTPTYPE_EXCHANGE'];
                break;
            default:
                return $smtpserver;
                break;
        }
    }

    /**
     * Get mailer for current user by name
     * @param User $user
     * @param string $name
     * @return OutboundEmail|false
     */
    public function getMailerByName($user, $name)
    {
        if ($name == "system" && !$this->isAllowUserAccessToSystemDefaultOutbound()) {
            $oe = $this->getUsersMailerForSystemOverride($user->id);
            if (!empty($oe) && !empty($oe->id)) {
                return $oe;
            } else {
                return $this->getSystemMailerSettings();
            }
        }
        $res = $this->db->query("SELECT id FROM outbound_email WHERE user_id = '{$user->id}' AND name='" . $this->db->quote($name) . "'");
        $a = $this->db->fetchByAssoc($res);
        if (!isset($a['id'])) {
            return false;
        }

        return $this->retrieve($a['id']);
    }

    /**
     * @return void
     */
    protected function checkSavePermissions(): void
    {
        global $log;


        $original = null;

        if (!empty($this->id)) {
            $original = new OutboundEmail();
            $original->retrieve($this->id);
        }

        if (empty($original)) {
            $original = $this;
        }

        $type = $this->type ?? '';

        $authenticatedUser = get_authenticated_user();
        if ($authenticatedUser === null) {
            $log->security("OutboundEmail::checkSavePermissions - not logged in - skipping check");
            return;
        }

        if ($type === 'system' && !is_admin($authenticatedUser)) {
            $log->security("OutboundEmail::checkSavePermissions - trying to save a system outbound email with non-admin user");
            throw new RuntimeException('Access denied');
        }

        $oeUserId = $original->user_id ?? '';

        if (!empty($oeUserId) && $oeUserId !== $authenticatedUser->id && !is_admin($authenticatedUser)) {
            $log->security("OutboundEmail::checkSavePermissions - trying to save a outbound email for another user");
            throw new RuntimeException('Access denied');
        }
    }
}
